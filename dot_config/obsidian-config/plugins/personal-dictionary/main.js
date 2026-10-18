// Personal Dictionary — adds "Add to personal dictionary" to the editor's
// right-click menu for the word under the pointer (or the selection), whether
// or not Chromium flagged it.
//
// Words go into the "## Words" section of a vault note, the master list that
// ~/.local/bin/personal-dict syncs to every other spell checker. The plugin
// also loads that list into Obsidian's live spell checker on start and
// whenever the note changes, so edits apply without a restart.
//
// Hand-written CommonJS, no build step. Deployed by chezmoi from
// ~/.config/obsidian-config/plugins/personal-dictionary.

const { Plugin, Notice, TFile, MarkdownView } = require("obsidian");

const NOTE_PATH = "3-Resources/Spelling/Personal Dictionary.md";
const HEADING = "## Words";
// Letters, marks and digits, plus apostrophes and hyphens inside a word.
const WORD_CHAR = /[\p{L}\p{M}\p{N}'’-]/u;

function spellSession() {
  try {
    return require("electron").remote.getCurrentWebContents().session;
  } catch (e) {
    return null;
  }
}

function sortWords(words) {
  return [...words].sort((a, b) => {
    const x = a.toLowerCase(), y = b.toLowerCase();
    return x < y ? -1 : x > y ? 1 : a < b ? -1 : a > b ? 1 : 0;
  });
}

// Same layout as personal-dict's split_note: everything outside the Words
// section is kept as-is.
function splitNote(text) {
  const lines = text.split("\n");
  const start = lines.findIndex((l) => l.trim() === HEADING);
  if (start === -1) {
    const before = text.endsWith("\n") || !text ? text : text + "\n";
    return { before: before + "\n", words: new Set(), after: "" };
  }
  let end = lines.findIndex((l, i) => i > start && l.startsWith("## "));
  if (end === -1) end = lines.length;
  const words = new Set();
  for (const line of lines.slice(start + 1, end)) {
    const m = line.trim().match(/^- (\S+)$/);
    if (m) words.add(m[1]);
  }
  return {
    before: lines.slice(0, start).join("\n") + (start ? "\n" : ""),
    words,
    after: end < lines.length ? lines.slice(end).join("\n") : "",
  };
}

function joinNote({ before, words, after }) {
  const body = sortWords(words).map((w) => `- ${w}\n`).join("");
  return before + `${HEADING}\n\n${body}` + (after ? "\n" + after : "");
}

// Trim surrounding quotes and dashes, but keep the word as written:
// right-clicking "crafter's" adds "crafter's", not "crafter".
function cleanWord(raw) {
  return raw.trim().replace(/^['’-]+|['’-]+$/g, "");
}

module.exports = class PersonalDictionaryPlugin extends Plugin {
  async onload() {
    this.lastPointer = null;
    this.loaded = new Set();

    // Remember where the right-click happened; editor-menu doesn't say.
    this.registerDomEvent(document, "contextmenu", (evt) => {
      this.lastPointer = { x: evt.clientX, y: evt.clientY };
    }, { capture: true });

    this.registerEvent(this.app.workspace.on("editor-menu", (menu, editor) => {
      const word = this.wordForMenu(editor);
      if (!word) return;
      menu.addItem((item) => item
        .setSection("spellcheck")
        .setTitle(`Add "${word}" to personal dictionary`)
        .setIcon("book-plus")
        .onClick(() => this.addWord(word)));
    }));

    this.addCommand({
      id: "add-word-at-cursor",
      name: "Add word at cursor to personal dictionary",
      editorCallback: (editor) => {
        const word = this.wordAtCursor(editor);
        if (word) this.addWord(word);
        else new Notice("No word at the cursor");
      },
    });

    this.addCommand({
      id: "open-personal-dictionary",
      name: "Open personal dictionary note",
      callback: () => this.app.workspace.openLinkText(NOTE_PATH, "", false),
    });

    this.registerEvent(this.app.vault.on("modify", (file) => {
      if (file.path === NOTE_PATH) this.loadIntoSpellChecker();
    }));
    this.app.workspace.onLayoutReady(() => this.loadIntoSpellChecker());
  }

  noteFile() {
    const f = this.app.vault.getAbstractFileByPath(NOTE_PATH);
    return f instanceof TFile ? f : null;
  }

  // Selection wins; otherwise the word under the pointer, falling back to the
  // cursor when the click didn't land on text.
  wordForMenu(editor) {
    const sel = cleanWord(editor.getSelection());
    if (sel) return /\s/.test(sel) ? null : sel;
    const cm = editor.cm;
    if (cm && this.lastPointer) {
      const pos = cm.posAtCoords(this.lastPointer);
      if (pos != null) {
        const line = cm.state.doc.lineAt(pos);
        const word = this.wordAt(line.text, pos - line.from);
        if (word) return word;
      }
    }
    return this.wordAtCursor(editor);
  }

  wordAtCursor(editor) {
    const cur = editor.getCursor();
    return this.wordAt(editor.getLine(cur.line), cur.ch);
  }

  wordAt(text, ch) {
    let start = ch, end = ch;
    while (start > 0 && WORD_CHAR.test(text[start - 1])) start--;
    while (end < text.length && WORD_CHAR.test(text[end])) end++;
    const word = cleanWord(text.slice(start, end));
    return /\p{L}/u.test(word) ? word : null;
  }

  async addWord(word) {
    const file = this.noteFile();
    if (!file) {
      new Notice(`Personal dictionary note not found: ${NOTE_PATH}`);
      return;
    }
    let added = false;
    await this.app.vault.process(file, (text) => {
      const note = splitNote(text);
      if (note.words.has(word)) return text;
      note.words.add(word);
      added = true;
      return joinNote(note);
    });
    const session = spellSession();
    if (session) {
      session.addWordToSpellCheckerDictionary(word);
      this.loaded.add(word);
    }
    this.respell();
    new Notice(added ? `Added "${word}" to personal dictionary` : `"${word}" is already in your personal dictionary`);
  }

  // Make Obsidian's live spell checker match the note: add new words, drop
  // ones this plugin added that have since been deleted from the note.
  async loadIntoSpellChecker() {
    const file = this.noteFile();
    const session = spellSession();
    if (!file || !session) return;
    const { words } = splitNote(await this.app.vault.cachedRead(file));
    const known = new Set(await session.listWordsInSpellCheckerDictionary());
    for (const w of words) {
      if (!known.has(w)) session.addWordToSpellCheckerDictionary(w);
    }
    for (const w of this.loaded) {
      if (!words.has(w) && known.has(w)) session.removeWordFromSpellCheckerDictionary(w);
    }
    this.loaded = words;
  }

  // Chromium only re-checks text when it changes; nudge open editors so the
  // squiggle under a newly added word goes away.
  respell() {
    this.app.workspace.getLeavesOfType("markdown").forEach((leaf) => {
      const el = leaf.view instanceof MarkdownView && leaf.view.editor.cm && leaf.view.editor.cm.contentDOM;
      if (!el) return;
      el.spellcheck = false;
      requestAnimationFrame(() => { el.spellcheck = true; });
    });
  }
};
