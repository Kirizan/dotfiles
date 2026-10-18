// Personal Dictionary — adds "Add to personal dictionary" to the editor's
// right-click menu for the word under the pointer (or the selection), whether
// or not Chromium flagged it.
//
// It also improves spelling suggestions. Chromium's Hunspell often can't find
// a word that's one letter off ("shoud", "wich"), and Obsidian only lists
// suggestions when Chromium reports the misspelling in time. For a word
// Hunspell flags, the menu's suggestions become Aspell's, which rank the
// intended word first far more often, followed by any of Chromium's that
// Aspell didn't offer.
//
// Words go into the "## Words" section of a vault note, the master list that
// ~/.local/bin/personal-dict syncs to every other spell checker. The plugin
// also loads that list into Obsidian's live spell checker on start and
// whenever the note changes, so edits apply without a restart.
//
// Hand-written CommonJS, no build step. Deployed by chezmoi from
// ~/.config/obsidian-config/plugins/personal-dictionary.

const { Plugin, Notice, TFile, MarkdownView } = require("obsidian");
const { execFileSync } = require("child_process");

const NOTE_PATH = "3-Resources/Spelling/Personal Dictionary.md";
const HEADING = "## Words";
// Letters, marks and digits, plus apostrophes and hyphens inside a word.
const WORD_CHAR = /[\p{L}\p{M}\p{N}'’-]/u;
// Obsidian shows at most six suggestions; keep its limit.
const MAX_SUGGESTIONS = 6;
// Hunspell with the system dictionary Chromium also uses decides what's
// misspelled, so suggestions only appear for underlined words. Aspell
// supplies the suggestions.
const HUNSPELL = ["hunspell", ["-a", "-d", "en_US"]];
const ASPELL = ["aspell", ["-a", "--lang=en_US", "--sug-mode=normal", "--encoding=utf-8"]];

function spellSession() {
  try {
    return require("electron").remote.getCurrentWebContents().session;
  } catch (e) {
    return null;
  }
}

// Ask an ispell-compatible checker about one word. Returns
// { misspelled, suggestions }, or null when the checker is missing or splits
// the word into parts (hyphenated words) — then Chromium's menu is left alone.
function ispellCheck([cmd, args], word) {
  let out;
  try {
    out = execFileSync(cmd, args, { input: `^${word}\n`, encoding: "utf8", timeout: 1000 });
  } catch (e) {
    return null;
  }
  const lines = out.split("\n").slice(1).filter((l) => l.trim());
  if (lines.length !== 1) return null;
  const line = lines[0];
  if (line[0] === "&") {
    return { misspelled: true, suggestions: line.slice(line.indexOf(": ") + 2).split(", ") };
  }
  if (line[0] === "#") return { misspelled: true, suggestions: [] };
  return { misspelled: false, suggestions: [] };
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
    this.words = new Set();
    this.loaded = new Set();

    // Remember where the right-click happened; editor-menu doesn't say.
    this.registerDomEvent(document, "contextmenu", (evt) => {
      this.lastPointer = { x: evt.clientX, y: evt.clientY };
    }, { capture: true });

    // Obsidian fires editor-menu after it has added Chromium's suggestions,
    // so they can be merged here.
    this.registerEvent(this.app.workspace.on("editor-menu", (menu, editor) => {
      const target = this.targetForMenu(editor);
      if (!target || this.words.has(target.word)) return;
      this.improveSuggestions(menu, editor, target);
      menu.addItem((item) => item
        .setSection("spellcheck")
        .setTitle(`Add "${target.word}" to personal dictionary`)
        .setIcon("book-plus")
        .onClick(() => this.addWord(target.word)));
    }));

    this.addCommand({
      id: "add-word-at-cursor",
      name: "Add word at cursor to personal dictionary",
      editorCallback: (editor) => {
        const target = this.targetAtCursor(editor);
        if (target) this.addWord(target.word);
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

  // The word the menu is about, and where it sits so a suggestion can replace
  // it: { word, line, start, end }, or { word, selection: true }. Selection
  // wins; otherwise the word under the pointer, falling back to the cursor
  // when the click didn't land on text.
  targetForMenu(editor) {
    const raw = editor.getSelection();
    if (raw) {
      const word = cleanWord(raw);
      return word && !/\s/.test(word) && word === raw.trim() ? { word, selection: true } : null;
    }
    const cm = editor.cm;
    if (cm && this.lastPointer) {
      const pos = cm.posAtCoords(this.lastPointer);
      if (pos != null) {
        const line = cm.state.doc.lineAt(pos);
        const target = this.wordAt(line.text, pos - line.from, line.number - 1);
        if (target) return target;
      }
    }
    return this.targetAtCursor(editor);
  }

  targetAtCursor(editor) {
    const cur = editor.getCursor();
    return this.wordAt(editor.getLine(cur.line), cur.ch, cur.line);
  }

  wordAt(text, ch, line) {
    let start = ch, end = ch;
    while (start > 0 && WORD_CHAR.test(text[start - 1])) start--;
    while (end < text.length && WORD_CHAR.test(text[end])) end++;
    const raw = text.slice(start, end);
    const word = cleanWord(raw);
    if (!/\p{L}/u.test(word)) return null;
    start += raw.indexOf(word);
    return { word, line, start, end: start + word.length };
  }

  replaceWord(editor, target, replacement) {
    if (target.selection) editor.replaceSelection(replacement);
    else editor.replaceRange(replacement, { line: target.line, ch: target.start }, { line: target.line, ch: target.end });
  }

  // Put Aspell's suggestions first in the menu's correction section, then
  // Chromium's extras. Chromium's items are relabelled in place (they're
  // already positioned in the menu); more are added if Aspell found more.
  improveSuggestions(menu, editor, target) {
    const verdict = ispellCheck(HUNSPELL, target.word);
    if (verdict && !verdict.misspelled) return;
    const aspell = ispellCheck(ASPELL, target.word);
    if (!aspell || (!verdict && !aspell.misspelled)) return;

    const items = Array.isArray(menu.items) ? menu.items : [];
    const existing = items.filter((i) => i.section === "correction");
    const chromium = existing.filter((i) => !i.disabled).map((i) => i.titleEl.textContent);
    const merged = [...new Set([...aspell.suggestions, ...chromium])].slice(0, MAX_SUGGESTIONS);
    if (!merged.length) return;

    merged.forEach((s, n) => {
      const apply = () => this.replaceWord(editor, target, s);
      if (existing[n]) existing[n].setDisabled(false).setTitle(s).setIcon("lucide-repeat").onClick(apply);
      else menu.addItem((i) => i.setSection("correction").setTitle(s).setIcon("lucide-repeat").onClick(apply));
    });
    for (const extra of existing.slice(merged.length)) items.splice(items.indexOf(extra), 1);
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
    this.words.add(word);
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
    if (!file) return;
    const { words } = splitNote(await this.app.vault.cachedRead(file));
    this.words = words;
    const session = spellSession();
    if (!session) return;
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
