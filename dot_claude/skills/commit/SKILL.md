---
name: commit
description: Creates a structured git commit with a descriptive message.
disable-model-invocation: true
---

# Commit

Create a well-structured git commit following repository conventions.

## Process

### 1. Assess Changes

Run in parallel:

```bash
git status
git diff
git diff --cached
git log --oneline -10
```

Understand:
- What files are modified, added, or deleted
- What the changes accomplish (read diffs carefully)
- The repository's existing commit message style (from recent log)

### 2. Stage Files

- Stage specific files by name — avoid `git add -A` or `git add .`
- Never stage files that contain secrets (`.env`, credentials, private keys)
- If sensitive files are detected, warn the user and skip them
- If unsure which files to stage, ask the user

### 3. Write Commit Message

Craft a commit message that:

- Starts with an imperative verb (Add, Fix, Update, Remove, Refactor, Configure)
- Summarizes the **why**, not just the **what**
- Matches the style of recent commits in the repository
- Is concise (ideally 1-2 sentences for the subject line)
- Uses a body paragraph for complex changes (blank line after subject)

Use a HEREDOC for proper formatting:
```bash
git commit -m "$(cat <<'EOF'
Subject line here

Optional body with more detail.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

### 4. Verify

```bash
git status
```

Confirm the commit was created successfully.

### 5. Inform User

Tell the user:
- What was committed (brief summary)
- That changes are ready to push when they're ready
- **Never push automatically**

## Rules

- **NEVER push to remote** — the user decides when to push
- **NEVER force push** or `--force` unless the user explicitly requests it
- **NEVER amend** previous commits unless the user explicitly requests it
- **NEVER skip hooks** (`--no-verify`) unless the user explicitly requests it
- **NEVER commit secrets** — warn if detected
- **Prefer specific file staging** over blanket `git add`
- If a pre-commit hook fails, fix the issue and create a **new** commit (don't amend)
- If there are no changes to commit, inform the user — don't create empty commits
