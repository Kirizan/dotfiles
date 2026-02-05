---
name: code-review
description: Performs code quality analysis. Use when the user says "review this code", "code review", "check code quality", "analyze this code", or asks for feedback on code structure, patterns, or quality.
context: fork
agent: Explore
argument-hint: "[file, directory, or blank for git diff]"
---

# Code Review

Analyze code for quality, correctness, and maintainability. This skill runs in a forked (read-only) context — it reports findings but does not modify code.

## Process

### 1. Determine Scope

- If an argument is provided, review that file or directory
- If no argument, use `git diff HEAD` to identify changed files
- If no diff either, ask the user what to review

### 2. Analyze Code

For each file in scope, evaluate:

- **Correctness**: Logic errors, off-by-one, null/nil handling, race conditions
- **Error handling**: Uncaught exceptions, swallowed errors, missing edge cases
- **Naming**: Variable/function names that are misleading, too vague, or inconsistent
- **Structure**: Functions doing too much, deep nesting, duplicated logic
- **Shell-specific**: Unquoted variables, missing `set -euo pipefail`, unsafe expansions
- **Language idioms**: Non-idiomatic patterns for the language in use

Consult `references/language-patterns.md` for language-specific patterns.

### 3. Generate Report

Output findings grouped by severity:

```
## Code Review: [scope]

### CRITICAL
- `file.py:42` — Description of the issue

### WARNING
- `file.sh:17` — Description of the concern

### INFO
- `utils.py:88` — Suggestion for improvement

### POSITIVE
- Good use of X pattern in `module.py`
```

## Rules

- Always include `file:line` references
- Be specific — "this variable could be null on line 42" not "watch out for nulls"
- Don't nitpick formatting or style preferences unless they cause bugs
- Acknowledge good patterns in the POSITIVE section
- If the code is solid, say so — don't manufacture issues
- Never suggest changes that alter behavior unless there's a bug
