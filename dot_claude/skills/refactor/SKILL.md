---
name: refactor
description: Refactors code with clear patterns. Use when the user says "refactor", "clean up this code", "simplify", "extract function", "reduce duplication", or asks to restructure code without changing behavior.
argument-hint: "[file or directory] [optional goal, e.g., 'extract validation logic']"
---

# Refactor

Restructure code to improve clarity, reduce duplication, or simplify complexity — without changing external behavior.

## Process

### 1. Analyze Scope

- Read the target file(s) thoroughly
- Understand existing behavior and public interfaces
- Identify test coverage (if tests exist, they must still pass)

### 2. Identify Opportunities

Consult `references/refactoring-catalog.md` and look for:

- Long functions that do multiple things
- Deeply nested conditionals
- Duplicated logic across functions/files
- Dead code (unreachable branches, unused variables/functions)
- Unclear naming that obscures intent
- Overly complex expressions that could be broken into named steps

### 3. Present Plan

Before making any changes, present the plan:

```
## Refactoring Plan: [file/scope]

1. **Extract function**: Lines 42-67 of `handler.py` → `validate_input()`
   - Reason: This block handles input validation, separate from handler logic
2. **Simplify conditional**: Lines 89-112 → early return pattern
   - Reason: 4 levels of nesting → flat structure with guard clauses
3. **Remove dead code**: `legacy_handler()` on line 150
   - Reason: No callers found in codebase
```

Wait for user approval before proceeding.

### 4. Apply Changes

- Make one refactoring at a time
- After each change, verify the code is still correct
- If tests exist, run them after each significant change

### 5. Summarize

List what was changed and why:

```
## Refactoring Complete

- Extracted `validate_input()` from `handle_request()` (handler.py:42)
- Simplified nested conditional to early returns (handler.py:89)
- Removed unused `legacy_handler()` (handler.py:150)

All existing tests pass.
```

## Rules

- **Never change behavior** — refactoring is structural, not functional
- **One refactoring at a time** — don't combine multiple changes into one edit
- **Preserve public interfaces** — don't rename exports, public methods, or API contracts
- **Always present plan first** — get approval before modifying code
- **Run tests** if they exist — a refactoring that breaks tests is wrong
- **Don't refactor what isn't in scope** — resist the urge to "fix" unrelated code
- **Don't add features** — if you notice missing functionality, mention it but don't add it

## Reference Files

- **`references/refactoring-catalog.md`** — Catalog of common refactoring patterns
