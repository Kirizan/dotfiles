---
name: create-script
description: Generates well-structured scripts with proper error handling, argument parsing, and best practices.
disable-model-invocation: true
argument-hint: "[language] [description of what the script should do]"
---

# Create Script

Generate production-quality scripts with proper structure, error handling, argument parsing, and documentation.

## Process

### 1. Determine Language

- If the first argument is `bash`, `sh`, `python`, or `py` — use that language
- If the argument is a filename with an extension (`.sh`, `.py`) — infer from extension
- If no language specified, default to bash

### 2. Determine Output Location

- If a filename is provided, use it
- If only a description is given, derive a sensible filename from the description
- Place scripts in the current directory unless the user specifies otherwise

### 3. Apply Template

Use the appropriate template from `references/`:
- Bash/sh: `references/shell-template.md`
- Python: `references/python-template.md`

Adapt the template to the user's described purpose. Remove sections that don't apply (e.g., no subcommands if the script does one thing).

### 4. Write and Finalize

- Write the script file
- Make it executable (`chmod +x` for shell scripts or add shebang for Python)
- Show the user what was created with a brief summary

## Rules

- Always include a `--help` / `-h` flag
- Always include error handling appropriate to the language
- Use the templates as a starting point — don't include boilerplate that doesn't serve the script's purpose
- Prefer clarity over cleverness
- Include comments only where the code isn't self-explanatory
- Use `NO_COLOR` environment variable support for colored output
