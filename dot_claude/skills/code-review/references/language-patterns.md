# Language-Specific Review Patterns

## Shell (Bash/Zsh)

### Critical
- **Unquoted variables**: `$var` instead of `"$var"` — word splitting and globbing
- **Missing error handling**: No `set -euo pipefail` in scripts
- **Command injection**: Unsanitized input in `eval`, backticks, or `$()`
- **Unsafe temp files**: Using predictable names in `/tmp` without `mktemp`

### Warning
- **Missing `--` in commands**: `rm $file` vs `rm -- "$file"` (filenames starting with `-`)
- **Using `[ ]` instead of `[[ ]]`**: Bash/zsh scripts should use `[[ ]]` for safety
- **Parsing `ls` output**: Use globs or `find` instead
- **Useless `cat`**: `cat file | grep` vs `grep file`
- **Not checking command existence**: `command -v tool >/dev/null` before using optional tools

### Info
- **Here-strings over echo pipes**: `grep x <<< "$var"` vs `echo "$var" | grep x`
- **`printf` over `echo`**: More portable for non-trivial output
- **Local variables in functions**: `local var` to avoid polluting global scope

## Python

### Critical
- **Bare `except:`**: Catches `SystemExit`, `KeyboardInterrupt` — use `except Exception:`
- **Mutable default arguments**: `def f(items=[])` — shared across calls
- **`eval()`/`exec()` with user input**: Code injection risk
- **`pickle` with untrusted data**: Arbitrary code execution

### Warning
- **Not using context managers**: `open()` without `with` — resource leaks
- **String formatting with `%` or `.format()` on user input**: Use f-strings or parameterized queries
- **Catching too broadly**: `except Exception` when a specific exception is expected
- **Type comparison with `==`**: Use `isinstance()` instead of `type(x) == Y`

### Info
- **Pathlib over os.path**: `Path()` is more readable and composable
- **List comprehensions over map/filter**: More Pythonic when readable
- **Dataclasses/namedtuples over plain dicts**: For structured data with known fields

## Lua

### Critical
- **Accidental globals**: Missing `local` keyword — leaks into global scope
- **Nil method calls**: `obj:method()` when `obj` might be nil — use guard checks
- **String concatenation in loops**: Use `table.concat()` instead of `..` in loops

### Warning
- **Not checking table key existence**: Accessing `tbl.key` when key may not exist
- **Using `#` on sparse tables**: Length operator unreliable on tables with gaps
- **Numeric for loop off-by-one**: Lua ranges are inclusive on both ends

### Info
- **Metatables for shared behavior**: Prefer metatables over copying methods
- **Local function references**: `local insert = table.insert` for hot paths

## Go Templates (Chezmoi)

### Critical
- **Undefined variable access**: Accessing `.var` not defined in data — silent empty string
- **Missing whitespace control**: `{{ if }}` vs `{{- if }}` — extra newlines in output

### Warning
- **Nested indentation**: Chezmoi convention is 2-space indent for nested blocks: `{{-   if }}`
- **Platform logic without fallback**: `if eq .chezmoi.os "darwin"` with no `else` — fails silently on other platforms
- **Hardcoded paths**: Use `.chezmoi.homeDir` instead of `/home/user`

### Info
- **`stat` for file existence**: `{{ if stat "/path" }}` to conditionally include config
- **`quote` for TOML values**: `{{ .var | quote }}` in `.chezmoi.toml.tmpl`
