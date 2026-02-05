# PR Review Checklist

Per-language checklists and cross-cutting concerns for pull request reviews.

## General Checklist

### Correctness
- [ ] Code does what the PR description claims
- [ ] Edge cases handled (empty input, null, zero, max values)
- [ ] Error paths return meaningful errors
- [ ] No off-by-one errors in loops or slicing
- [ ] Concurrent access handled if applicable

### Security
- [ ] No hardcoded secrets or credentials
- [ ] User input validated and sanitized
- [ ] No injection vulnerabilities (SQL, command, XSS)
- [ ] Authentication/authorization checks present where needed
- [ ] Sensitive data not logged

### Testing
- [ ] New functionality has tests
- [ ] Edge cases tested
- [ ] Error paths tested
- [ ] Tests are deterministic (no timing dependencies, random data without seeds)
- [ ] Test names describe what they verify

### Documentation
- [ ] Public API changes documented
- [ ] Non-obvious logic has comments
- [ ] README/docs updated if user-facing behavior changed
- [ ] Breaking changes documented in PR description

## Language-Specific Checklists

### Shell Scripts
- [ ] `set -euo pipefail` present
- [ ] Variables quoted: `"$var"`
- [ ] `[[ ]]` used instead of `[ ]` (bash/zsh)
- [ ] `mktemp` for temp files
- [ ] Functions use `local` for variables
- [ ] `--` separator before variable arguments
- [ ] Exit codes are meaningful

### Python
- [ ] No bare `except:` — use `except Exception:` at minimum
- [ ] No mutable default arguments
- [ ] Resources cleaned up (context managers / `with`)
- [ ] `subprocess` uses `shell=False`
- [ ] Type hints on public functions
- [ ] No `print()` for operational output — use `logging`

### JavaScript/TypeScript
- [ ] `async`/`await` errors handled (try/catch or `.catch()`)
- [ ] No `any` types without justification
- [ ] Event listeners cleaned up
- [ ] No prototype pollution patterns
- [ ] `===` used instead of `==`
- [ ] No `eval()` or `Function()` constructor

### Go
- [ ] Errors checked and wrapped with context
- [ ] Goroutine leaks prevented (context cancellation, WaitGroup)
- [ ] Nil pointer checks before dereference
- [ ] `defer` for cleanup (file handles, locks)
- [ ] Exported types/functions documented

### Lua
- [ ] Variables declared `local`
- [ ] Nil checks before method calls
- [ ] `table.concat()` instead of `..` in loops
- [ ] Module returns a table (not polluting globals)

## Backward Compatibility

### API Changes
- [ ] No removed endpoints/fields without deprecation period
- [ ] New required fields have defaults or migration
- [ ] Response format changes are additive, not breaking
- [ ] Version bump reflects change scope (semver)

### Configuration Changes
- [ ] Old config format still works or migration provided
- [ ] New required config has sensible defaults
- [ ] Environment variable changes documented

### Database/Schema Changes
- [ ] Migration is reversible
- [ ] No data loss in migration
- [ ] Indexes added for new query patterns
- [ ] Large table changes consider downtime/locking

## Commit History

- [ ] Each commit has a clear, descriptive message
- [ ] Commits are logically separated (not one giant commit)
- [ ] No "fix typo" or "WIP" commits in the final history
- [ ] No merge commits from pulling main into feature branch (rebase preferred)
