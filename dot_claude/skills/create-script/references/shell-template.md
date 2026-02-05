# Shell Script Template

Use this as the starting structure for bash scripts. Remove sections that don't apply.

```bash
#!/usr/bin/env bash
# script-name — Brief description of what this script does
#
# Usage: script-name [OPTIONS] <required-arg>

set -euo pipefail

# --- Constants -----------------------------------------------------------

readonly SCRIPT_NAME="${0##*/}"
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# --- Colors (NO_COLOR support) -------------------------------------------

if [[ -z "${NO_COLOR:-}" ]] && [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BOLD='\033[1m'
    RESET='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BOLD=''
    RESET=''
fi

# --- Logging helpers ------------------------------------------------------

die() { printf "${RED}error:${RESET} %s\n" "$*" >&2; exit 1; }
warn() { printf "${YELLOW}warning:${RESET} %s\n" "$*" >&2; }
info() { printf "${GREEN}info:${RESET} %s\n" "$*"; }

# --- Usage ----------------------------------------------------------------

usage() {
    cat <<EOF
${BOLD}Usage:${RESET} ${SCRIPT_NAME} [OPTIONS] <argument>

Brief description of what this script does.

${BOLD}Arguments:${RESET}
  <argument>        Description of required argument

${BOLD}Options:${RESET}
  -h, --help        Show this help message
  -v, --verbose     Enable verbose output
  -n, --dry-run     Show what would be done without doing it

${BOLD}Examples:${RESET}
  ${SCRIPT_NAME} foo          Do the thing with foo
  ${SCRIPT_NAME} -v bar       Do the thing verbosely with bar
EOF
}

# --- Argument parsing -----------------------------------------------------

VERBOSE=0
DRY_RUN=0

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                exit 0
                ;;
            -v|--verbose)
                VERBOSE=1
                shift
                ;;
            -n|--dry-run)
                DRY_RUN=1
                shift
                ;;
            --)
                shift
                break
                ;;
            -*)
                die "Unknown option: $1 (see --help)"
                ;;
            *)
                break
                ;;
        esac
    done

    # Validate required arguments
    if [[ $# -lt 1 ]]; then
        die "Missing required argument (see --help)"
    fi

    ARGUMENT="$1"
}

# --- Cleanup trap ---------------------------------------------------------

cleanup() {
    # Remove temp files, restore state, etc.
    :
}
trap cleanup EXIT

# --- Main logic -----------------------------------------------------------

main() {
    parse_args "$@"

    # Script logic here
    info "Processing: ${ARGUMENT}"
}

main "$@"
```

## Key Principles

- `set -euo pipefail` — exit on error, undefined vars, pipe failures
- Quote all variables: `"$var"` not `$var`
- Use `[[ ]]` for conditionals, not `[ ]`
- Use `"$@"` to pass arguments, not `$*`
- Use `local` for function variables
- Use `readonly` for constants
- Use `mktemp` for temp files: `tmp=$(mktemp)` or `tmpdir=$(mktemp -d)`
- Use `--` to separate options from arguments in commands: `rm -- "$file"`
- Check command existence: `command -v tool >/dev/null 2>&1 || die "tool not found"`
- Prefer `printf` over `echo` for portability
