# Python Script Template

Use this as the starting structure for Python scripts. Remove sections that don't apply.

```python
#!/usr/bin/env python3
"""Brief description of what this script does.

Usage:
    script-name [OPTIONS] <required-arg>
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

logger = logging.getLogger(__name__)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "argument",
        help="Description of required argument",
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output",
    )
    parser.add_argument(
        "-n", "--dry-run",
        action="store_true",
        help="Show what would be done without doing it",
    )
    return parser.parse_args(argv)


def setup_logging(verbose: bool = False) -> None:
    """Configure logging based on verbosity."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(levelname)s: %(message)s",
    )


def main(argv: list[str] | None = None) -> int:
    """Main entry point. Returns exit code."""
    args = parse_args(argv)
    setup_logging(args.verbose)

    # Script logic here
    logger.info("Processing: %s", args.argument)

    return 0


if __name__ == "__main__":
    sys.exit(main())
```

## Key Principles

- Use `argparse` for argument parsing — never hand-roll
- Use `pathlib.Path` instead of `os.path` for file operations
- Use `logging` instead of `print` for operational output
- Use `if __name__ == "__main__":` guard
- Use type hints for function signatures
- Return exit codes from `main()` — `0` for success, non-zero for failure
- Use context managers (`with`) for file I/O and resources
- Prefer `subprocess.run()` over `os.system()` — always with `shell=False`
- Handle `KeyboardInterrupt` gracefully if the script runs long
- Use `from __future__ import annotations` for modern type hint syntax
