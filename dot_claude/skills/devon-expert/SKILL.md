---
name: devon-expert
description: Use when working in the DEVON project (~/projects/devon). Covers CLI commands, source plugin architecture, storage management, HuggingFace integration, and configuration system.
---

# DEVON Project Expert

DEVON (Discovery Engine and Vault for Open Neural models) is a CLI tool for discovering, downloading, and managing LLM models from HuggingFace. It's the model management counterpart to KITT.

## Project Location

`~/projects/devon/`

## Technical Stack

- **Language:** Python 3.10+
- **Build tool:** Poetry
- **CLI framework:** Click
- **Output:** Rich (tables, panels, spinners)
- **Model source:** HuggingFace Hub API
- **Config:** YAML with deep merge
- **Data models:** Python dataclasses
- **License:** Apache 2.0

## Architecture

```
src/devon/
├── cli/           # Click commands (search, download, list, info, clean, export, status)
├── sources/       # Model source plugins (base ABC, registry, huggingface)
├── storage/       # Local model storage with JSON index
├── search/        # Query parsing and filtering
├── download/      # Download orchestration
├── models/        # ModelMetadata dataclass
├── config/        # Settings with YAML loading
└── utils/         # URL parser, size parser, format detection
```

## Key Patterns

### Source Plugin System
- `ModelSource` ABC in `sources/base.py` — defines `search()`, `get_model_info()`, `download_model()`
- `SourceRegistry` in `sources/registry.py` — class-level dict registry
- `@register_source` decorator for auto-registration
- `sources/__init__.py` imports HuggingFaceSource to trigger registration

### Storage
- `ModelStorage` in `storage/organizer.py` — JSON index at `{base_path}/../index.json`
- Keys: `{source}::{model_id}` (e.g., `huggingface::Qwen/Qwen2.5-32B`)
- Reads `base_path` from `~/.config/devon/config.yaml` via `Settings` class
- Default: `~/.cache/devon/models/`

### Configuration
- `Settings` class in `config/settings.py` — loads `~/.config/devon/config.yaml`
- Deep merges user config over `DEFAULT_CONFIG` dict
- Dot-notation access: `settings.get("storage.base_path")`
- Properties: `storage_path`, `default_source`, `search_limit`

### HuggingFace Source (v0.32+ API)
- Uses `HfApi.list_models()` with keyword args (NOT `ModelFilter` — removed in HF Hub 2.0)
- `card_data` is a `ModelCardData` object, not a dict — use `getattr()`
- Attributes are snake_case: `created_at`, `last_modified`, `card_data`
- `license` field can be a list — join with commas
- `snapshot_download()` no longer needs `resume_download=True` (always resumes)

### CLI Commands
| Command | File | Key Options |
|---------|------|-------------|
| `devon search` | `cli/search_cmd.py` | `--provider`, `--params`, `--size`, `--format`, `--task`, `--license`, `--limit`, `--source` |
| `devon download` | `cli/download_cmd.py` | `MODEL_ID_OR_URL`, `--source`, `--force` |
| `devon list` | `cli/list_cmd.py` | `--source` |
| `devon info` | `cli/info_cmd.py` | `MODEL_ID`, `--source` |
| `devon status` | `cli/status_cmd.py` | (none) |
| `devon clean` | `cli/clean_cmd.py` | `--unused`, `--days`, `--all`, `--dry-run` |
| `devon export` | `cli/export_cmd.py` | `--format kitt\|json`, `--output` |

## Conventions

1. **Poetry for deps** — `poetry install`, `eval $(poetry env activate)`
2. **Type hints** throughout
3. **`format_bytes()`** and **`format_number()`** are in `utils/size_parser.py` — don't duplicate in CLI commands
4. **URL detection** — `URLParser.is_url()` then `URLParser.parse()` to get `(source, model_id)`
5. **Error handling** — catch exceptions in CLI, print with `[red]Error: ...[/red]`
6. **No `ModelFilter`** — use kwargs directly with `list_models(author=..., library=..., etc.)`

## Relationship to KITT

- DEVON manages/stores models, KITT tests them
- `devon export --format kitt -o models.txt` produces paths KITT can consume
- Both use Poetry, Click, Rich, Python 3.10+

## Additional Resources

For detailed file inventory and data model fields, see:
- **`references/architecture.md`**
