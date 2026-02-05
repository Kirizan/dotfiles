# DEVON Architecture Reference

## Complete File Inventory

```
~/projects/devon/
├── pyproject.toml              # Poetry config, devon-ai package
├── README.md                   # Usage docs with env activation + config examples
├── LICENSE                     # Apache 2.0
├── .gitignore
├── config/
│   └── default_config.yaml     # Reference config (not loaded at runtime)
├── src/devon/
│   ├── __init__.py             # __version__ = "1.0.0"
│   ├── __main__.py             # python -m devon entry point
│   ├── models/
│   │   └── model_info.py       # ModelMetadata dataclass
│   ├── sources/
│   │   ├── __init__.py         # Imports HuggingFaceSource for registration
│   │   ├── base.py             # ModelSource ABC
│   │   ├── registry.py         # SourceRegistry + @register_source
│   │   └── huggingface.py      # HuggingFaceSource implementation
│   ├── search/
│   │   ├── query_parser.py     # QueryParser — extracts inline filters from query strings
│   │   └── filters.py          # ModelFilter class for post-search filtering
│   ├── download/
│   │   ├── downloader.py       # DownloadManager orchestration
│   │   └── progress.py         # Rich progress bar factory
│   ├── storage/
│   │   ├── organizer.py        # ModelStorage — JSON index, register/list/delete
│   │   ├── index.py            # Re-export of ModelStorage
│   │   └── formats.py          # Re-export of format detection utils
│   ├── cli/
│   │   ├── main.py             # Click group, registers all subcommands
│   │   ├── search_cmd.py       # devon search
│   │   ├── download_cmd.py     # devon download
│   │   ├── list_cmd.py         # devon list
│   │   ├── info_cmd.py         # devon info
│   │   ├── status_cmd.py       # devon status
│   │   ├── clean_cmd.py        # devon clean
│   │   └── export_cmd.py       # devon export
│   ├── config/
│   │   └── settings.py         # Settings class — YAML loading, deep merge, dot-notation
│   └── utils/
│       ├── url_parser.py       # URLParser — HuggingFace URL detection
│       ├── size_parser.py      # parse_size(), format_bytes(), format_number(), parse_params()
│       └── format_utils.py     # detect_format(), detect_formats_from_files()
└── tests/
    ├── test_sources/
    │   └── test_registry.py    # Registry + custom source registration
    ├── test_search/
    │   └── test_query_parser.py
    ├── test_storage/
    │   └── test_organizer.py   # Full CRUD tests with tmp_path fixtures
    ├── test_download/
    └── test_utils/
        ├── test_url_parser.py
        ├── test_size_parser.py
        └── test_format_utils.py
```

## ModelMetadata Fields

```python
@dataclass
class ModelMetadata:
    source: str                        # "huggingface"
    model_id: str                      # "Qwen/Qwen2.5-32B"
    model_name: str                    # "Qwen2.5-32B"
    author: str                        # "Qwen"
    total_size_bytes: int
    file_count: int
    parameter_count: Optional[int]     # In billions (30 = 30B)
    architecture: Optional[str]        # "llama", "qwen", etc.
    format: List[str]                  # ["safetensors", "gguf"]
    quantization: Optional[str]        # "Q4_K_M", "fp16"
    tags: List[str]
    license: Optional[str]
    downloads: int
    likes: int
    created_at: str                    # ISO 8601
    updated_at: str                    # ISO 8601
    web_url: str
    repo_url: str
    extra: Dict[str, Any]
```

## Storage Index Structure

```json
{
  "huggingface::Qwen/Qwen2.5-32B": {
    "source": "huggingface",
    "model_id": "Qwen/Qwen2.5-32B",
    "path": "/home/user/models/huggingface/Qwen/Qwen2.5-32B",
    "metadata": { ... },
    "files": ["model.safetensors", "config.json"],
    "downloaded_at": "2024-01-15T10:30:00",
    "last_used": null,
    "size_bytes": 64424509440
  }
}
```

## Configuration (config.yaml)

```yaml
storage:
  base_path: ~/.cache/devon/models    # Override with absolute path
  max_size_gb: null                   # null = unlimited
download:
  resume: true
  verify_checksums: true
sources:
  default: huggingface
  enabled: [huggingface]
search:
  default_limit: 20
  sort_by: downloads
display:
  color: true
```

Loaded from `~/.config/devon/config.yaml`. Deep-merged over defaults.

## HuggingFace API Compatibility Notes

- `huggingface_hub >= 0.32`: `ModelFilter` class removed, use kwargs on `list_models()`
- `list_models()` returns `ModelInfo` with both camelCase and snake_case attrs
- Use `hf_model.id` (preferred) or `hf_model.modelId` (legacy)
- `card_data` is `ModelCardData` object — use `getattr(card_data, "license", None)`
- `license` can be `str` or `list` — handle both
- `siblings` may have `size=None` — filter with `if s.size`
- `snapshot_download()` always resumes — `resume_download` param is deprecated

## Test Patterns

- Use `tmp_path` fixture for storage tests (avoids touching real index)
- `ModelStorage(base_path=tmp_path / "models")` for isolated tests
- Create fake files with `write_bytes(b"x" * 1000)` for size tracking
- Source registry tests use custom ABC subclass, clean up with `del SourceRegistry._sources["name"]`
