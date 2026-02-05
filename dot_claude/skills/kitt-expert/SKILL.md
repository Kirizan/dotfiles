---
name: kitt-expert
description: Use when working in the KITT project (~/projects/kitt). Covers inference engine abstraction, benchmark system, Docker management, hardware fingerprinting, KARR results storage, and CLI commands.
---

# KITT Project Expert

KITT (Kirizan's Inference Testing Tools) is an end-to-end testing and benchmarking suite for LLM inference engines. It measures quality consistency and performance across vLLM, TGI, llama.cpp, and Ollama.

## Project Location

`~/projects/kitt/`

## Technical Stack

- **Language:** Python 3.10+
- **Build tool:** Poetry
- **CLI framework:** Click
- **Output:** Rich (tables, panels, spinners)
- **Config validation:** Pydantic v2
- **Hardware detection:** py-cpuinfo, psutil, nvidia-ml-py (pynvml)
- **Git integration:** GitPython
- **Container management:** Docker CLI via subprocess
- **License:** Apache 2.0

## Architecture

```
src/kitt/
├── cli/           # Click commands (run, engines, test, results, compare, web, fingerprint)
├── engines/       # Inference engine plugins (base ABC, registry, vllm, tgi, llama_cpp, ollama)
├── benchmarks/    # Benchmark plugins (base ABC, registry, performance/*, quality/*)
├── config/        # Pydantic models + YAML loader
├── hardware/      # System fingerprinting (GPU, CPU, RAM, storage, CUDA)
├── runners/       # Suite/single test runners + checkpoint recovery
├── collectors/    # GPU memory tracking, system metrics
├── reporters/     # JSON, Markdown, comparison output
├── git_ops/       # KARR repository management
├── web/           # Flask dashboard
└── utils/         # Compression, validation, versioning
```

## Key Patterns

### Engine Plugin System
- `InferenceEngine` ABC in `engines/base.py`
- `EngineRegistry` in `engines/registry.py` with `@register_engine` decorator
- `EngineRegistry.auto_discover()` imports all built-in engines
- Each engine manages its own Docker container via `DockerManager`
- All engines expose `initialize()`, `generate()`, `cleanup()`

### Engine Implementations
| Engine | Docker Image | API | Port | Formats |
|--------|-------------|-----|------|---------|
| vLLM | `vllm/vllm-openai:latest` | OpenAI `/v1/completions` | 8000 | safetensors, pytorch |
| TGI | `ghcr.io/huggingface/text-generation-inference:latest` | HF `/generate` | 8080 | safetensors, pytorch |
| llama.cpp | `ghcr.io/ggerganov/llama.cpp:server` | OpenAI `/v1/completions` | 8081 | gguf |
| Ollama | `ollama/ollama:latest` | Ollama `/api/generate` | 11434 | gguf |

### Benchmark System
- `LLMBenchmark` ABC in `benchmarks/base.py` with `run()` and `_execute()`
- `BenchmarkRegistry` with `@register_benchmark` decorator
- YAML-defined benchmarks via `YAMLBenchmark` class in `benchmarks/loader.py`
- Checkpoint recovery every 100 items via `CheckpointManager`

### Built-in Benchmarks
**Performance:** throughput, latency, memory, warmup_analysis
**Quality:** mmlu, gsm8k, truthfulqa, hellaswag

### Docker Management
- `DockerManager` in `engines/docker_manager.py` — static methods
- Uses `subprocess` to call `docker` CLI (no Docker SDK)
- Container naming: `kitt-{timestamp}`
- Health checks: exponential backoff, 300s timeout, max 10s interval
- Always uses `--network host` for port binding

### Hardware Fingerprinting
- `HardwareFingerprint.generate()` → compact string like `rtx4090-24gb_i9-13900k-24c_64gb-ddr5_samsung-990pro-nvme_cuda-12.4_550.90_linux-6.8`
- `HardwareFingerprint.detect_system()` → `SystemInfo` dataclass
- Detects: GPU (pynvml or nvidia-smi), CPU, RAM, storage type, CUDA, driver, OS, environment type

### Environment Types
`dgx_spark`, `dgx`, `wsl2`, `docker`, `container`, `native_linux`, `native_macos`, `native_windows`

### KARR (Git-Backed Results)
- `KARRRepoManager` in `git_ops/repo_manager.py`
- Directory: `karr-{fingerprint[:40]}/`
- Structure: `{model}/{engine}/{timestamp}/` with metrics.json, summary.md, hardware.json, config.json
- Git LFS for large result files (*.jsonl.gz)
- Results compressed in 50MB chunks

### Configuration
- YAML configs in `configs/` directory (suites, engines, tests)
- Pydantic models in `config/models.py` for validation
- `load_yaml()` → `load_config(path, ModelClass)` pattern
- Suite configs reference test names, with `global_config` and per-test `test_overrides`

### Test Suites
| Suite | Purpose | Benchmarks | Runs |
|-------|---------|------------|------|
| quick | Smoke test | throughput only | 1 |
| standard | Full evaluation | all quality + performance | 3 |
| performance | Perf-focused | throughput, latency, memory, warmup | 3 |

## CLI Commands

| Command | Description | Key Options |
|---------|-------------|-------------|
| `kitt run` | Execute benchmarks | `-m MODEL`, `-e ENGINE`, `-s SUITE`, `-o OUTPUT`, `--store-karr` |
| `kitt engines list` | List engines + status | |
| `kitt engines check NAME` | Check engine availability | |
| `kitt engines setup NAME` | Pull Docker image | `--dry-run` |
| `kitt test list` | List benchmarks | `--category` |
| `kitt test new NAME` | Create custom benchmark | |
| `kitt results init` | Initialize KARR repo | `--path` |
| `kitt results list` | List stored results | `--model`, `--engine` |
| `kitt results compare` | Compare runs | positional run paths |
| `kitt fingerprint` | Show hardware fingerprint | `--verbose` |
| `kitt compare` | Interactive TUI | positional run paths |
| `kitt web` | Web dashboard | `--port`, `--host`, `--results-dir` |

## Conventions

1. **Dataclasses** for result types, **Pydantic** for config validation
2. **`@register_engine`** / **`@register_benchmark`** decorators for plugin registration
3. **Docker via subprocess** — no Docker SDK dependency
4. **`logging.getLogger(__name__)`** throughout
5. **Full type hints** on all public methods
6. **Checkpoint recovery** for long benchmarks (save every 100 items)
7. **GPU memory profiling** via `GPUMemoryTracker` context manager

## Important: LLM-Only

KITT only works with **generative LLMs** (Llama, Qwen, Mistral, etc.). Encoder models like BERT are incompatible — they can't generate text and won't load in any supported engine.

## Relationship to DEVON

- DEVON manages/stores models, KITT tests them
- `devon export --format kitt -o models.txt` produces paths KITT can consume
- Both share: Poetry, Click, Rich, Python 3.10+, plugin registry pattern

## Additional Resources

For detailed data models, Docker patterns, and benchmark configs, see:
- **`references/architecture.md`**
