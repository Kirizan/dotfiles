# KITT Architecture Reference

## Complete File Inventory

```
~/projects/kitt/
├── pyproject.toml               # Poetry config, kitt package, extras: datasets, web, cli_ui
├── README.md
├── LICENSE                      # Apache 2.0
├── Dockerfile                   # Run KITT itself from container
├── docker-compose.yml
├── src/kitt/
│   ├── __init__.py              # __version__ = "1.1.0"
│   ├── __main__.py              # python -m kitt entry point
│   ├── cli/
│   │   ├── main.py              # CLI group + fingerprint, compare, web commands
│   │   ├── run.py               # kitt run
│   │   ├── engine_commands.py   # kitt engines {list,check,setup}
│   │   ├── test_commands.py     # kitt test {list,new}
│   │   ├── results_commands.py  # kitt results {init,list,compare,import,submit,cleanup}
│   │   └── compare_tui.py       # kitt compare (Textual TUI)
│   ├── engines/
│   │   ├── base.py              # InferenceEngine ABC, GenerationResult, GenerationMetrics
│   │   ├── registry.py          # EngineRegistry, @register_engine
│   │   ├── docker_manager.py    # DockerManager, ContainerConfig
│   │   ├── openai_compat.py     # Shared OpenAI-compatible API client
│   │   ├── vllm_engine.py       # vLLM implementation
│   │   ├── tgi_engine.py        # TGI implementation
│   │   ├── llama_cpp_engine.py  # llama.cpp implementation
│   │   └── ollama_engine.py     # Ollama implementation
│   ├── benchmarks/
│   │   ├── base.py              # LLMBenchmark ABC, BenchmarkResult, WarmupConfig
│   │   ├── registry.py          # BenchmarkRegistry, @register_benchmark
│   │   ├── loader.py            # YAMLBenchmark, BenchmarkLoader
│   │   ├── dataset_manager.py   # DatasetManager (HuggingFace, local, JSONL)
│   │   ├── performance/
│   │   │   ├── throughput.py    # ThroughputBenchmark
│   │   │   ├── latency.py       # LatencyBenchmark
│   │   │   ├── memory.py        # MemoryBenchmark
│   │   │   └── warmup_analysis.py
│   │   └── quality/
│   │       ├── standard/        # MMLU, GSM8K, TruthfulQA, HellaSwag
│   │       └── custom/          # User-defined quality benchmarks
│   ├── config/
│   │   ├── models.py            # Pydantic: TestConfig, SuiteConfig, SamplingParams, etc.
│   │   └── loader.py            # load_yaml(), load_config(), load_test_config(), etc.
│   ├── hardware/
│   │   ├── fingerprint.py       # HardwareFingerprint class
│   │   └── detector.py          # detect_gpu(), detect_cpu(), detect_ram(), detect_storage()
│   ├── collectors/
│   │   ├── gpu_stats.py         # GPUMonitor, GPUMemoryTracker (context manager)
│   │   ├── metrics.py
│   │   └── system_info.py
│   ├── runners/
│   │   ├── suite.py             # SuiteRunner, SuiteResult
│   │   ├── single_test.py       # SingleTestRunner
│   │   └── checkpoint.py        # CheckpointManager
│   ├── reporters/
│   │   ├── json_reporter.py     # suite_result_to_dict(), save_json_report()
│   │   ├── markdown.py          # generate_summary()
│   │   └── comparison.py        # compare_metrics()
│   ├── git_ops/
│   │   ├── repo_manager.py      # KARRRepoManager
│   │   ├── pr_creator.py
│   │   └── config.py
│   ├── web/
│   │   └── app.py               # Flask dashboard (requires -E web)
│   └── utils/
│       ├── compression.py       # ResultCompression (50MB chunks, gzip)
│       ├── validation.py
│       └── versioning.py
├── configs/
│   ├── suites/
│   │   ├── quick.yaml           # throughput only, 1 run
│   │   ├── standard.yaml        # all benchmarks, 3 runs
│   │   └── performance.yaml     # perf benchmarks, 3 runs
│   ├── engines/
│   │   ├── vllm.yaml
│   │   ├── tgi.yaml
│   │   ├── ollama.yaml
│   │   ├── llama_cpp.yaml
│   │   └── parameter_mapping.yaml
│   └── tests/
│       ├── performance/         # latency.yaml, memory.yaml, throughput.yaml, warmup.yaml
│       └── quality/standard/    # mmlu.yaml, gsm8k.yaml, truthfulqa.yaml, hellaswag.yaml
└── tests/
    ├── test_engines/
    ├── test_benchmarks/
    ├── test_config/
    ├── test_git_ops/
    └── integration/
```

## Core Data Models

### Engine Types (dataclasses)

```python
@dataclass
class GenerationMetrics:
    ttft_ms: float                  # Time to first token
    tps: float                      # Tokens per second
    total_latency_ms: float
    gpu_memory_peak_gb: float
    gpu_memory_avg_gb: float
    timestamp: datetime

@dataclass
class GenerationResult:
    output: str
    metrics: GenerationMetrics
    prompt_tokens: int
    completion_tokens: int

@dataclass
class EngineDiagnostics:
    available: bool
    image: str = ""
    error: Optional[str] = None
    guidance: Optional[str] = None

@dataclass
class ContainerConfig:
    image: str
    port: int                       # Host port
    container_port: int
    gpu: bool = True
    volumes: Dict[str, str]         # {host: container}
    env: Dict[str, str]
    extra_args: List[str]           # ["--shm-size=8g"]
    command_args: List[str]         # Engine-specific CLI args
    name_prefix: str = "kitt"
```

### Benchmark Types (dataclasses)

```python
@dataclass
class BenchmarkResult:
    test_name: str
    test_version: str
    passed: bool
    metrics: Dict[str, Any]
    outputs: List[Any]
    errors: List[str]
    timestamp: datetime
    run_number: int = 1
    warmup_times: List[float]

@dataclass
class SuiteResult:
    suite_name: str
    results: List[BenchmarkResult]
    timestamp: datetime
    total_time_seconds: float
    # Properties: passed, total_benchmarks, passed_count, failed_count
```

### Hardware Types (dataclasses)

```python
@dataclass
class GPUInfo:
    model: str; vram_gb: int; count: int = 1

@dataclass
class CPUInfo:
    model: str; cores: int; threads: int

@dataclass
class StorageInfo:
    brand: str; model: str; type: str  # nvme|ssd|hdd|unknown

@dataclass
class SystemInfo:
    gpu: Optional[GPUInfo]; cpu: CPUInfo; ram_gb: int; ram_type: str
    storage: StorageInfo; cuda_version: Optional[str]
    driver_version: Optional[str]; os: str; kernel: str
    environment_type: str
```

### Configuration Models (Pydantic)

```python
class SamplingParams(BaseModel):
    temperature: float = 0.0       # [0.0, 2.0]
    top_p: float = 1.0             # [0.0, 1.0]
    top_k: int = 50
    max_tokens: int = 2048

class TestConfig(BaseModel):
    name: str; version: str; category: str; description: str
    warmup: WarmupConfig; dataset: DatasetConfig
    prompts: PromptConfig; sampling: SamplingParams
    evaluation: EvaluationConfig; runs: int
    performance_collection: PerformanceCollectionConfig
    test_config: Dict[str, Any]

class SuiteConfig(BaseModel):
    suite_name: str; version: str; description: str
    tests: List[str]; global_config: Dict[str, Any]
    sampling_overrides: Optional[SamplingParams]
    test_overrides: Dict[str, SuiteOverrides]
```

## Docker Container Lifecycle

```python
# 1. Build config
config = ContainerConfig(image="vllm/vllm-openai:latest", port=8000, ...)

# 2. Launch
container_id = DockerManager.run_container(config)

# 3. Wait for ready
DockerManager.wait_for_healthy("http://localhost:8000/health",
                                timeout=300.0, container_id=container_id)

# 4. Use engine
result = engine.generate(prompt, temperature=0.0, max_tokens=256, ...)

# 5. Cleanup
DockerManager.stop_container(container_id, timeout=10)
```

Health check: exponential backoff starting at 2s, max 10s interval, 300s total timeout.

## GPU Memory Profiling Pattern

```python
with GPUMemoryTracker(gpu_index=0, sample_interval_ms=100) as tracker:
    result = engine.generate(prompt, ...)
peak_gb = tracker.get_peak_memory_mb() / 1024
avg_gb = tracker.get_average_memory_mb() / 1024
```

## KARR Results Directory Structure

```
karr-{fingerprint[:40]}/
├── hardware_fingerprint.txt
├── .gitattributes              # LFS tracking for *.jsonl.gz, *.bin
├── README.md
└── {model_name}/
    └── {engine_name}/
        └── {YYYY-MM-DD_HHMMSS}/
            ├── config.json
            ├── metrics.json
            ├── summary.md
            ├── hardware.json
            └── outputs/
                └── results_chunk_*.jsonl.gz
```

## Suite Configuration Example

```yaml
# configs/suites/standard.yaml
suite_name: standard
version: "1.0.0"
description: "Full evaluation"
tests:
  - throughput
  - latency
  - memory
  - warmup_analysis
  - mmlu
  - gsm8k
  - truthfulqa
  - hellaswag
global_config:
  runs: 3
sampling_overrides:
  temperature: 0.0
  max_tokens: 2048
test_overrides:
  mmlu:
    runs: 1
```

## Dependencies

```
python ^3.10, pyyaml ^6.0, click ^8.1, pydantic ^2.0,
py-cpuinfo ^9.0, psutil >=5.9, nvidia-ml-py >=12.535,
gitpython ^3.1, rich >=13.0

Extras:
  datasets: [datasets]       # HuggingFace datasets for quality benchmarks
  web: [flask]               # Web dashboard
  cli_ui: [textual]          # Interactive TUI comparisons
  all: [datasets, flask, textual]
```

## Testing

- pytest ^8.0 with pytest-mock, pytest-timeout (30s default)
- Coverage target: 40% (`fail_under = 40`)
- Test paths: `tests/`
- Python path: `src/`
- Mock Docker/GPU interactions in unit tests
