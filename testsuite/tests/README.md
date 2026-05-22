# Test Suite Organization

Tests are grouped by the functionality they exercise.

## GPR Language

| Directory | Purpose |
|-----------|---------|
| `parser/` | Correct parsing of GPR project files |
| `variables/` | Typed variable declaration and evaluation |
| `built-ins/` | Built-in function evaluation |
| `packages/` | Package scoping, renaming, and inheritance |

## Project Tree

| Directory | Purpose |
|-----------|---------|
| `tree/` | Project tree loading and dependency resolution |
| `extension/` | Project extension and attribute inheritance |
| `aggregate/` | Aggregation of multiple project trees |
| `config/` | Toolchain configuration and target selection |
| `kb/` | Knowledge base querying and compiler description |
| `runtime/` | Runtime configuration and RTS selection |

## Sources

| Directory | Purpose |
|-----------|---------|
| `source/` | Source file discovery, naming, and ownership |
| `library/` | Library project build output |

## Build System

| Directory | Purpose |
|-----------|---------|
| `build-actions/` | Correctness of individual build steps |
| `build_db/` | Incremental build and signature-based change detection |
| `build_db_dag/` | DAG-based dependency tracking |
| `actions_scheduler/` | Parallel action execution |
| `build/` | Build configuration options |
| `ali_parser/` | ALI file parsing for dependency tracking |

## GPR2 Library API

| Directory | Purpose |
|-----------|---------|
| `gpr2-api/` | GPR2 library API contracts |
| `view/` | Project view queries |
| `attributes/` | Built-in attribute semantics |

## Tools

| Directory | Purpose |
|-----------|---------|
| `tools/` | GPR tool command-line behavior |
| `reporters/` | Diagnostic output formatting |

## Quality

| Directory | Purpose |
|-----------|---------|
| `robustness/` | Graceful handling of invalid or unexpected input |

## Miscellaneous

| Directory | Purpose |
|-----------|---------|
| `misc/` | Example programs and testsuite infrastructure |
