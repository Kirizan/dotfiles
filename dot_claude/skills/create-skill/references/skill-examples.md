# Skill Examples

Proven patterns for different types of Claude Code skills.

## Pattern 1: Read-Only Analysis

Skills that examine code and produce a report without modifying anything.

```yaml
---
name: code-review
description: Performs code quality analysis. Use when the user says "review this code" or "code review".
context: fork
agent: Explore
argument-hint: "[file, directory, or blank for git diff]"
---
```

**Characteristics:**
- Uses `context: fork` to guarantee no side effects
- Uses `agent: Explore` for efficient code searching
- Produces structured reports with `file:line` references
- Good for: reviews, scans, audits, documentation analysis

## Pattern 2: Generator / Creator

Skills that create files or artifacts based on user input.

```yaml
---
name: create-component
description: Creates a new UI component with tests and styles.
disable-model-invocation: true
argument-hint: "[component-name] [description]"
---
```

**Characteristics:**
- Uses `disable-model-invocation: true` — only runs when user types `/create-component`
- Has side effects (creates files), so it should not auto-activate
- Often includes templates in reference files
- Good for: scaffolding, boilerplate, project setup

## Pattern 3: Domain Expert

Skills that provide specialized knowledge for a specific codebase or domain.

```yaml
---
name: api-expert
description: Expert on the project's API layer. Use when user discusses API endpoints, REST patterns, request handling, middleware, or asks about API conventions.
---
```

**Characteristics:**
- No `context: fork` — may need to modify code
- No `disable-model-invocation` — should activate automatically when relevant
- Heavy use of reference files for architecture documentation
- Good for: complex codebases, specialized domains, team conventions

## Pattern 4: Workflow Automation

Skills that codify a multi-step workflow.

```yaml
---
name: release
description: Manages the release workflow.
disable-model-invocation: true
argument-hint: "[version]"
---
```

**Characteristics:**
- Uses `disable-model-invocation: true` — workflows should be explicitly requested
- Documents a strict sequence of steps
- Often involves git operations, CI checks, version bumps
- Good for: deployments, releases, migrations, data operations

## Directory Structure Examples

### Minimal Skill (No References)
```
commit/
└── SKILL.md
```

### Standard Skill
```
code-review/
├── SKILL.md
└── references/
    └── patterns.md
```

### Skill with Multiple References
```
create-script/
├── SKILL.md
└── references/
    ├── shell-template.md
    └── python-template.md
```
