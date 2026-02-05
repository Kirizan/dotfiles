---
name: create-skill
description: Bootstraps a new Claude Code skill with proper structure, frontmatter, and reference files.
disable-model-invocation: true
argument-hint: "[skill-name] [brief description]"
---

# Create Skill

Create a new Claude Code skill with proper directory structure, SKILL.md frontmatter, and optional reference files.

## Process

### 1. Gather Requirements

From the arguments and conversation, determine:

- **Skill name**: kebab-case identifier (e.g., `deploy-helper`)
- **Purpose**: What the skill does
- **Invocation**: How users trigger it (automatic via description matching, or manual via `/skill-name`)
- **Context**: Does it need write access, or is it read-only analysis?
- **Tools**: Does it need specific tools (Bash, Explore, etc.)?

### 2. Determine Placement

Ask the user if needed:

| Location | Path | Scope |
|----------|------|-------|
| Personal | `~/.claude/skills/<name>/` | Available in all projects on this machine |
| Project | `.claude/skills/<name>/` | Available to anyone working on this project |

For the dotfiles repo, personal skills go in `dot_claude/skills/<name>/` (deployed by chezmoi).

### 3. Choose Frontmatter

Consult `references/frontmatter-reference.md` for all valid fields. At minimum:

```yaml
---
name: skill-name
description: When to activate this skill (trigger phrases and contexts)
---
```

Add optional fields based on the skill's nature:

- `context: fork` — for read-only analysis skills
- `agent: Explore` — for skills that primarily search/read code
- `disable-model-invocation: true` — for skills only invoked via `/skill-name`
- `argument-hint: "[args]"` — show expected arguments

### 4. Create Files

Create the directory structure:

```
<name>/
├── SKILL.md           # Required: frontmatter + instructions
└── references/        # Optional: supporting knowledge
    └── *.md
```

Write `SKILL.md` with:
1. Frontmatter block
2. Title and purpose
3. Process section (step-by-step workflow)
4. Rules section (constraints and guidelines)
5. References section (if reference files exist)

### 5. Verify

- Confirm the skill appears in `ls ~/.claude/skills/` (or project equivalent)
- Verify frontmatter is valid YAML between `---` delimiters

## Rules

- Skill names must be kebab-case
- `SKILL.md` is required — it's the entry point
- The `description` field controls when the skill activates — write it carefully with trigger phrases
- Reference files go in `references/` subdirectory as markdown
- Don't over-engineer — a skill can be just a SKILL.md with no references
- Consult `references/skill-examples.md` for proven patterns

## Reference Files

- **`references/frontmatter-reference.md`** — All valid frontmatter fields and their usage
- **`references/skill-examples.md`** — Example skill patterns for different use cases
