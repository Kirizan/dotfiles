# Skill Frontmatter Reference

All valid YAML frontmatter fields for `SKILL.md` files, placed between `---` delimiters.

## Required Fields

### `name`
- **Type**: string
- **Purpose**: Identifier for the skill, used in `/name` invocation
- **Format**: kebab-case (e.g., `code-review`, `create-script`)
- **Example**: `name: code-review`

### `description`
- **Type**: string
- **Purpose**: Controls when Claude automatically activates the skill. Include trigger phrases and contexts that should match user requests.
- **Example**: `description: Performs code quality analysis. Use when the user says "review this code", "code review", or asks for feedback on code quality.`
- **Tip**: Be specific with trigger phrases to avoid false activations

## Optional Fields

### `context`
- **Type**: string
- **Values**: `fork`
- **Purpose**: Run the skill in a forked (isolated) context. The skill gets a read-only copy of the conversation — it cannot modify files or affect the main session.
- **Use when**: The skill only analyzes/reports and should not make changes
- **Example**: `context: fork`

### `agent`
- **Type**: string
- **Values**: `Explore`, or other agent types
- **Purpose**: Run the skill as a specific agent subtype
- **Use when**: The skill primarily searches, reads, and analyzes code
- **Example**: `agent: Explore`

### `disable-model-invocation`
- **Type**: boolean
- **Values**: `true`
- **Purpose**: Prevent Claude from automatically invoking this skill based on description matching. The skill can only be triggered manually via `/skill-name`.
- **Use when**: The skill has side effects (creates files, runs commands) and should only run when explicitly requested
- **Example**: `disable-model-invocation: true`

### `argument-hint`
- **Type**: string
- **Purpose**: Shows the user what arguments the skill expects when they type `/skill-name`
- **Format**: Brief placeholder text showing expected arguments
- **Example**: `argument-hint: "[file or directory] [optional flags]"`

## Combinations

### Read-Only Analysis Skill
```yaml
---
name: analyzer
description: Analyzes code for X. Use when user asks to "analyze", "check", or "scan" code.
context: fork
agent: Explore
argument-hint: "[file or directory]"
---
```

### Action Skill (User-Invoked Only)
```yaml
---
name: generator
description: Generates X with proper structure.
disable-model-invocation: true
argument-hint: "[name] [description]"
---
```

### Auto-Activated Knowledge Skill
```yaml
---
name: domain-expert
description: Provides expertise on X. Use when user discusses Y, Z, or asks about A, B, C.
---
```
