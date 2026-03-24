# CLAUDE.md — Projects Workspace

## Skills — ALWAYS use them

You have skills available. Using them is a **blocking requirement** — invoke the matching skill BEFORE generating any other response about the task. Do not start work without first loading the relevant skill.

### Project expertise (use when working in that project)

| Skill | Trigger | Project |
|-------|---------|---------|
| **kitt-expert** | Any task in `~/projects/kitt/` | KITT — inference engine testing suite |
| **devon-expert** | Any task in `~/projects/devon/` | DEVON — model discovery and management |
| **dotfiles-expert** | Any task involving dotfiles, chezmoi, shell config, or `~/.claude/skills/` | Dotfiles repo |

Load the project skill at the start of every session when working in that project.

### Development workflows (use when the action matches)

| Skill | Trigger |
|-------|---------|
| **commit** | User asks to commit (`/commit`) |
| **code-review** | User asks to review code quality (`/code-review`) |
| **pr-review** | User asks to review a pull request (`/pr-review`) |
| **refactor** | User asks to refactor or clean up code (`/refactor`) |
| **security-scan** | User asks to scan for vulnerabilities (`/security-scan`) |
| **create-script** | User asks to generate a script (`/create-script`) |
| **create-skill** | User asks to create a new skill (`/create-skill`) |

### Reminder checklist

Before starting any task, ask yourself:
1. Am I in a project directory? → Load the project expert skill.
2. Is the user asking me to commit, review, refactor, or scan? → Use the workflow skill.
3. Am I about to write code without having loaded a relevant skill? → Stop and load it first.
