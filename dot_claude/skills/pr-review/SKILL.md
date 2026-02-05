---
name: pr-review
description: Reviews pull requests for correctness, quality, and risks. Use when the user says "review this PR", "review pull request", "PR review", "check this PR", or provides a PR number or URL for review.
context: fork
agent: Explore
argument-hint: "[PR number, URL, or blank for current branch's PR]"
---

# Pull Request Review

Review a pull request for correctness, quality, breaking changes, and risks. This skill runs in a forked (read-only) context.

## Process

### 1. Fetch PR Context

Use `gh` CLI to gather PR information:

```bash
# Get PR metadata and description
gh pr view <number>

# Get the full diff
gh pr diff <number>

# Get PR comments for context
gh api repos/{owner}/{repo}/pulls/<number>/comments

# Get commit history
gh pr view <number> --json commits
```

If no PR number is provided, check if the current branch has an open PR:
```bash
gh pr view
```

### 2. Understand the Change

- Read the PR description and linked issues
- Review commit history — does each commit tell a clear story?
- Understand the overall intent before reviewing individual files

### 3. Review Each File

For every changed file, evaluate:

- **Correctness**: Does the code do what the PR claims?
- **Edge cases**: Missing null checks, boundary conditions, error paths
- **Security**: Injection, auth bypass, secrets exposure (consult security-scan patterns)
- **Performance**: N+1 queries, unnecessary allocations, missing indexes
- **Breaking changes**: API changes, schema changes, config format changes
- **Test coverage**: Are new code paths tested? Are edge cases covered?

### 4. Check Cross-Cutting Concerns

- **Backward compatibility**: Will this break existing clients/users?
- **Documentation**: Do docs need updating for this change?
- **Migration**: Does this require data migration or deployment ordering?
- **Rollback**: Can this change be safely reverted?

### 5. Generate Review

```
## PR Review: #<number> — <title>

### Verdict: APPROVE | REQUEST CHANGES | COMMENT

### Summary
Brief assessment of the PR quality and purpose.

### Must Fix
- `file:line` — Issue that blocks merging

### Should Fix
- `file:line` — Issue worth addressing before merge

### Suggestions
- `file:line` — Optional improvement

### Positive
- Good patterns or decisions worth calling out

### Checklist
- [ ] Code correctness verified
- [ ] Error handling adequate
- [ ] No security concerns
- [ ] No breaking changes (or documented)
- [ ] Test coverage adequate
- [ ] Documentation updated (if needed)
```

## Rules

- Review the full diff — don't skip files
- Focus on correctness and risks over style preferences
- Be specific: `file:line` references for every finding
- "Must Fix" means you would request changes for this alone
- "Should Fix" means it's worth addressing but not blocking
- Don't nitpick formatting unless it causes bugs
- If the PR is good, say so — don't manufacture issues
- Consider the commit history — clean history is a signal of care

## Reference Files

- **`references/review-checklist.md`** — Per-language review checklist and compatibility checks
