---
name: security-scan
description: Scans code for security vulnerabilities. Use when the user says "security scan", "check for vulnerabilities", "find security issues", "secrets scan", "check for leaked credentials", or asks about security posture of code.
context: fork
agent: Explore
argument-hint: "[file, directory, or blank for entire project]"
---

# Security Scan

Scan code for security vulnerabilities, leaked secrets, and unsafe patterns. This skill runs in a forked (read-only) context — it reports findings but does not modify code.

## Process

### 1. Determine Scope

- If an argument is provided, scan that file or directory
- If no argument, scan the entire project working directory
- Prioritize files that handle: user input, authentication, file I/O, network, subprocess execution

### 2. Secrets Detection

Search for hardcoded secrets using patterns from `references/vulnerability-checklist.md`:

- API keys, tokens, passwords in source code
- AWS access keys, GitHub tokens, private keys
- Connection strings with embedded credentials
- `.env` files committed to version control
- Base64-encoded secrets

### 3. Vulnerability Analysis

Check for:

- **Injection**: SQL injection, command injection, XSS, template injection
- **Authentication/Authorization**: Hardcoded credentials, missing auth checks, privilege escalation
- **File System**: Path traversal, unsafe temp files, world-writable permissions
- **Dependencies**: Known vulnerable patterns, unsafe deserialization
- **Shell Scripts**: Unquoted variables, unsafe `eval`, missing input validation
- **Network**: HTTP instead of HTTPS, disabled TLS verification, SSRF

### 4. Generate Report

```
## Security Scan: [scope]

### Secrets Scan
- [ ] No hardcoded API keys
- [ ] No embedded passwords
- [ ] No private keys in source
- [ ] No .env files in version control

### CRITICAL — Immediate Action Required
- `file:line` — Description, risk, remediation

### HIGH — Should Fix Soon
- `file:line` — Description, risk, remediation

### MEDIUM — Address When Possible
- `file:line` — Description, risk, remediation

### LOW — Consider Fixing
- `file:line` — Description, risk, remediation
```

## Rules

- Always include `file:line` references
- For each finding, explain: what the vulnerability is, what an attacker could do, and how to fix it
- Check `.gitignore` — warn if sensitive file patterns are not ignored
- Don't report theoretical vulnerabilities in code that never handles untrusted input
- False positives are worse than missed findings — be precise
