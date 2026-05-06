---
name: commit-message
description: This skill should be used when the user asks to "generate a commit message", "write a commit message", "suggest a commit message", "what should my commit message be", or asks Claude to look at staged changes and produce a message. Do NOT run git commit.
version: 1.0.0
---

# Commit Message Generator

Generate a commit message from currently staged changes. Do NOT run `git commit` or any variant.

## Steps

1. Run `git diff --cached` to read the staged diff.
2. Run `git diff --cached --name-only` to get the list of changed files.
3. Analyze the diff: what changed, why it likely changed, which component/scope is affected.
4. Write a commit message using the Conventional Commits format:

```
<type>(<scope>): <short summary>

<optional body — only if the why is non-obvious>
```

**Types:** `feat`, `fix`, `refactor`, `chore`, `docs`, `test`, `style`, `perf`, `ci`

**Rules:**
- Subject line ≤ 72 characters, imperative mood ("add" not "added")
- Scope = the module, package, or subsystem (e.g. `emacs`, `zsh`, `ai`)
- Omit body unless the motivation needs explaining
- If multiple unrelated things are staged, note that and suggest splitting

## Output

Return only the commit message and nothing else with no quote. Do NOT run `git commit`.
