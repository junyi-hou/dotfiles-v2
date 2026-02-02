---
name: git-commit-message
description: Helps generate and prepare a high-quality git commit message based on staged changes. Use this when the user wants to commit changes but needs help drafting a message and wants to review/edit it before finalizing the commit.
---

# Git Commit Message

This skill helps you generate a structured, informative git commit message based on the current staged changes and opens it in the user's preferred editor for final review and manual commitment.

## Workflow

When a user asks to commit their changes or generate a commit message, follow these steps:

1.  **Check Staged Changes**: Run `git diff --cached` to see what changes are being committed.
2.  **Generate Message**: Analyze the diff and draft a commit message according to the requirements below.
3.  **Commit with Editor**: Use the `scripts/prepare_commit_msg.sh` script to launch `git commit` with the generated message pre-filled in the user's editor.
4.  **Completion**: The user reviews and finishes the commit in their editor.
    - **CRITICAL**: If the `prepare_commit_msg.sh` script fails, is interrupted, or the editor process is canceled by the user, **DO NOT** attempt to commit the changes using `git commit -m`. Inform the user that the commit was aborted.

## Formatting Requirements

When generating commit messages, adhere to these standards:

- **Structure**: Use a short summary line (max 50-72 chars), followed by a blank line and a more detailed body if necessary.
- **Prefixes**: Use Conventional Commits style prefixes:
    - `feat:` for new features
    - `fix:` for bug fixes
    - `docs:` for documentation changes
    - `style:` for formatting (no code changes)
    - `refactor:` for code changes that neither fix a bug nor add a feature
    - `perf:` for performance improvements
    - `test:` for adding/updating tests
    - `chore:` for maintenance tasks
- **Tone**: Use the imperative mood (e.g., "Add feature" instead of "Added feature").
- **Body**: Explain the *why* behind the change, not just the *what*. Highlight any breaking changes with `BREAKING CHANGE:`.

### Example Usage

If the user says "Commit these changes for me", you should:
- Read staged changes.
- Generate a message.
- Run `bash scripts/prepare_commit_msg.sh "The generated message"`.
- Tell the user: "I've started the commit process and pre-filled the message in your editor. Please review and save to complete the commit."

## Resources

### scripts/

- `prepare_commit_msg.sh`: Takes a commit message as an argument and runs `git commit -e -m` to open it in the system's `$EDITOR`.
