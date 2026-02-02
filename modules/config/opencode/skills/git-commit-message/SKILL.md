---
name: git-commit-message
description: Helps generate and prepare a high-quality git commit message based on staged changes. Use this when the user wants to commit changes but needs help drafting a message and wants to review/edit it before finalizing the commit.
---

# Git Commit Message

This skill helps you generate a structured, informative git commit message based on the current staged changes and opens it in the user's preferred editor for final review and manual commitment.

## Workflow

When a user asks to commit their changes or generate a commit message, follow these steps:

1.  **Check Staged Changes**: Run `git diff --cached` to see what changes are being committed.
2.  **Generate Message**: Analyze the diff and draft a concise but descriptive commit message following project conventions (or standard "feat:", "fix:", "refactor:" prefixes).
3.  **Commit with Editor**: Use the `scripts/prepare_commit_msg.sh` script to launch `git commit` with the generated message pre-filled in the user's editor.
4.  **Completion**: The user reviews and finishes the commit in their editor.
    - **CRITICAL**: If the `prepare_commit_msg.sh` script fails, is interrupted, or the editor process is canceled by the user, **DO NOT** attempt to commit the changes using `git commit -m`. Inform the user that the commit was aborted.

### Example Usage

If the user says "Commit these changes for me", you should:
- Read staged changes.
- Generate a message.
- Run `bash scripts/prepare_commit_msg.sh "The generated message"`.
- Tell the user: "I've started the commit process and pre-filled the message in your editor. Please review and save to complete the commit."

## Resources

### scripts/

- `prepare_commit_msg.sh`: Takes a commit message as an argument and runs `git commit -e -m` to open it in the system's `$EDITOR`.
