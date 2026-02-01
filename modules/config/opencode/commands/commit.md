---
name: commit
description: Automatically generates a commit message using the git-commit-message skill and opens the editor to commit.
---

# Commit Command

When this command is invoked, follow these steps:

1.  **Invoke Skill**: Call the `git-commit-message` skill.
2.  **Follow Skill Workflow**:
    - Run `git diff --cached` to analyze changes.
    - Generate a descriptive commit message.
    - Execute the `prepare_commit_msg.sh` script from the skill to open the editor.
3.  **Finalize**: Notify the user that the commit process has started in their editor.
