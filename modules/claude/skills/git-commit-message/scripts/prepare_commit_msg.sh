#!/bin/bash

# Check if there are staged changes
if git diff --cached --quiet; then
    echo "Error: No staged changes found. Please stage your changes first."
    exit 1
fi

# Get the commit message from stdin or argument
if [ -n "$1" ]; then
    COMMIT_MSG="$1"
else
    # Read from stdin if no argument
    COMMIT_MSG=$(cat)
fi

# Directly run git commit with the message pre-filled in the editor
if ! git commit -e -m "$COMMIT_MSG"; then
    echo "Error: Commit aborted or failed."
    exit 1
fi
