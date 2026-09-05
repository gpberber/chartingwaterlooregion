# Git hooks

Git does not commit hook files, so each clone installs them once:

```bash
cp _dev/hooks/pre-commit .git/hooks/pre-commit
```

The hook runs `_dev/check_repo_safety.sh` before every commit, whether the commit
comes from Claude, the terminal, or the RStudio Git pane. It blocks files over
25 MB, secrets files, key-like strings, and background reading folders.
