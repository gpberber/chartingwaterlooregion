#!/usr/bin/env bash
# check_repo_safety.sh
# Blocks a commit if it would publish something that must stay private or
# something too big for GitHub. Used as the git pre-commit hook (see
# _dev/hooks/README.md) and by the /publish and /save skills.
#
# Checks, in order:
#   1. No staged file larger than MAX_MB (GitHub warns at 50 MB, refuses at 100 MB;
#      we stop well short so the repo stays quick to clone).
#   2. No secrets files staged (.Renviron, .env).
#   3. No strings that look like API keys or tokens in staged content.
#   4. Nothing from a background/ folder (copyrighted reading material).
#
# Exit 0 = safe, exit 1 = blocked (message explains what to fix).

MAX_MB=25
status=0

# --- 1. size check ---------------------------------------------------------
while IFS= read -r file; do
  [ -f "$file" ] || continue
  size_mb=$(( $(wc -c < "$file") / 1024 / 1024 ))
  if [ "$size_mb" -ge "$MAX_MB" ]; then
    echo "BLOCKED: $file is ${size_mb} MB (limit ${MAX_MB} MB)."
    echo "         Use /share-data to put it in a GitHub Release, then git rm --cached it."
    status=1
  fi
done < <(git diff --cached --name-only --diff-filter=AM)

# --- 2. secrets files -------------------------------------------------------
if git diff --cached --name-only | grep -Eq '(^|/)(\.Renviron|\.env)$'; then
  echo "BLOCKED: .Renviron or .env is staged. Keys belong in ~/.Renviron (home folder), never in the repo."
  status=1
fi

# --- 3. secret-looking strings in staged content ----------------------------
# Patterns: GitHub tokens, AWS keys, OpenAI/Anthropic keys, generic key/token assignments.
pattern='ghp_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,}|AKIA[0-9A-Z]{16}|sk-[A-Za-z0-9_-]{20,}|(api[_-]?key|secret|token|password)[[:space:]]*[:=][[:space:]]*["'"'"'][^"'"'"']{8,}'
hits=$(git diff --cached -U0 | grep -E '^\+' | grep -Ei "$pattern" | grep -v 'check_repo_safety' | head -5)
if [ -n "$hits" ]; then
  echo "BLOCKED: staged changes contain text that looks like a key or token:"
  echo "$hits"
  echo "         Move the value to ~/.Renviron and read it with Sys.getenv()."
  status=1
fi

# --- 4. background folders --------------------------------------------------
if git diff --cached --name-only | grep -Eiq '(^|/)background/'; then
  echo "BLOCKED: a background/ folder is staged. Background reading lives outside the repo"
  echo "         at ../chartingwaterlooregion-background/<post-slug>/."
  status=1
fi

if [ "$status" -eq 0 ]; then
  echo "Repo safety check passed."
fi
exit $status
