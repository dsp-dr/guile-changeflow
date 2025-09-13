#!/usr/bin/env bash
# Sync all agent branches with main before starting agent work

set -euo pipefail

echo "=== Agent Branch Synchronization ==="
echo "Merging latest main into all agent branches"
echo "Time: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track results
SYNCED=()
FAILED=()
NOT_FOUND=()

# Agent branches to sync
BRANCHES=(
  "agent-001-core-system"
  "agent-002-testing"
  "agent-003-documentation"
  "agent-004-integration"
  "agent-005-deployment"
)

# Ensure we're in a git repo
if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    echo -e "${RED}Error: Not in a git repository${NC}"
    exit 1
fi

# Save current branch
ORIGINAL_BRANCH=$(git branch --show-current)

# Fetch latest
echo "Fetching latest changes from origin..."
git fetch origin

echo ""
echo "Starting branch synchronization..."
echo "================================="

for branch in "${BRANCHES[@]}"; do
    echo ""
    echo "Processing: $branch"

    # Check if branch exists on remote
    if ! git show-ref --verify --quiet "refs/remotes/origin/$branch"; then
        echo -e "${YELLOW}  ⚠ Branch not found on remote${NC}"
        NOT_FOUND+=("$branch")
        continue
    fi

    # Check how far behind the branch is
    BEHIND=$(git rev-list --count origin/$branch..origin/main)

    if [ "$BEHIND" -eq 0 ]; then
        echo -e "${GREEN}  ✓ Already up to date${NC}"
        SYNCED+=("$branch (already synced)")
        continue
    fi

    echo "  Branch is $BEHIND commits behind main"
    echo "  Attempting to merge main..."

    # Try to merge main into the branch
    if git checkout "$branch" 2>/dev/null; then
        if git merge origin/main --no-edit -m "merge: sync with main branch before agent work

Automated merge to ensure agent branches have latest changes
from main before parallel development begins.

Co-Authored-By: GitHub Actions <actions@github.com>"; then
            echo -e "${GREEN}  ✓ Successfully merged main${NC}"

            # Push the changes
            if git push origin "$branch"; then
                echo -e "${GREEN}  ✓ Pushed to remote${NC}"
                SYNCED+=("$branch (merged $BEHIND commits)")
            else
                echo -e "${RED}  ✗ Failed to push${NC}"
                FAILED+=("$branch (merge succeeded, push failed)")
            fi
        else
            echo -e "${RED}  ✗ Merge failed (conflicts likely)${NC}"
            FAILED+=("$branch (merge conflicts)")

            # Abort the merge
            git merge --abort 2>/dev/null || true
        fi
    else
        echo -e "${RED}  ✗ Failed to checkout branch${NC}"
        FAILED+=("$branch (checkout failed)")
    fi
done

# Return to original branch
echo ""
echo "Returning to original branch: $ORIGINAL_BRANCH"
git checkout "$ORIGINAL_BRANCH" 2>/dev/null || git checkout main

# Print summary
echo ""
echo "================================="
echo "Synchronization Summary"
echo "================================="

if [ ${#SYNCED[@]} -gt 0 ]; then
    echo -e "${GREEN}✓ Successfully synced (${#SYNCED[@]}):${NC}"
    printf '  - %s\n' "${SYNCED[@]}"
fi

if [ ${#FAILED[@]} -gt 0 ]; then
    echo ""
    echo -e "${RED}✗ Failed to sync (${#FAILED[@]}):${NC}"
    printf '  - %s\n' "${FAILED[@]}"
fi

if [ ${#NOT_FOUND[@]} -gt 0 ]; then
    echo ""
    echo -e "${YELLOW}⚠ Not found (${#NOT_FOUND[@]}):${NC}"
    printf '  - %s\n' "${NOT_FOUND[@]}"
fi

echo ""
echo "================================="

# Exit with error if any branches failed
if [ ${#FAILED[@]} -gt 0 ]; then
    echo -e "${RED}Some branches failed to sync. Manual intervention may be required.${NC}"
    exit 1
else
    echo -e "${GREEN}All existing branches are now synchronized with main!${NC}"
    echo "Agents can begin parallel development."
    exit 0
fi