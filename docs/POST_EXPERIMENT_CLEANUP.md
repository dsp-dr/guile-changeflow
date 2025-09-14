# Post Multi-Agent Experiment Cleanup Guide

## Current Situation (1 AM, Sep 14, 2025)
- 5 AI agents (gcf-a1 through gcf-a5) are stuck on permission prompts
- Coordinator and monitor sessions still running but inactive
- Task agent successfully built the entire system (now v1.0.1 in production)
- Multiple worktrees and branches created but unused

## What Can Be Safely Removed

### 1. Tmux Sessions (7 total)
All agent sessions are stuck and can be killed:
```bash
# Kill all agent sessions
tmux kill-session -t gcf-a1
tmux kill-session -t gcf-a2
tmux kill-session -t gcf-a3
tmux kill-session -t gcf-a4
tmux kill-session -t gcf-a5
tmux kill-session -t gcf-coordinator
tmux kill-session -t gcf-monitor

# Or kill all at once
tmux kill-server
```

### 2. Git Worktrees (5 feature worktrees)
These were created for parallel development but agents never used them:
```bash
# Remove all feature worktrees
git worktree remove /home/dsp-dr/ghq/github.com/dsp-dr/gcf-core-models
git worktree remove /home/dsp-dr/ghq/github.com/dsp-dr/gcf-integrations
git worktree remove /home/dsp-dr/ghq/github.com/dsp-dr/gcf-mcp-server
git worktree remove /home/dsp-dr/ghq/github.com/dsp-dr/gcf-risk-engine
git worktree remove /home/dsp-dr/ghq/github.com/dsp-dr/gcf-web-interface

# Clean up worktree references
git worktree prune
```

### 3. Feature Branches
These branches have no meaningful work (agents got stuck):
```bash
# Delete local branches
git branch -D feat/core-models
git branch -D feat/integrations
git branch -D feat/mcp-server
git branch -D feat/risk-engine
git branch -D feat/web-interface
git branch -D agent-1-core
git branch -D feat/cloudflare-mcp-implementation
git branch -D integration

# Delete remote branches (if pushed)
git push origin --delete feat/core-models
git push origin --delete feat/integrations
git push origin --delete feat/mcp-server
git push origin --delete feat/risk-engine
git push origin --delete feat/web-interface
```

### 4. Background Processes
Check for stuck Python monitoring scripts:
```bash
# Find and kill monitoring scripts
ps aux | grep agent-monitor | grep -v grep
ps aux | grep coordinator | grep -v grep

# Kill any found processes
pkill -f agent-monitor
pkill -f coordinator
```

### 5. Temporary Files
```bash
# Clean up any agent artifacts
rm -f /tmp/gcf-agent-*
rm -f /tmp/claude-*
rm -rf ~/.claude/projects/*guile-changeflow*/agent-*
```

## What to Keep

### DO NOT DELETE
- `main` branch - has all the working code
- `/home/dsp-dr/ghq/github.com/dsp-dr/guile-changeflow` - main repository
- Production deployment configuration
- GitHub secrets (CLOUDFLARE_API_TOKEN, CLOUDFLARE_ACCOUNT_ID)

## Quick Cleanup Script

Save as `cleanup-experiment.sh`:
```bash
#!/usr/bin/env bash

echo "🧹 Cleaning up multi-agent experiment artifacts..."

# 1. Kill tmux sessions
echo "Killing tmux sessions..."
for session in gcf-a1 gcf-a2 gcf-a3 gcf-a4 gcf-a5 gcf-coordinator gcf-monitor; do
    tmux kill-session -t $session 2>/dev/null && echo "  Killed $session"
done

# 2. Remove worktrees
echo "Removing git worktrees..."
for worktree in gcf-core-models gcf-integrations gcf-mcp-server gcf-risk-engine gcf-web-interface; do
    path="/home/dsp-dr/ghq/github.com/dsp-dr/$worktree"
    if [ -d "$path" ]; then
        git worktree remove "$path" && echo "  Removed $worktree"
    fi
done
git worktree prune

# 3. Delete local branches
echo "Deleting local branches..."
for branch in feat/core-models feat/integrations feat/mcp-server feat/risk-engine feat/web-interface agent-1-core feat/cloudflare-mcp-implementation integration; do
    git branch -D $branch 2>/dev/null && echo "  Deleted $branch"
done

# 4. Kill background processes
echo "Killing background processes..."
pkill -f agent-monitor 2>/dev/null && echo "  Killed agent-monitor"
pkill -f coordinator 2>/dev/null && echo "  Killed coordinator"

echo "✅ Cleanup complete!"
echo ""
echo "Remaining:"
git branch
echo ""
git worktree list
```

## Verification After Cleanup

Run these to confirm cleanup:
```bash
# Should show no sessions
tmux ls

# Should only show main branch
git branch

# Should only show main repository
git worktree list

# Should show clean working directory
git status
```

## Why This Cleanup is Safe

1. **Agents Never Produced Code**: Stuck on permission prompts since 9:41 PM
2. **Task Agent Built Everything**: The working system is already in production
3. **Branches Are Empty**: No meaningful commits on feature branches
4. **Worktrees Unused**: Agents never navigated to their worktrees
5. **Production is Live**: v1.0.1 deployed with all 8 ITIL tools

## Lessons Learned

The multi-agent experiment revealed:
- Agents got stuck on filesystem permission prompts
- Task agent (single focused agent) was more effective
- Parallel development requires better agent coordination
- Simple, focused approaches work better than complex orchestration

## Post-Cleanup State

After cleanup you'll have:
- Clean main branch with working code
- Single repository (no worktrees)
- No background processes
- No tmux sessions
- Production running v1.0.1 at api.changeflow.us