#!/bin/bash

# Two-hour monitoring loop for agent coordination
# Runs every 5 minutes and logs progress

LOGDIR="/home/dsp-dr/ghq/github.com/dsp-dr/guile-changeflow/logs"
LOGFILE="$LOGDIR/monitor-loop.log"
END_TIME=$(($(date +%s) + 7200))  # 2 hours from now

mkdir -p "$LOGDIR"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOGFILE"
}

check_commits() {
    local total=0
    for dir in gcf-core-models gcf-mcp-server gcf-risk-engine gcf-web-interface gcf-integrations; do
        cd ../$dir 2>/dev/null || continue
        commits=$(git log --oneline --since="2 hours ago" 2>/dev/null | wc -l)
        total=$((total + commits))
    done
    cd /home/dsp-dr/ghq/github.com/dsp-dr/guile-changeflow
    echo $total
}

check_files() {
    local total=0
    for dir in gcf-core-models gcf-mcp-server gcf-risk-engine gcf-web-interface gcf-integrations; do
        files=$(find ../$dir/src -name "*.scm" 2>/dev/null | wc -l)
        total=$((total + files))
    done
    echo $total
}

monitor_cycle() {
    local cycle=$1
    log "=== Monitoring Cycle $cycle ==="

    # Check coordinator
    if tmux has-session -t gcf-coordinator 2>/dev/null; then
        log "Coordinator: ✅ Active"
    else
        log "Coordinator: ❌ Not found"
    fi

    # Check development agents
    active_agents=0
    for agent in gcf-a1 gcf-a2 gcf-a3 gcf-a4 gcf-a5; do
        if tmux has-session -t $agent 2>/dev/null; then
            ((active_agents++))
        fi
    done
    log "Development Agents: $active_agents/5 active"

    # Check progress
    total_files=$(check_files)
    total_commits=$(check_commits)
    log "Files Created: $total_files"
    log "Recent Commits: $total_commits"

    # Alert if no commits after 30 minutes
    if [[ $cycle -gt 6 ]] && [[ $total_commits -eq 0 ]]; then
        log "⚠️ WARNING: No commits after $((cycle * 5)) minutes!"
    fi

    # Success indicators
    if [[ $total_files -gt 25 ]] && [[ $total_commits -gt 0 ]]; then
        log "🎯 SUCCESS: System productive with files and commits"
    elif [[ $total_files -gt 20 ]]; then
        log "📊 PROGRESS: Good file creation, awaiting commits"
    else
        log "🔄 BUILDING: System still in early phase"
    fi

    log ""
}

# Main loop
log "Starting 2-hour monitoring loop"
cycle=1

while [[ $(date +%s) -lt $END_TIME ]]; do
    monitor_cycle $cycle

    # Every 15 minutes, prod the coordinator
    if [[ $((cycle % 3)) -eq 0 ]]; then
        log "Sending coordinator reminder..."
        tmux send-keys -t gcf-coordinator "Time to check on all agents and ensure they're making progress. Run the monitoring dashboard." Enter 2>/dev/null
    fi

    ((cycle++))

    # Wait 5 minutes
    sleep 300
done

log "Monitoring loop completed after 2 hours"
log "Final Statistics:"
monitor_cycle "FINAL"