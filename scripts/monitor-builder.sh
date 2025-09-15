#!/usr/bin/env bash
# Builder Agent Monitor - Pokes coordinator every 10 minutes to check on builder
# Usage: ./scripts/monitor-builder.sh &

set -e

LOG_FILE="logs/builder-monitor.log"
COORDINATOR_SESSION="coordinator"
BUILDER_SESSION="builder"
CHECK_INTERVAL=600  # 10 minutes in seconds

# Ensure logs directory exists
mkdir -p logs

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG_FILE"
}

check_sessions() {
    if ! tmux has-session -t "$COORDINATOR_SESSION" 2>/dev/null; then
        log "❌ Coordinator session not found"
        return 1
    fi

    if ! tmux has-session -t "$BUILDER_SESSION" 2>/dev/null; then
        log "❌ Builder session not found"
        return 1
    fi

    return 0
}

capture_builder_pane() {
    # Capture the full builder pane content
    local builder_content
    builder_content=$(tmux capture-pane -t "$BUILDER_SESSION" -p)
    echo "$builder_content"
}

ping_coordinator() {
    local builder_content="$1"
    local timestamp=$(date '+%H:%M:%S')

    # Send monitoring ping to coordinator
    tmux send-keys -t "$COORDINATOR_SESSION" "" C-m
    tmux send-keys -t "$COORDINATOR_SESSION" "# ⏰ MONITOR PING [$timestamp] - Builder Status Check" C-m
    tmux send-keys -t "$COORDINATOR_SESSION" "# Builder pane content:" C-m

    # Send builder content in chunks to avoid overwhelming
    echo "$builder_content" | head -20 | while IFS= read -r line; do
        tmux send-keys -t "$COORDINATOR_SESSION" "# > $line" C-m
        sleep 0.1
    done

    tmux send-keys -t "$COORDINATOR_SESSION" "# Review builder progress and assist if needed" C-m
    tmux send-keys -t "$COORDINATOR_SESSION" "" C-m
}

main() {
    log "🤖 Starting builder monitor (checking every ${CHECK_INTERVAL}s)"

    while true; do
        if check_sessions; then
            log "📊 Checking builder progress..."

            # Capture builder pane
            builder_content=$(capture_builder_pane)

            # Count lines to see if builder is active
            line_count=$(echo "$builder_content" | wc -l)
            log "Builder pane has $line_count lines"

            # Ping coordinator with builder status
            ping_coordinator "$builder_content"
            log "🔔 Sent status update to coordinator"
        else
            log "⚠️ Sessions not available, waiting..."
        fi

        log "💤 Sleeping for $CHECK_INTERVAL seconds..."
        sleep "$CHECK_INTERVAL"
    done
}

# Handle cleanup on exit
cleanup() {
    log "🛑 Monitor shutting down"
    exit 0
}

trap cleanup SIGINT SIGTERM

# Start monitoring
main