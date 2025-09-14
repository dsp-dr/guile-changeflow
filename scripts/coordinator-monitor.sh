#!/bin/bash

# Meta-coordinator monitoring script
# Runs every 15 minutes to check on development agents and coordinate progress
# Part of the hierarchical multi-agent system

set -euo pipefail

LOGFILE="/home/dsp-dr/ghq/github.com/dsp-dr/guile-changeflow/logs/coordinator-monitor.log"
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S UTC')

# Ensure log directory exists
mkdir -p "$(dirname "$LOGFILE")"

# Log function
log() {
    echo "[$TIMESTAMP] $*" | tee -a "$LOGFILE"
}

# Check if coordinator agent is responsive
check_coordinator() {
    if tmux has-session -t gcf-coordinator 2>/dev/null; then
        log "✅ Coordinator agent session active"
        return 0
    else
        log "❌ Coordinator agent session not found"
        return 1
    fi
}

# Send monitoring prompt to coordinator
send_coordinator_prompt() {
    local cycle_count="$1"

    log "📊 Sending monitoring cycle $cycle_count to coordinator agent"

    tmux send-keys -t gcf-coordinator "
# MONITORING CYCLE $cycle_count - $(date)
# Your task: Check progress of all 5 development agents and provide guidance

Please run your agent monitoring dashboard and check:
1. Agent progress and file creation status
2. Git commit activity (remind agents to commit per AGENT-BEST-PRACTICES.org)
3. Any error states or blockers requiring intervention
4. Integration point readiness between agents
5. Milestone progress toward core logic implementation

For each agent that needs guidance:
- Send specific, helpful messages via tmux
- Reference their role documentation in instructions/<role>.org
- Remind about commit discipline if no recent git activity
- Help resolve any technical blockers

Focus areas by agent:
- gcf-a1 (Core Models): SRFI-9 records, state machines
- gcf-a2 (MCP Server): JSON-RPC 2.0, HTTP endpoints
- gcf-a3 (Risk Engine): 0-100 scoring, ITIL factors
- gcf-a4 (Web Interface): Guile web server, dashboard
- gcf-a5 (Integrations): GitHub webhooks, notifications

After monitoring, provide a brief status summary.
" Enter

    # Small delay to ensure message is processed
    sleep 2
}

# Main monitoring loop
main() {
    log "🤖 Meta-coordinator monitoring script started"
    log "🎯 Target: gcf-coordinator agent session"

    # Read cycle count from state file or start at 1
    local state_file="/tmp/coordinator-monitor-cycle"
    local cycle_count=1

    if [[ -f "$state_file" ]]; then
        cycle_count=$(cat "$state_file")
        ((cycle_count++))
    fi

    echo "$cycle_count" > "$state_file"

    log "📈 Starting monitoring cycle #$cycle_count"

    if check_coordinator; then
        send_coordinator_prompt "$cycle_count"
        log "✅ Monitoring prompt sent successfully"

        # Check if we should also send a keybase update (every hour = every 4 cycles)
        if (( cycle_count % 4 == 0 )); then
            log "📱 Sending hourly keybase update"
            keybase chat send jwalsh "Meta-monitoring cycle #$cycle_count: Coordinator agent prompted to check all 5 development agents. System running autonomously with 15-minute intervention cycles. Next: coordinator will assess progress and provide guidance." || log "⚠️ Keybase send failed"
        fi

    else
        log "❌ Coordinator not responsive - alerting via keybase"
        keybase chat send jwalsh "🚨 ALERT: Coordinator agent (gcf-coordinator) session not found during monitoring cycle #$cycle_count. Meta-monitoring system detected failure. Please check tmux sessions." || log "❌ Emergency keybase send failed"
    fi

    log "✅ Monitoring cycle #$cycle_count completed"
    log "⏰ Next cycle in 15 minutes"

    # Log current system status
    log "📊 System status:"
    log "   - Development agents: 5 (gcf-a1 through gcf-a5)"
    log "   - Coordinator agent: gcf-coordinator"
    log "   - Meta-monitoring: active"
    log "   - Cycle frequency: 15 minutes"
    log "   - Log file: $LOGFILE"
}

# Error handling
trap 'log "❌ Script error at line $LINENO"' ERR

main "$@"