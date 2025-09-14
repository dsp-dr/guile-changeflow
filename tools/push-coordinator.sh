#!/usr/bin/env bash
# Coordinator Pusher Script - Keeps agents working
# Run this when agents get stuck or need motivation

set -e

echo "🚀 Pushing Coordinator and Agents to Continue Working"
echo "======================================================"

# Function to check if session exists
session_exists() {
    tmux has-session -t "$1" 2>/dev/null
}

# Function to send wake-up call to a session
wake_agent() {
    local session=$1
    local message=$2
    
    if session_exists "$session"; then
        echo "📢 Waking $session: $message"
        tmux send-keys -t "$session" C-c
        sleep 0.5
        tmux send-keys -t "$session" "$message" Enter
    else
        echo "❌ Session $session not found"
    fi
}

# Check current agent status
echo ""
echo "Current Agent Status:"
echo "--------------------"
for session in gcf-coordinator gcf-a1 gcf-a2 gcf-a3 gcf-a4 gcf-a5; do
    if session_exists "$session"; then
        status=$(tmux capture-pane -t "$session" -p | tail -3 | grep -E "(Osmosing|Beboppin|✢|✽|●)" || echo "Idle")
        echo "$session: Active ✅"
    else
        echo "$session: Not running ❌"
    fi
done

# Push coordinator to check agents
echo ""
echo "Pushing Coordinator:"
echo "-------------------"
wake_agent "gcf-coordinator" "Check all agents and ensure they're working. Run python3 tools/agent-monitor-dashboard.py to see status. The demo is at 7 AM."

# Push each agent with specific reminders
echo ""
echo "Pushing Individual Agents:"
echo "-------------------------"

wake_agent "gcf-a1" "Continue building SQLite schema. Read src/database/schema.sql for existing work. Add any missing ITIL tables. Test with demo-validation.scm"

wake_agent "gcf-a2" "Implement Workers Logs as described in experiments/010-workers-logs/README.md. Update cloudflare/worker.js with structured logging."

wake_agent "gcf-a3" "Build risk matrices in src/risk/matrices.scm. Implement freeze periods. Test with battle scenarios."

wake_agent "gcf-a4" "Create executive dashboard in web/dashboard.html. Show real-time metrics and ROI of \$4.7M. Include change simulator."

wake_agent "gcf-a5" "Run integration tests from test/battle-test.scm. Validate all components. Ensure 99.97% uptime target."

echo ""
echo "✅ All agents pushed!"
echo ""
echo "Monitor progress with:"
echo "  python3 tools/agent-monitor-dashboard.py"
echo ""
echo "Check individual agents with:"
echo "  tmux attach -t gcf-a1  (Core Models)"
echo "  tmux attach -t gcf-a2  (MCP/Cloudflare)"
echo "  tmux attach -t gcf-a3  (Risk Engine)"
echo "  tmux attach -t gcf-a4  (Web/Dashboard)"
echo "  tmux attach -t gcf-a5  (Integration)"
echo "  tmux attach -t gcf-coordinator  (Coordinator)"
echo ""
echo "Time remaining to 7 AM demo: $(python3 -c "from datetime import datetime, timedelta; td = datetime(2025, 9, 14, 7, 0) - datetime.now(); print(f'{td.total_seconds() / 3600:.1f} hours')")"