#!/bin/sh
# Monitor all agent progress
# Run this to see what all agents are doing

check_agent() {
    session=$1
    worktree=$2
    echo "=== Agent $session ($(basename $worktree)) ==="

    # Check if session exists
    if tmux has-session -t $session 2>/dev/null; then
        echo "Session active"
        # Capture last 10 lines of output
        tmux capture-pane -t $session -p | tail -10
    else
        echo "Session not active"
    fi

    # Check git status in worktree
    if [ -d "$worktree" ]; then
        echo "Git status:"
        cd "$worktree" && git status --short
        # Check for STATUS.org
        if [ -f "$worktree/STATUS.org" ]; then
            echo "Status file:"
            head -15 "$worktree/STATUS.org"
        fi
    fi
    echo "----------------------------------------"
    echo ""
}

while true; do
    clear
    echo "GUILE CHANGEFLOW - AGENT MONITOR"
    echo "================================="
    date
    echo ""

    check_agent gcf-a1 /home/dsp-dr/ghq/github.com/dsp-dr/gcf-core-models
    check_agent gcf-a2 /home/dsp-dr/ghq/github.com/dsp-dr/gcf-mcp-server
    check_agent gcf-a3 /home/dsp-dr/ghq/github.com/dsp-dr/gcf-risk-engine
    check_agent gcf-a4 /home/dsp-dr/ghq/github.com/dsp-dr/gcf-web-interface
    check_agent gcf-a5 /home/dsp-dr/ghq/github.com/dsp-dr/gcf-integrations

    echo "Press Ctrl-C to exit"
    sleep 30
done