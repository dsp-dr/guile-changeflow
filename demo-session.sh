#!/bin/bash
# demo-session.sh - Create tmux session for deployment pipeline demo

set -e

SESSION_NAME="itil-deployment-demo"
DEMO_DIR="/home/jwalsh/ghq/github.com/dsp-dr/guile-changeflow"

echo "🎬 Setting up ITIL Deployment Pipeline Demo Session"
echo "================================================="

# Kill existing session if it exists
if tmux has-session -t $SESSION_NAME 2>/dev/null; then
    echo "📺 Killing existing session..."
    tmux kill-session -t $SESSION_NAME
fi

echo "🚀 Creating new tmux session: $SESSION_NAME"

# Create main session
cd $DEMO_DIR
tmux new-session -d -s $SESSION_NAME -x 120 -y 40

# Set up the main window for the demo
tmux send-keys -t $SESSION_NAME "clear" Enter
tmux send-keys -t $SESSION_NAME "echo '🎯 ITIL Deployment Pipeline Demo Setup'" Enter
tmux send-keys -t $SESSION_NAME "echo '======================================'" Enter
tmux send-keys -t $SESSION_NAME "echo ''" Enter
tmux send-keys -t $SESSION_NAME "pwd" Enter
tmux send-keys -t $SESSION_NAME "echo ''" Enter

# Create additional windows

# Window 1: Simulator Console
tmux new-window -t $SESSION_NAME -n "simulator"
tmux send-keys -t $SESSION_NAME:simulator "clear" Enter
tmux send-keys -t $SESSION_NAME:simulator "echo '🔧 Deployment Pipeline Simulator Ready'" Enter
tmux send-keys -t $SESSION_NAME:simulator "echo 'Run: guile -L src demo-deployment-pipeline.scm'" Enter

# Window 2: System Monitor
tmux new-window -t $SESSION_NAME -n "monitor"
tmux send-keys -t $SESSION_NAME:monitor "clear" Enter
tmux send-keys -t $SESSION_NAME:monitor "echo '📊 System Monitor'" Enter
tmux send-keys -t $SESSION_NAME:monitor "echo 'Watching for deployment activity...'" Enter
tmux send-keys -t $SESSION_NAME:monitor "watch -n 2 'date; echo; ps aux | grep guile | head -5'" Enter

# Window 3: Logs
tmux new-window -t $SESSION_NAME -n "logs"
tmux send-keys -t $SESSION_NAME:logs "clear" Enter
tmux send-keys -t $SESSION_NAME:logs "echo '📝 Deployment Logs'" Enter
tmux send-keys -t $SESSION_NAME:logs "echo 'Deployment activity will appear here...'" Enter

# Window 4: ITIL Change Requests
tmux new-window -t $SESSION_NAME -n "change-requests"
tmux send-keys -t $SESSION_NAME:change-requests "clear" Enter
tmux send-keys -t $SESSION_NAME:change-requests "echo '📋 ITIL Change Requests'" Enter
tmux send-keys -t $SESSION_NAME:change-requests "echo 'Generated change requests will be tracked here'" Enter
tmux send-keys -t $SESSION_NAME:change-requests "ls -la CHANGE-REQUEST*.org" Enter

# Go back to main window
tmux select-window -t $SESSION_NAME:0

echo "✅ Session created successfully!"
echo ""
echo "To attach to the session:"
echo "  tmux attach-session -t $SESSION_NAME"
echo ""
echo "To start recording with asciicinema:"
echo "  asciicinema rec deployment-demo.cast"
echo ""
echo "To run the demo:"
echo "  guile -L src demo-deployment-pipeline.scm"
echo ""
echo "Windows created:"
echo "  0: main       - Main demo interface"
echo "  1: simulator  - Run the deployment simulator"
echo "  2: monitor    - System monitoring"
echo "  3: logs       - Deployment logs"
echo "  4: change-requests - ITIL change tracking"
echo ""
echo "🎬 Ready to start the demo!"

# Start the session in the background
echo "Starting session..."
tmux attach-session -t $SESSION_NAME