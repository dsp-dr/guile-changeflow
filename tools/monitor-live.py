#!/usr/bin/env python3
"""
Live monitoring of agent coordination
Checks if coordinator is driving other agents
"""

import subprocess
import time
import json
from datetime import datetime
from pathlib import Path

def capture_tmux_session(session_name):
    """Capture the current state of a tmux session"""
    try:
        result = subprocess.run(
            ['tmux', 'capture-pane', '-t', session_name, '-p'],
            capture_output=True,
            text=True,
            timeout=5
        )
        return result.stdout
    except:
        return ""

def check_git_activity(worktree_name):
    """Check recent git commits in a worktree"""
    try:
        worktree_path = Path.home() / "ghq/github.com/dsp-dr" / worktree_name
        result = subprocess.run(
            ['git', 'log', '--oneline', '--since=1 hour ago'],
            cwd=worktree_path,
            capture_output=True,
            text=True,
            timeout=5
        )
        return len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0
    except:
        return -1

def check_files_created(worktree_name):
    """Count Scheme files in a worktree"""
    try:
        worktree_path = Path.home() / "ghq/github.com/dsp-dr" / worktree_name
        src_path = worktree_path / "src"
        if src_path.exists():
            scm_files = list(src_path.rglob("*.scm"))
            return len(scm_files)
        return 0
    except:
        return -1

def monitor_system():
    """Main monitoring function"""
    agents = [
        ('gcf-a1', 'gcf-core-models', 'Core Models'),
        ('gcf-a2', 'gcf-mcp-server', 'MCP Server'),
        ('gcf-a3', 'gcf-risk-engine', 'Risk Engine'),
        ('gcf-a4', 'gcf-web-interface', 'Web Interface'),
        ('gcf-a5', 'gcf-integrations', 'Integrations'),
    ]

    print(f"\n{'='*80}")
    print(f"AGENT COORDINATION STATUS - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"{'='*80}\n")

    # Check coordinator
    print("COORDINATOR STATUS:")
    coord_output = capture_tmux_session('gcf-coordinator')
    coord_lines = coord_output.split('\n')

    # Look for activity indicators
    is_active = False
    if 'Running' in coord_output or 'Checking' in coord_output:
        is_active = True
        print("  ✅ Coordinator is ACTIVE - running monitoring tasks")
    elif 'Do you want' in coord_output:
        print("  ⏳ Coordinator waiting for permission")
    else:
        print("  🔍 Coordinator status unclear")

    # Show last meaningful line from coordinator
    meaningful_lines = [l.strip() for l in coord_lines
                       if l.strip() and not l.startswith('│') and not l.startswith('╭')]
    if meaningful_lines:
        print(f"  Last action: {meaningful_lines[-1][:60]}")

    print(f"\n{'─'*80}\n")

    # Check each development agent
    print("DEVELOPMENT AGENTS:")

    total_files = 0
    total_commits = 0
    agents_active = 0

    for session, worktree, name in agents:
        output = capture_tmux_session(session)
        files = check_files_created(worktree)
        commits = check_git_activity(worktree)

        status = "❓"
        if 'Creating' in output or 'Implementing' in output:
            status = "🚧"
            agents_active += 1
        elif 'Do you want' in output:
            status = "⏳"
        elif files > 0:
            status = "✅"
            agents_active += 1

        print(f"\n  {session} ({name}):")
        print(f"    Status: {status}")
        print(f"    Files created: {files if files >= 0 else 'N/A'}")
        print(f"    Recent commits: {commits if commits >= 0 else 'N/A'}")

        if files > 0:
            total_files += files
        if commits > 0:
            total_commits += commits

        # Show last activity
        lines = output.split('\n')
        activity_lines = [l.strip() for l in lines
                         if l.strip() and 'Creating' in l or 'Implementing' in l]
        if activity_lines:
            print(f"    Activity: {activity_lines[-1][:50]}...")

    print(f"\n{'─'*80}\n")

    # Summary
    print("SYSTEM SUMMARY:")
    print(f"  Coordinator: {'✅ Active' if is_active else '⏳ Waiting'}")
    print(f"  Agents active: {agents_active}/5")
    print(f"  Total files created: {total_files}")
    print(f"  Total recent commits: {total_commits}")

    # Coordinator effectiveness
    if is_active and agents_active > 3:
        print("\n  🎯 COORDINATOR EFFECTIVENESS: HIGH")
        print("     Coordinator is actively monitoring and agents are producing code")
    elif agents_active > 0:
        print("\n  📊 COORDINATOR EFFECTIVENESS: MODERATE")
        print("     Some agents active, coordinator may need to intervene")
    else:
        print("\n  ⚠️ COORDINATOR EFFECTIVENESS: LOW")
        print("     Coordinator needs to actively guide stuck agents")

    print(f"\n{'='*80}\n")

if __name__ == "__main__":
    # Run once or loop
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == "--loop":
        while True:
            monitor_system()
            print("Next check in 60 seconds... (Ctrl+C to stop)")
            time.sleep(60)
    else:
        monitor_system()