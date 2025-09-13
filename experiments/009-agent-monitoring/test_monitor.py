#!/usr/bin/env python3
"""Quick test of the agent monitoring system"""

import sys
import os
sys.path.append(os.path.dirname(__file__))

from agent_monitor import AgentMonitor

def test_monitor():
    monitor = AgentMonitor()

    print("Testing agent monitoring...")
    print(monitor.dashboard())

    print("\n" + "="*50)

    # Check commit reminders
    reminders = monitor.check_commit_reminders()
    if reminders:
        print(f"Agents needing commit reminders: {reminders}")
    else:
        print("No commit reminders needed right now")

    print("\n" + "="*50)

    # Show detailed state for one agent
    agent_state = monitor.get_agent_state('gcf-a1')
    print(f"Detailed state for {agent_state['name']}:")
    print(f"  Creating files: {agent_state['state']['creating_files']}")
    print(f"  Has errors: {agent_state['state']['has_errors']}")
    print(f"  Waiting input: {agent_state['state']['waiting_input']}")
    print(f"  Line count: {agent_state['state']['line_count']}")

if __name__ == '__main__':
    test_monitor()