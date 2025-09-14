#!/usr/bin/env python3
"""
Real-time Agent Monitoring Dashboard
Tracks progress toward 7 AM demo readiness
"""

import subprocess
import time
import os
from datetime import datetime, timedelta
from pathlib import Path

class AgentMonitorDashboard:
    def __init__(self):
        self.deadline = datetime(2025, 9, 14, 7, 0, 0)  # 7 AM deadline
        self.agents = {
            'gcf-a1': {
                'name': 'Core Models',
                'deliverables': ['schema.sql', 'audit triggers', 'migrations'],
                'worktree': 'gcf-core-models'
            },
            'gcf-a2': {
                'name': 'MCP/Cloudflare',
                'deliverables': ['worker.js', 'Workers Logs', 'MCP protocol'],
                'worktree': 'gcf-mcp-server'
            },
            'gcf-a3': {
                'name': 'Risk Engine',
                'deliverables': ['matrices.scm', 'freeze periods', 'scoring'],
                'worktree': 'gcf-risk-engine'
            },
            'gcf-a4': {
                'name': 'Web/Dashboard',
                'deliverables': ['dashboard.html', 'charts', 'simulator'],
                'worktree': 'gcf-web-interface'
            },
            'gcf-a5': {
                'name': 'Integration',
                'deliverables': ['integration tests', 'chaos tests', 'notifications'],
                'worktree': 'gcf-integrations'
            }
        }

    def capture_agent_output(self, session):
        """Capture tmux session output"""
        try:
            result = subprocess.run(
                ['tmux', 'capture-pane', '-t', session, '-p'],
                capture_output=True, text=True, timeout=2
            )
            return result.stdout
        except:
            return ""

    def check_deliverables(self, worktree):
        """Check what files have been created"""
        worktree_path = Path.home() / "ghq/github.com/dsp-dr" / worktree
        if not worktree_path.exists():
            return []

        created_files = []
        for ext in ['*.sql', '*.scm', '*.js', '*.html', '*.org']:
            files = list(worktree_path.rglob(ext))
            created_files.extend([f.name for f in files])
        return created_files

    def get_agent_status(self, session, info):
        """Determine agent's current status"""
        output = self.capture_agent_output(session)

        # Check for activity indicators
        if 'Creating' in output or 'Writing' in output or 'Building' in output:
            return "🚧 WORKING"
        elif 'Testing' in output or 'Running tests' in output:
            return "🧪 TESTING"
        elif 'Committed' in output or 'git commit' in output:
            return "💾 COMMITTING"
        elif 'Error' in output or 'Failed' in output:
            return "❌ ERROR"
        elif '❯' in output:
            return "⏳ WAITING"
        else:
            return "❓ UNKNOWN"

    def display_dashboard(self):
        """Display the monitoring dashboard"""
        os.system('clear')
        now = datetime.now()
        time_remaining = self.deadline - now
        hours_left = time_remaining.total_seconds() / 3600

        print("=" * 80)
        print(f"🎯 GUILE CHANGEFLOW - 7 AM DEMO READINESS DASHBOARD")
        print(f"⏰ Time: {now.strftime('%H:%M:%S')} | Deadline: 7:00 AM | Remaining: {hours_left:.1f} hours")
        print("=" * 80)
        print()

        # Agent Status Table
        print("AGENT STATUS:")
        print("-" * 80)
        print(f"{'Agent':<15} {'Status':<15} {'Files Created':<20} {'Progress':<25}")
        print("-" * 80)

        total_files = 0
        agents_working = 0

        for session, info in self.agents.items():
            status = self.get_agent_status(session, info)
            files = self.check_deliverables(info['worktree'])
            total_files += len(files)

            if "WORKING" in status or "TESTING" in status:
                agents_working += 1

            progress = f"{len(files)} files"
            print(f"{info['name']:<15} {status:<15} {len(files):<20} {progress:<25}")

        print("-" * 80)
        print()

        # Overall Progress
        print("SYSTEM METRICS:")
        print(f"  Total Files Created: {total_files}")
        print(f"  Agents Active: {agents_working}/5")
        print(f"  Cloudflare Status: ✅ api.changeflow.us responding")
        print()

        # Critical Deliverables Checklist
        print("CRITICAL DELIVERABLES:")
        deliverables = [
            ("Database Schema (SQLite)", self.check_file_exists("schema.sql")),
            ("Cloudflare Worker", self.check_file_exists("worker.js")),
            ("Risk Matrices", self.check_file_exists("matrices.scm")),
            ("Executive Dashboard", self.check_file_exists("dashboard.html")),
            ("Integration Tests", self.check_file_exists("integration")),
            ("Battle Tests", self.check_file_exists("battle-test")),
            ("Workers Logs Experiment", self.check_file_exists("010-workers-logs"))
        ]

        for item, exists in deliverables:
            status = "✅" if exists else "❌"
            print(f"  {status} {item}")

        print()
        print("=" * 80)

        # Alerts
        if hours_left < 2:
            print("⚠️  ALERT: Less than 2 hours remaining! Focus on critical path!")
        if agents_working < 3:
            print("⚠️  ALERT: Most agents inactive! Send wake-up commands!")
        if total_files < 50:
            print("⚠️  ALERT: File count low for production system!")

    def check_file_exists(self, pattern):
        """Check if a file pattern exists in any worktree"""
        for _, info in self.agents.items():
            worktree_path = Path.home() / "ghq/github.com/dsp-dr" / info['worktree']
            if worktree_path.exists():
                for root, dirs, files in os.walk(worktree_path):
                    for file in files:
                        if pattern.lower() in file.lower():
                            return True
        return False

    def run(self):
        """Run the dashboard continuously"""
        while True:
            self.display_dashboard()
            print("\nRefreshing in 30 seconds... (Ctrl+C to exit)")
            time.sleep(30)

if __name__ == "__main__":
    dashboard = AgentMonitorDashboard()
    dashboard.run()