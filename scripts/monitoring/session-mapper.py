#!/usr/bin/env python3
"""
Claude Session Mapper Tool
Maps tmux sessions to Claude project directories and analyzes agent activity
"""

import os
import json
import glob
from pathlib import Path
from datetime import datetime, timezone
from collections import defaultdict

class ClaudeSessionMapper:
    def __init__(self):
        self.claude_dir = Path.home() / ".claude"
        self.projects_dir = self.claude_dir / "projects"
        self.guile_changeflow_sessions = {
            'gcf-a1': {
                'name': 'Core Models',
                'dir': '-home-dsp-dr-ghq-github-com-dsp-dr-gcf-core-models',
                'worktree': 'gcf-core-models',
                'role': 'Data models, state machines, SRFI-9 records'
            },
            'gcf-a2': {
                'name': 'MCP Server',
                'dir': '-home-dsp-dr-ghq-github-com-dsp-dr-gcf-mcp-server',
                'worktree': 'gcf-mcp-server',
                'role': 'HTTP server, JSON-RPC 2.0, MCP protocol'
            },
            'gcf-a3': {
                'name': 'Risk Engine',
                'dir': '-home-dsp-dr-ghq-github-com-dsp-dr-gcf-risk-engine',
                'worktree': 'gcf-risk-engine',
                'role': 'Risk calculation, 0-100 scoring, ITIL factors'
            },
            'gcf-a4': {
                'name': 'Web Interface',
                'dir': '-home-dsp-dr-ghq-github-com-dsp-dr-gcf-web-interface',
                'worktree': 'gcf-web-interface',
                'role': 'Web dashboard, HTTP server port 8080'
            },
            'gcf-a5': {
                'name': 'Integrations',
                'dir': '-home-dsp-dr-ghq-github-com-dsp-dr-gcf-integrations',
                'worktree': 'gcf-integrations',
                'role': 'GitHub/Slack integration, webhooks port 8082'
            },
            'gcf-coordinator': {
                'name': 'Meta-Coordinator',
                'dir': '-home-dsp-dr-ghq-github-com-dsp-dr-guile-changeflow',
                'worktree': 'guile-changeflow',
                'role': 'Monitor and guide 5 development agents'
            }
        }

    def analyze_session(self, session_key):
        """Analyze a specific Claude session's activity"""
        session_info = self.guile_changeflow_sessions.get(session_key)
        if not session_info:
            return None

        project_path = self.projects_dir / session_info['dir']
        if not project_path.exists():
            return {
                'session': session_key,
                'name': session_info['name'],
                'status': 'no_project_dir',
                'files': []
            }

        # Find all JSONL files in the project directory
        jsonl_files = list(project_path.glob("*.jsonl"))

        result = {
            'session': session_key,
            'name': session_info['name'],
            'role': session_info['role'],
            'worktree': session_info['worktree'],
            'project_dir': str(project_path),
            'jsonl_files': len(jsonl_files),
            'conversations': [],
            'last_activity': None,
            'total_messages': 0,
            'tool_usage': defaultdict(int)
        }

        # Analyze each JSONL file
        for jsonl_file in sorted(jsonl_files):
            try:
                conversation_data = self.analyze_jsonl(jsonl_file)
                result['conversations'].append({
                    'file': jsonl_file.name,
                    'messages': conversation_data['message_count'],
                    'last_timestamp': conversation_data['last_timestamp'],
                    'tools_used': conversation_data['tools_used']
                })
                result['total_messages'] += conversation_data['message_count']

                # Track tool usage
                for tool, count in conversation_data['tools_used'].items():
                    result['tool_usage'][tool] += count

                # Update last activity
                if conversation_data['last_timestamp']:
                    if not result['last_activity'] or conversation_data['last_timestamp'] > result['last_activity']:
                        result['last_activity'] = conversation_data['last_timestamp']

            except Exception as e:
                print(f"Error analyzing {jsonl_file}: {e}")

        return result

    def analyze_jsonl(self, jsonl_path):
        """Analyze a single JSONL conversation file"""
        data = {
            'message_count': 0,
            'last_timestamp': None,
            'tools_used': defaultdict(int)
        }

        try:
            with open(jsonl_path, 'r') as f:
                for line in f:
                    if line.strip():
                        try:
                            entry = json.loads(line)
                            data['message_count'] += 1

                            # Track timestamp
                            if 'timestamp' in entry:
                                timestamp = entry['timestamp']
                                if not data['last_timestamp'] or timestamp > data['last_timestamp']:
                                    data['last_timestamp'] = timestamp

                            # Track tool usage
                            if 'content' in entry:
                                content = str(entry['content'])
                                if '<function_calls>' in content:
                                    if 'Bash' in content:
                                        data['tools_used']['Bash'] += 1
                                    if 'Read' in content:
                                        data['tools_used']['Read'] += 1
                                    if 'Write' in content or 'MultiEdit' in content:
                                        data['tools_used']['Write/Edit'] += 1
                                    if 'Grep' in content or 'Glob' in content:
                                        data['tools_used']['Search'] += 1
                                    if 'TodoWrite' in content:
                                        data['tools_used']['TodoWrite'] += 1

                        except json.JSONDecodeError:
                            continue
        except FileNotFoundError:
            pass

        return data

    def get_all_sessions_status(self):
        """Get status of all Guile ChangeFlow sessions"""
        results = []
        for session_key in self.guile_changeflow_sessions.keys():
            session_data = self.analyze_session(session_key)
            if session_data:
                results.append(session_data)
        return results

    def format_report(self):
        """Generate a formatted report of all sessions"""
        sessions = self.get_all_sessions_status()

        report = ["=== Claude Session Mapping Report ==="]
        report.append(f"Generated: {datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M:%S UTC')}")
        report.append("")

        for session in sessions:
            report.append(f"Session: {session['session']} ({session['name']})")
            report.append(f"  Role: {session['role']}")
            report.append(f"  Worktree: ../{session['worktree']}")
            report.append(f"  JSONL Files: {session['jsonl_files']}")
            report.append(f"  Total Messages: {session['total_messages']}")

            if session['last_activity']:
                report.append(f"  Last Activity: {session['last_activity']}")

            if session['tool_usage']:
                report.append(f"  Tool Usage:")
                for tool, count in sorted(session['tool_usage'].items()):
                    report.append(f"    - {tool}: {count}")

            report.append("")

        return "\n".join(report)

    def get_coordinator_activity(self):
        """Specifically analyze coordinator agent activity"""
        coord_data = self.analyze_session('gcf-coordinator')
        if not coord_data:
            return "Coordinator session not found"

        report = ["=== Coordinator Agent Activity ==="]
        report.append(f"Messages: {coord_data['total_messages']}")
        report.append(f"JSONL Files: {coord_data['jsonl_files']}")

        if coord_data['last_activity']:
            report.append(f"Last Active: {coord_data['last_activity']}")

        if coord_data['tool_usage']:
            report.append("Actions Taken:")
            for tool, count in coord_data['tool_usage'].items():
                report.append(f"  - {tool}: {count} times")

        return "\n".join(report)

if __name__ == "__main__":
    mapper = ClaudeSessionMapper()
    print(mapper.format_report())
    print("")
    print(mapper.get_coordinator_activity())