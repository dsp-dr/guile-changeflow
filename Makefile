# Guile ChangeFlow Project Management Makefile
# For managing documentation and coordination

.PHONY: help status agenda agents monitor issues setup clean

help:
	@echo "Guile ChangeFlow Project Management"
	@echo "===================================="
	@echo ""
	@echo "Documentation:"
	@echo "  status    - Generate project status reports"
	@echo "  agenda    - Open Emacs org-agenda"
	@echo ""
	@echo "Agent Management:"
	@echo "  agents    - List all agent worktrees"
	@echo "  monitor   - Monitor agent progress"
	@echo "  sessions  - Create tmux sessions for agents"
	@echo ""
	@echo "GitHub:"
	@echo "  issues    - List GitHub issues"
	@echo "  issue     - Create new issue (TITLE=... BODY=...)"
	@echo ""
	@echo "Setup:"
	@echo "  setup     - Initial project setup"
	@echo "  clean     - Clean generated files"

# Documentation targets
status:
	@echo "Generating project status reports..."
	@emacs --batch -l project-status.el 2>/dev/null
	@echo "Generated: PROJECT-STATUS.org and AGENT-STATUS.org"

agenda:
	@emacs -l guile-changeflow.el -f org-agenda

# Agent management
agents:
	@echo "=== Agent Worktrees ==="
	@git worktree list | grep -v main

monitor:
	@./monitor-agents.sh

sessions:
	@echo "Creating tmux sessions for agents..."
	@tmux new-session -d -s gcf-a1 -c ../gcf-core-models 2>/dev/null || echo "gcf-a1 exists"
	@tmux new-session -d -s gcf-a2 -c ../gcf-mcp-server 2>/dev/null || echo "gcf-a2 exists"
	@tmux new-session -d -s gcf-a3 -c ../gcf-risk-engine 2>/dev/null || echo "gcf-a3 exists"
	@tmux new-session -d -s gcf-a4 -c ../gcf-web-interface 2>/dev/null || echo "gcf-a4 exists"
	@tmux new-session -d -s gcf-a5 -c ../gcf-integrations 2>/dev/null || echo "gcf-a5 exists"
	@echo "Sessions created. Use 'tmux ls' to list."

# GitHub integration
issues:
	@gh issue list

issue:
	@if [ -z "$(TITLE)" ]; then \
		echo "Usage: gmake issue TITLE=\"...\" BODY=\"...\""; \
	else \
		gh issue create --title "$(TITLE)" --body "$(BODY)"; \
	fi

# Project setup
setup: setup-forge setup-hooks

setup-forge:
	@mkdir -p .forge 2>/dev/null || true
	@echo "/.forge/" >> .gitignore
	@echo "Forge directory created (git-ignored)"

setup-hooks:
	@cp tools/hooks/pre-commit .git/hooks/ 2>/dev/null || true
	@chmod +x .git/hooks/pre-commit 2>/dev/null || true
	@echo "Git hooks installed"

# Clean up
clean:
	@rm -f PROJECT-STATUS.org AGENT-STATUS.org agenda.txt todo-stats.txt
	@echo "Cleaned generated files"

# Development shortcuts
a1:
	@tmux attach -t gcf-a1

a2:
	@tmux attach -t gcf-a2

a3:
	@tmux attach -t gcf-a3

a4:
	@tmux attach -t gcf-a4

a5:
	@tmux attach -t gcf-a5

# Integration testing
test-integration:
	@echo "Running integration tests..."
	@cd experiments/000-core-tools && gmake test

# Quick status check
quick-status:
	@echo "=== Quick Status ==="
	@echo "Worktrees: $$(git worktree list | wc -l)"
	@echo "Branches: $$(git branch -a | grep feat/ | wc -l)"
	@echo "Sessions: $$(tmux ls 2>/dev/null | grep gcf- | wc -l || echo 0)"
	@echo "Commits: $$(git log --oneline | wc -l)"

# Generate all reports
reports: status
	@emacs --batch -l guile-changeflow.el \
		--eval "(guile-changeflow-export-todo-stats)" 2>/dev/null
	@echo "All reports generated"