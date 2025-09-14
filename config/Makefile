# Guile ChangeFlow Project Management Makefile
# For managing documentation and coordination

# Find all org files (excluding .git and .tmp directories)
ORG_FILES := $(shell find . -name "*.org" -not -path "./.git/*" -not -path "./.tmp/*")

.PHONY: help status agenda agents monitor issues setup clean lint lint-org lint-scheme \
        ci-status ci-check ci-logs ci-runs ci-branch-sync ci-fix \
        a1 a2 a3 a4 a5 test-integration quick-status reports

help:
	@echo "Guile ChangeFlow Project Management"
	@echo "===================================="
	@echo ""
	@echo "Documentation:"
	@echo "  status    - Generate project status reports"
	@echo "  agenda    - Open Emacs org-agenda"
	@echo "  lint      - Lint all files (org + scheme)"
	@echo "  lint-org  - Lint org documentation files"
	@echo "  lint-scheme - Lint Scheme source files (when they exist)"
	@echo ""
	@echo "Agent Management:"
	@echo "  agents    - List all agent worktrees"
	@echo "  monitor   - Monitor agent progress"
	@echo "  sessions  - Create tmux sessions for agents"
	@echo "  a1-a5     - Attach to specific agent session"
	@echo ""
	@echo "CI/CD:"
	@echo "  ci-status - Show recent CI workflow runs"
	@echo "  ci-check  - Check if CI is passing on main"
	@echo "  ci-logs   - View logs of latest CI run"
	@echo "  ci-runs   - List all recent workflow runs"
	@echo "  ci-branch-sync - Trigger branch sync check"
	@echo "  ci-fix    - Show common CI fixes"
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

# Linting - Main target
lint: lint-org lint-scheme

# Lint org documentation files
lint-org:
	@echo "=== Linting Org Files ==="
	@echo "Found $(words $(ORG_FILES)) org files to check"
	@for file in $(ORG_FILES); do \
		echo "Checking: $$file"; \
		emacs --batch -Q \
			--eval "(require 'org)" \
			--visit "$$file" \
			-f org-lint \
			2>&1 | grep -v "^Loading" | grep -v "^Checking" | grep -v "^Done" || true; \
	done
	@echo "=== Org Lint Complete ==="

# Lint Scheme source files (placeholder for when agents create them)
lint-scheme:
	@echo "=== Linting Scheme Files ==="
	@if [ -z "$$(find . -name '*.scm' -o -name '*.ss' -not -path './.git/*' -not -path './.tmp/*' 2>/dev/null)" ]; then \
		echo "No Scheme files found to lint (yet)"; \
		echo "Agents will create them soon!"; \
	else \
		echo "Found Scheme files:"; \
		find . -name "*.scm" -o -name "*.ss" -not -path "./.git/*" -not -path "./.tmp/*" | while read file; do \
			echo "  Checking: $$file"; \
			guile -L . -c "(use-modules (ice-9 format)) (load \"$$file\")" 2>&1 | grep -E "ERROR|WARNING" || echo "    ✓ Valid"; \
		done; \
	fi
	@echo "=== Scheme Lint Complete ==="

# Agent management
agents:
	@echo "=== Agent Worktrees ==="
	@git worktree list | grep -v main

monitor:
	@./scripts/monitor-agents.sh

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

# CI/CD Management
ci-status:
	@echo "=== CI Workflow Status ==="
	@gh run list --limit 10 --json status,name,conclusion,createdAt \
		| jq -r '.[] | "\(.createdAt | split("T")[0]) \(.status) \(.conclusion // "pending") \(.name)"' \
		| column -t

ci-check:
	@echo "=== Checking CI status on main branch ==="
	@gh run list --branch main --limit 5 --json conclusion,name \
		| jq -r 'map(select(.conclusion == "failure")) | if length > 0 then "❌ CI FAILING: " + (.[0].name) else "✅ CI PASSING" end'

ci-logs:
	@echo "=== Latest CI run logs ==="
	@gh run view --log $$(gh run list --limit 1 --json databaseId --jq '.[0].databaseId')

ci-runs:
	@echo "=== Recent CI runs (use 'gh run view <id>' for details) ==="
	@gh run list --limit 20

ci-branch-sync:
	@echo "=== Branch Sync Status ==="
	@echo "Triggering branch sync check..."
	@gh workflow run branch-sync-check.yml --ref main
	@echo "Check status with: gmake ci-status"

ci-fix:
	@echo "=== Checking for CI issues ==="
	@echo "1. Checking for failing workflows..."
	@gh run list --branch main --status failure --limit 5
	@echo ""
	@echo "2. Common fixes:"
	@echo "   - Missing optional tools: Update .github/workflows/ci.yml"
	@echo "   - Branch out of sync: gmake ci-branch-sync"
	@echo "   - Lint failures: gmake lint"

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