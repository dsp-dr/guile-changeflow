# Guile ChangeFlow Project Management Makefile
# For managing documentation and coordination

# Find all org files (excluding .git and .tmp directories)
ORG_FILES := $(shell find . -name "*.org" -not -path "./.git/*" -not -path "./.tmp/*")

.PHONY: help status agenda agents monitor issues setup clean lint lint-org lint-scheme \
        ci-status ci-check ci-logs ci-runs ci-branch-sync ci-fix \
        a1 a2 a3 a4 a5 test-integration quick-status reports \
        dev-setup test-mcp mcp-server mcp-stop mcp-status test-guile \
        check-env mcp-server-background test deploy \
        demo-data demo-quick demo-frontend demo-backend demo-infra

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
	@echo "Development:"
	@echo "  dev-setup - Setup local development environment"
	@echo "  mcp-server- Start local MCP server on port 8088"
	@echo "  mcp-stop  - Stop local MCP server"
	@echo "  mcp-status- Check MCP server status"
	@echo "  test-mcp  - Test MCP server functionality"
	@echo "  test-guile- Test Guile module system"
	@echo ""
	@echo "Demo Data:"
	@echo "  demo-data - Generate all demo datasets (data/demo-change-requests.json)"
	@echo "  demo-quick- Run 5-PR quick demo with pre-generated data"
	@echo "  demo-frontend- Run frontend-focused demo (10 PRs)"
	@echo "  demo-backend- Run backend-focused demo (15 PRs)"
	@echo "  demo-infra- Run infrastructure-focused demo (8 PRs)"
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
	@git worktree list | grep -v main || echo "No agent worktrees active"

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

# Development environment setup and MCP server management
dev-setup:
	@echo "=== Setting up development environment ==="
	@echo "1. Checking dependencies..."
	@command -v guile || echo "   Install Guile 3.0+: apt-get install guile-3.0 guile-3.0-dev"
	@command -v node || echo "   Install Node.js: https://nodejs.org/"
	@command -v jq || echo "   Install jq: apt-get install jq"
	@echo "2. Setting up directories..."
	@mkdir -p .claude logs
	@echo "3. Verifying project structure..."
	@[ -f scripts/mcp-local-server.js ] && echo "   ✅ MCP server script found" || echo "   ❌ MCP server script missing"
	@[ -f .claude/mcp_config.json ] && echo "   ✅ MCP config found" || echo "   ❌ MCP config missing"
	@echo "4. Testing environment (use 'direnv allow' for auto-setup)"

# MCP Server Management (using port 8427 - "8 24/7" watching everything!)
mcp-server:
	@echo "Starting local MCP server on port 8427..."
	@if curl -s http://localhost:8427/ >/dev/null 2>&1; then \
		echo "⚠️  Port 8427 already in use"; \
	else \
		nohup node scripts/mcp-local-server.js 8427 > logs/mcp-server.log 2>&1 & \
		echo "📡 MCP server starting (PID: $$!)"; \
		sleep 2; \
		curl -s http://localhost:8427/ | jq . || echo "Server startup may need more time"; \
	fi

mcp-stop:
	@echo "Stopping MCP server..."
	@pkill -f "mcp-local-server.js" || echo "No MCP server process found"
	@echo "✅ MCP server stopped"

mcp-status:
	@echo "=== MCP Server Status ==="
	@if curl -s http://localhost:8427/ >/dev/null 2>&1; then \
		echo "✅ MCP server running on port 8427"; \
		curl -s http://localhost:8427/ | jq '.status, .version' 2>/dev/null || echo "Response received"; \
	else \
		echo "❌ MCP server not responding on port 8427"; \
	fi

# MCP Testing
test-mcp:
	@echo "=== Testing MCP Server ==="
	@echo "1. Health check..."
	@curl -s http://localhost:8427/ | jq . || echo "❌ Health check failed"
	@echo ""
	@echo "2. Tools list..."
	@curl -s -X POST http://localhost:8427/ \
		-H "Content-Type: application/json" \
		-d '{"jsonrpc":"2.0","method":"tools/list","id":1}' \
		| jq '.result.tools | length' 2>/dev/null \
		&& echo "✅ Tools endpoint working" \
		|| echo "❌ Tools endpoint failed"
	@echo ""
	@echo "3. Running comprehensive test suite..."
	@node test/mcp-comprehensive-test.js 2>/dev/null || echo "❌ Test suite failed (check dependencies)"

# Guile Testing
test-guile:
	@echo "=== Testing Guile Environment ==="
	@echo "1. Guile version..."
	@guile --version | head -1
	@echo ""
	@echo "2. Module loading test..."
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -c "(use-modules (ice-9 format)) (display \"Guile modules: OK\\n\")" || \
		echo "❌ Guile module loading failed"
	@echo ""
	@echo "3. Running module test suite..."
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -s test/module-test-simple.scm || \
		echo "❌ Module test suite failed"

# Environment check target for .envrc
check-env:
	@echo "Checking environment..."
	@command -v guile > /dev/null && echo "✅ Guile installed" || (echo "❌ Guile missing" && exit 1)
	@command -v node > /dev/null && echo "✅ Node.js installed" || (echo "❌ Node.js missing" && exit 1)
	@command -v npm > /dev/null && echo "✅ npm installed" || (echo "❌ npm missing" && exit 1)
	@[ -d src ] && echo "✅ src/ directory exists" || (echo "❌ src/ missing" && exit 1)
	@[ -d test ] && echo "✅ test/ directory exists" || (echo "❌ test/ missing" && exit 1)
	@[ -f scripts/mcp-local-server.js ] && echo "✅ MCP server script exists" || (echo "❌ MCP script missing" && exit 1)

# Background MCP server for .envrc
mcp-server-background:
	@mkdir -p logs
	@nohup node scripts/mcp-local-server.js $(MCP_SERVER_PORT) > logs/mcp-server.log 2>&1 &
	@echo "MCP server started in background (PID: $$\!)"

# Main test target
test: test-guile test-mcp
	@echo "=== All tests completed ==="

# Build target - only copy worker.js, keep wrangler.toml in infra
build: infra/cloudflare/worker.js

infra/cloudflare/worker.js: mcp-server/changeflow-mcp.js
	@echo "📦 Building MCP server to infra/cloudflare/worker.js..."
	@cp mcp-server/changeflow-mcp.js infra/cloudflare/worker.js
	@echo "✅ Build complete"

# Deploy to Cloudflare (depends on worker.js being up to date)
deploy: build
	@echo "=== Deploying to Cloudflare Workers ==="
	@${MAKE} -C infra/cloudflare deploy
	@echo "✅ Deployment complete"

deploy-staging: build
	@echo "=== Deploying to Cloudflare Workers (Staging) ==="
	@${MAKE} -C infra/cloudflare deploy-staging
	@echo "✅ Staging deployment complete"

deploy-production: build
	@echo "=== Deploying to Cloudflare Workers (Production) ==="
	@${MAKE} -C infra/cloudflare deploy-production
	@echo "✅ Production deployment complete"

# Production sanity check
check-prod:
	@echo "=== Production Sanity Check ==="
	@curl -s https://api.changeflow.us/health | jq -r 'if .status == "healthy" then "✅ Production: HEALTHY" else "❌ Production: UNHEALTHY" end'
	@echo "Service: $$(curl -s https://api.changeflow.us/health | jq -r '.service')"
	@echo "Version: $$(curl -s https://api.changeflow.us/health | jq -r '.version')"
	@echo "Environment: $$(curl -s https://api.changeflow.us/health | jq -r '.environment')"

# Staging sanity check
check-staging:
	@echo "=== Staging Sanity Check ==="
	@curl -s https://guile-changeflow-staging.jasonwalsh.workers.dev/health | jq -r 'if .status == "healthy" then "✅ Staging: HEALTHY" else "❌ Staging: UNHEALTHY" end'
	@echo "Service: $$(curl -s https://guile-changeflow-staging.jasonwalsh.workers.dev/health | jq -r '.service')"
	@echo "Version: $$(curl -s https://guile-changeflow-staging.jasonwalsh.workers.dev/health | jq -r '.version')"

# Quick health check for both environments
check: check-staging check-prod
	@echo "=== All Environments Checked ==="

# OAuth Experiment Deployment
oauth-deploy:
	@echo "=== Deploying OAuth Experiment ==="
	@${MAKE} -C experiments/011-mcp-oauth-routes deploy
	@echo "✅ OAuth experiment deployed"

oauth-test:
	@echo "=== Testing OAuth Locally ==="
	@${MAKE} -C experiments/011-mcp-oauth-routes test

# Demo Data Generation
demo-data: data/demo-change-requests.json

data/demo-change-requests.json:
	@echo "🎯 Generating reproducible demo datasets..."
	@mkdir -p data
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -c "(use-modules (simulator data-export)) (generate-demo-dataset)"
	@echo "✅ Demo datasets created in data/ directory"

# Demo runners with pre-generated data
demo-quick: data/demo-quick.json
	@echo "🚀 Running 5-PR quick demo..."
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -c "(use-modules (simulator data-export) (simulator deployment-pipeline)) \
		          (run-demo-with-data \"data/demo-quick.json\" 5)"

demo-frontend: data/demo-frontend-only.json
	@echo "🎨 Running frontend-focused demo..."
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -c "(use-modules (simulator data-export) (simulator deployment-pipeline)) \
		          (run-demo-with-data \"data/demo-frontend-only.json\" 10)"

demo-backend: data/demo-backend-focus.json
	@echo "⚙️  Running backend-focused demo..."
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -c "(use-modules (simulator data-export) (simulator deployment-pipeline)) \
		          (run-demo-with-data \"data/demo-backend-focus.json\" 15)"

demo-infra: data/demo-infrastructure.json
	@echo "🏗️  Running infrastructure-focused demo..."
	@export GUILE_LOAD_PATH="$$PWD/src:$$GUILE_LOAD_PATH" && \
		guile -c "(use-modules (simulator data-export) (simulator deployment-pipeline)) \
		          (run-demo-with-data \"data/demo-infrastructure.json\" 8)"

# Clean demo data
clean-demo:
	@echo "🧹 Cleaning demo data..."
	@rm -rf data/demo-*.json
	@echo "✅ Demo data cleaned"
