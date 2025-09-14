# MCP OAuth Experiment Makefile

.PHONY: deploy deploy-staging deploy-production test clean

# Deploy to staging by default
deploy: deploy-staging

# Deploy to staging environment
deploy-staging:
	@echo "🧪 Deploying OAuth experiment to staging..."
	@wrangler deploy --env staging
	@echo "✅ Staging deployment complete"

# Deploy to production (use with caution!)
deploy-production:
	@echo "⚠️  Deploying OAuth experiment to PRODUCTION"
	@echo "Press Ctrl+C to cancel, or wait 3 seconds..."
	@sleep 3
	@wrangler deploy
	@echo "✅ Production deployment complete"

# Test OAuth endpoints locally
test:
	@echo "🧪 Testing OAuth endpoints..."
	@wrangler dev

# Clean up
clean:
	@rm -rf node_modules
	@echo "✅ Cleaned up node_modules"

# Install dependencies
install:
	@npm install
	@echo "✅ Dependencies installed"

# Show configuration
info:
	@echo "=== OAuth Experiment Configuration ==="
	@echo "Worker: mcp-oauth-experiment"
	@echo "Main: worker.js"
	@echo ""
	@echo "OAuth Endpoints:"
	@echo "  /authorize - Start OAuth flow"
	@echo "  /callback  - OAuth callback"
	@echo "  /token     - Token exchange"
	@echo "  /sse       - Protected MCP endpoint"
	@echo ""
	@echo "Required Secrets:"
	@echo "  GITHUB_CLIENT_ID"
	@echo "  GITHUB_CLIENT_SECRET"