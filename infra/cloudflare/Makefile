# Cloudflare Worker Deployment Makefile
# Note: Run 'make build' from root directory first to generate worker.js

.PHONY: deploy deploy-staging deploy-production validate info logs logs-staging

# Default deployment (staging)
deploy: deploy-staging

# Deploy to staging
deploy-staging:
	@if [ ! -f worker.js ]; then \
		echo "❌ worker.js not found. Run 'make build' from root directory first"; \
		exit 1; \
	fi
	@echo "📡 Deploying to staging worker..."
	@wrangler deploy --env staging
	@echo "✅ Staging deployment complete"
	@echo "🔗 URL: https://guile-changeflow-staging.jasonwalsh.workers.dev"

# Deploy to production
deploy-production:
	@if [ ! -f worker.js ]; then \
		echo "❌ worker.js not found. Run 'make build' from root directory first"; \
		exit 1; \
	fi
	@echo "🚀 Deploying to production worker..."
	@wrangler deploy
	@echo "✅ Production deployment complete"
	@echo "🔗 URL: https://api.changeflow.us"

# Validate wrangler.toml
validate:
	@echo "🔍 Validating wrangler.toml..."
	@wrangler deploy --dry-run
	@echo "✅ Configuration valid"

# Show current deployment info
info:
	@echo "=== Cloudflare Worker Info ==="
	@echo "Production: guile-changeflow-prod"
	@echo "Staging: guile-changeflow-staging"
	@echo "Custom Domain: api.changeflow.us"
	@echo ""
	@echo "Secrets needed:"
	@echo "  - GITHUB_CLIENT_ID"
	@echo "  - GITHUB_CLIENT_SECRET"

# Tail production logs
logs:
	@wrangler tail

# Tail staging logs
logs-staging:
	@wrangler tail --env staging