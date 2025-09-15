#!/bin/bash
# Parallel deployment script for Guile ChangeFlow v1.4.4
# Deploys to staging and production simultaneously

set -e

echo "🚀 Guile ChangeFlow Parallel Deployment Script"
echo "Version: 1.4.4"
echo "Time: $(date -Iseconds)"
echo "════════════════════════════════════════════════"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
VERSION="1.4.4"
STAGING_WORKER="guile-changeflow-staging"
PROD_WORKER="guile-changeflow-prod"
STAGING_URL="https://staging.mcp.changeflow.us"
PROD_URL="https://mcp.changeflow.us"

# Pre-flight checks
preflight_checks() {
    echo "🔍 Running pre-flight checks..."

    # Check wrangler authentication
    if ! wrangler whoami &>/dev/null; then
        echo -e "${RED}❌ Wrangler not authenticated${NC}"
        echo "Run: wrangler login"
        exit 1
    fi

    # Check git status
    if [[ -n $(git status --porcelain) ]]; then
        echo -e "${YELLOW}⚠️ Uncommitted changes detected${NC}"
        read -p "Continue anyway? (y/n) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi

    # Check version tag
    if ! git rev-parse "v${VERSION}" &>/dev/null; then
        echo "Creating version tag v${VERSION}..."
        git tag -a "v${VERSION}" -m "Release v${VERSION}"
    fi

    echo -e "${GREEN}✅ Pre-flight checks passed${NC}"
}

# Deploy to environment
deploy_environment() {
    local env_name=$1
    local worker_name=$2
    local url=$3
    local log_file="/tmp/deploy-${env_name,,}-${VERSION}.log"

    echo "[$env_name] Deploying to $worker_name..."

    # Deploy with wrangler
    if wrangler deploy \
        --name "$worker_name" \
        --compatibility-date 2024-01-01 \
        --var VERSION:"$VERSION" \
        > "$log_file" 2>&1; then

        # Wait for deployment to propagate
        sleep 5

        # Verify deployment
        local deployed_version=$(curl -s "$url/version" 2>/dev/null | jq -r .version)

        if [ "$deployed_version" = "$VERSION" ]; then
            echo -e "[$env_name] ${GREEN}✅ Deployed successfully (v${deployed_version})${NC}"
            return 0
        else
            echo -e "[$env_name] ${YELLOW}⚠️ Deployed but version mismatch (got: ${deployed_version})${NC}"
            return 1
        fi
    else
        echo -e "[$env_name] ${RED}❌ Deployment failed${NC}"
        echo "[$env_name] Check logs at: $log_file"
        return 1
    fi
}

# Main deployment
main() {
    preflight_checks

    echo ""
    echo "🚀 Starting parallel deployment..."
    echo "════════════════════════════════════════════════"

    # Start deployments in parallel
    deploy_environment "STAGING" "$STAGING_WORKER" "$STAGING_URL" &
    STAGING_PID=$!

    deploy_environment "PRODUCTION" "$PROD_WORKER" "$PROD_URL" &
    PROD_PID=$!

    # Wait for both deployments
    echo ""
    echo "⏳ Waiting for deployments to complete..."

    wait $STAGING_PID
    STAGING_RESULT=$?

    wait $PROD_PID
    PROD_RESULT=$?

    echo ""
    echo "════════════════════════════════════════════════"
    echo "📊 Deployment Results:"

    if [ $STAGING_RESULT -eq 0 ]; then
        echo -e "  Staging:    ${GREEN}✅ SUCCESS${NC}"
    else
        echo -e "  Staging:    ${RED}❌ FAILED${NC}"
    fi

    if [ $PROD_RESULT -eq 0 ]; then
        echo -e "  Production: ${GREEN}✅ SUCCESS${NC}"
    else
        echo -e "  Production: ${RED}❌ FAILED${NC}"
    fi

    # Overall status
    if [[ $STAGING_RESULT -eq 0 && $PROD_RESULT -eq 0 ]]; then
        echo ""
        echo -e "${GREEN}🎉 Deployment successful!${NC}"
        echo "Version $VERSION is now live in both environments"

        # Push tag to remote
        git push origin "v${VERSION}" 2>/dev/null || true

        exit 0
    else
        echo ""
        echo -e "${RED}⚠️ Deployment completed with errors${NC}"
        echo "Manual intervention may be required"
        exit 1
    fi
}

# Run main function
main "$@"