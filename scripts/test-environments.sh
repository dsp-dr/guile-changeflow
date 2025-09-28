#!/bin/bash

# Test script for staging and production environments
set -e

echo "============================================"
echo "Testing ChangeFlow MCP Environments"
echo "============================================"
echo ""

# Function to test an environment
test_environment() {
    local ENV_NAME=$1
    local BASE_URL=$2

    echo "Testing $ENV_NAME environment..."
    echo "Base URL: $BASE_URL"
    echo "-------------------------------------------"

    # Test health endpoint
    echo -n "1. Health check: "
    HEALTH_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$BASE_URL/health" || echo "000")
    if [ "$HEALTH_STATUS" = "200" ]; then
        echo "✅ OK (200)"
        VERSION=$(curl -s "$BASE_URL/health" | jq -r '.version // "unknown"' 2>/dev/null || echo "unknown")
        echo "   Version: $VERSION"
    else
        echo "❌ FAILED ($HEALTH_STATUS)"
    fi

    # Test OAuth discovery
    echo -n "2. OAuth discovery: "
    OAUTH_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$BASE_URL/.well-known/oauth-authorization-server" || echo "000")
    if [ "$OAUTH_STATUS" = "200" ]; then
        echo "✅ OK (200)"
    else
        echo "❌ FAILED ($OAUTH_STATUS)"
    fi

    # Test SSE endpoint (expect 401)
    echo -n "3. SSE endpoint (expect 401): "
    SSE_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$BASE_URL/v1/sse" || echo "000")
    if [ "$SSE_STATUS" = "401" ]; then
        echo "✅ OK (401 as expected)"
    else
        echo "⚠️  Unexpected status ($SSE_STATUS)"
    fi

    # Test authorize endpoint
    echo -n "4. Authorize endpoint: "
    AUTH_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$BASE_URL/authorize" || echo "000")
    if [ "$AUTH_STATUS" = "302" ] || [ "$AUTH_STATUS" = "200" ]; then
        echo "✅ OK ($AUTH_STATUS)"
    else
        echo "❌ FAILED ($AUTH_STATUS)"
    fi

    echo ""
}

# Test production
if [ "$1" != "staging-only" ]; then
    test_environment "PRODUCTION" "https://mcp.changeflow.us"
fi

# Test staging (if accessible)
if [ "$1" != "prod-only" ]; then
    echo "Note: Staging may not be publicly accessible"
    test_environment "STAGING" "https://guile-changeflow-staging.workers.dev"
fi

echo "============================================"
echo "Test completed"
echo "============================================"