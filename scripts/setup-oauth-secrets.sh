#!/usr/bin/env bash
# Setup OAuth secrets for ChangeFlow MCP Server
# This script helps add OAuth credentials to GitHub repository secrets

set -e

echo "=== OAuth Secret Setup Script ==="
echo "This will help you add OAuth secrets to GitHub repository"
echo ""

# Check if gh CLI is installed
if ! command -v gh &> /dev/null; then
    echo "❌ GitHub CLI (gh) is not installed"
    echo "Install with: pkg install gh (FreeBSD) or brew install gh (macOS)"
    exit 1
fi

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "❌ Not in a git repository"
    exit 1
fi

# Check if .env exists
if [ ! -f .env ]; then
    echo "⚠️ .env file not found"
    echo "Copy .env.template to .env and fill in your credentials first"
    exit 1
fi

# Source the .env file
source .env

# Validate required variables
if [ -z "$GITHUB_CLIENT_ID" ] || [ "$GITHUB_CLIENT_ID" == "your_client_id_here" ]; then
    echo "❌ GITHUB_CLIENT_ID not set in .env"
    exit 1
fi

if [ -z "$GITHUB_CLIENT_SECRET" ] || [ "$GITHUB_CLIENT_SECRET" == "your_client_secret_here" ]; then
    echo "❌ GITHUB_CLIENT_SECRET not set in .env"
    exit 1
fi

echo "✅ Found OAuth credentials in .env"
echo ""

# Add to GitHub repository secrets
echo "Adding secrets to GitHub repository..."

# Add CLIENT_ID
gh secret set GITHUB_OAUTH_CLIENT_ID --body "$GITHUB_CLIENT_ID" && \
    echo "✅ Added GITHUB_OAUTH_CLIENT_ID"

# Add CLIENT_SECRET
gh secret set GITHUB_OAUTH_CLIENT_SECRET --body "$GITHUB_CLIENT_SECRET" && \
    echo "✅ Added GITHUB_OAUTH_CLIENT_SECRET"

echo ""
echo "=== GitHub Secrets Added Successfully ==="
echo ""
echo "Next steps:"
echo "1. Go to Cloudflare dashboard: https://dash.cloudflare.com"
echo "2. Navigate to Workers & Pages → guile-changeflow-prod → Settings → Variables"
echo "3. Add these environment variables:"
echo "   - GITHUB_CLIENT_ID = $GITHUB_CLIENT_ID (as Text)"
echo "   - GITHUB_CLIENT_SECRET = [your secret] (as Secret - click Encrypt)"
echo ""
echo "4. Repeat for staging worker if needed"
echo "5. Deploy with: gmake deploy-production"