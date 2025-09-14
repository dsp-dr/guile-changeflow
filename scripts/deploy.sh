#!/bin/bash

# Guile ChangeFlow MCP Server Deployment Script
# Production deployment to Cloudflare Workers

set -e

echo "🚀 Deploying Guile ChangeFlow MCP Server to Production"
echo "=================================================="

# Check if wrangler is installed
if ! command -v wrangler &> /dev/null; then
    echo "❌ Wrangler CLI not found. Installing..."
    npm install -g wrangler
fi

# Login to Cloudflare (if needed)
echo "📝 Checking Cloudflare authentication..."
wrangler whoami || wrangler login

# Deploy to production
echo "🏗️  Deploying Worker to production..."
wrangler deploy --compatibility-date 2025-01-13

# Set up custom domain (if not already configured)
echo "🌐 Configuring custom domain api.changeflow.us..."
# This is typically done in the Cloudflare dashboard

# Test the deployment
echo "🧪 Testing production deployment..."
echo ""

# Health check
echo "Testing /health endpoint:"
curl -s https://api.changeflow.us/health | jq .

echo ""
echo "Testing /mcp endpoint:"
curl -s https://api.changeflow.us/mcp | jq .

echo ""
echo "Testing /tools endpoint:"
curl -s https://api.changeflow.us/tools | jq '.[] | {name: .name, description: .description}'

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📊 Workers Logs Configuration:"
echo "  - Sampling Rate: 10% of requests"
echo "  - Tool invocations: Always logged"
echo "  - Error tracking: Enabled"
echo ""
echo "🔗 Production URLs:"
echo "  - API: https://api.changeflow.us"
echo "  - MCP: https://api.changeflow.us/mcp"
echo "  - Tools: https://api.changeflow.us/tools"
echo ""
echo "📝 To view logs:"
echo "  wrangler tail"
echo ""
echo "🎯 Ready for 7 AM demo!"