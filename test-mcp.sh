#!/bin/bash

# MCP Server Testing Script
# Tests all MCP endpoints and tool invocations

API_URL="${1:-https://api.changeflow.us}"

echo "🧪 Testing MCP Server at $API_URL"
echo "=================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test function
test_endpoint() {
    local method=$1
    local path=$2
    local data=$3
    local description=$4

    echo -e "${YELLOW}Testing:${NC} $description"
    echo "  Method: $method"
    echo "  URL: $API_URL$path"

    if [ "$method" = "POST" ]; then
        echo "  Data: $data"
        response=$(curl -s -X POST "$API_URL$path" \
            -H "Content-Type: application/json" \
            -d "$data")
    else
        response=$(curl -s "$API_URL$path")
    fi

    if [ $? -eq 0 ]; then
        echo -e "  ${GREEN}✓ Success${NC}"
        echo "  Response:"
        echo "$response" | jq . 2>/dev/null || echo "$response"
    else
        echo -e "  ${RED}✗ Failed${NC}"
    fi
    echo ""
}

# 1. Health Check
test_endpoint "GET" "/health" "" "Health Check"

# 2. MCP Discovery
test_endpoint "GET" "/mcp" "" "MCP Discovery"

# 3. Tools List
test_endpoint "GET" "/tools" "" "List Available Tools"

# 4. Create Change Request (Low Risk)
test_endpoint "POST" "/mcp/tools/invoke" '{
  "tool": "create_change_request",
  "params": {
    "title": "Update documentation",
    "description": "Update API documentation with new endpoints",
    "systems": ["docs"],
    "urgency": "low"
  }
}' "Create Low Risk Change"

# 5. Create Change Request (High Risk)
test_endpoint "POST" "/mcp/tools/invoke" '{
  "tool": "create_change_request",
  "params": {
    "title": "Update production payment gateway",
    "description": "Critical security patch for payment processing in production",
    "systems": ["payment", "api", "database"],
    "urgency": "emergency"
  }
}' "Create High Risk Change"

# 6. Risk Assessment
test_endpoint "POST" "/mcp/tools/invoke" '{
  "tool": "assess_risk",
  "params": {
    "title": "Deploy new authentication system",
    "description": "Replace existing auth with OAuth 2.0 in production",
    "systems": ["auth", "api", "frontend"],
    "urgency": "high"
  }
}' "Risk Assessment"

# 7. List Changes
test_endpoint "POST" "/mcp/tools/invoke" '{
  "tool": "list_change_requests",
  "params": {}
}' "List All Changes"

# 8. Get Specific Change (will fail if ID doesn't exist)
test_endpoint "POST" "/mcp/tools/invoke" '{
  "tool": "get_change_request",
  "params": {
    "id": "CHG-2025-001"
  }
}' "Get Specific Change"

# 9. Test API endpoint
test_endpoint "GET" "/api/changes" "" "API: List Changes"

# 10. Test webhook endpoint
test_endpoint "POST" "/webhooks/github" '{
  "action": "opened",
  "pull_request": {
    "number": 123,
    "title": "Fix security vulnerability in authentication",
    "body": "This PR fixes a critical security issue in the production authentication system",
    "html_url": "https://github.com/org/repo/pull/123",
    "user": {
      "login": "developer"
    },
    "base": {
      "repo": {
        "name": "main-app"
      }
    },
    "labels": [
      {"name": "urgent"}
    ]
  }
}' "GitHub Webhook"

echo "=================================="
echo "🎯 Testing Complete!"
echo ""
echo "📊 Workers Logs Notes:"
echo "  - 10% of requests are sampled for logging"
echo "  - All tool invocations are logged"
echo "  - Check logs with: wrangler tail"
echo ""

# Test CORS headers
echo "🔒 CORS Configuration Test:"
curl -s -I -X OPTIONS "$API_URL/mcp" \
    -H "Origin: https://claude.ai" \
    -H "Access-Control-Request-Method: POST" | grep -i "access-control"

echo ""
echo "✅ All tests completed. Check output above for results."