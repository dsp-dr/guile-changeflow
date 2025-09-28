#!/bin/bash
# Test OAuth flow for Claude.ai

echo "Testing OAuth Flow for MCP Server"
echo "================================="

# 1. Test discovery endpoint
echo ""
echo "1. Testing discovery endpoint..."
curl -s https://mcp.changeflow.us/.well-known/oauth-authorization-server | jq '.authorization_endpoint'

# 2. Test SSE without auth (should get 401)
echo ""
echo "2. Testing SSE endpoint without auth (expecting 401)..."
STATUS=$(curl -s -o /dev/null -w "%{http_code}" https://mcp.changeflow.us/v1/sse)
echo "   Status: $STATUS (expected 401)"

# 3. Generate test authorization code
echo ""
echo "3. Generating test authorization code..."
CODE_DATA='{"client_id":"test-client","scope":"mcp","exp":'$(($(date +%s) * 1000 + 600000))'}'
AUTH_CODE=$(echo "$CODE_DATA" | base64)
echo "   Code: $AUTH_CODE"

# 4. Exchange code for token
echo ""
echo "4. Exchanging code for token..."
TOKEN_RESPONSE=$(echo "{\"grant_type\":\"authorization_code\",\"code\":\"$AUTH_CODE\",\"client_id\":\"test-client\"}" | \
  curl -X POST https://mcp.changeflow.us/token \
  -H "Content-Type: application/json" \
  -d @- -s)

echo "   Response: $TOKEN_RESPONSE"
ACCESS_TOKEN=$(echo "$TOKEN_RESPONSE" | jq -r '.access_token')

# 5. Test SSE with token
echo ""
echo "5. Testing SSE with Bearer token..."
if [ "$ACCESS_TOKEN" != "null" ]; then
  curl -s https://mcp.changeflow.us/v1/sse \
    -H "Authorization: Bearer $ACCESS_TOKEN" \
    | head -5
else
  echo "   Failed to get access token"
fi

echo ""
echo "Test complete!"