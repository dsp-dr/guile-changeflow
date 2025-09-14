#!/usr/bin/env bash

# MCP Server Audit Script
# Analyzes remote MCP servers to understand their implementation patterns

echo "=== MCP Server Audit Tool ==="
echo "Analyzing remote MCP server implementations"
echo ""

# Array of MCP servers to test
declare -A MCP_SERVERS=(
    ["Stripe"]="https://mcp.stripe.com"
    ["Linear"]="https://mcp.linear.app/sse"
    ["Asana"]="https://mcp.asana.com/sse"
    ["Square"]="https://mcp.squareup.com/sse"
    ["HubSpot"]="https://mcp.hubspot.com/anthropic"
    ["Vercel"]="https://mcp.vercel.com/"
    ["Plaid"]="https://api.dashboard.plaid.com/mcp/sse"
    ["ChangeFlow"]="https://mcp.changeflow.us/mcp"
)

echo "Testing ${#MCP_SERVERS[@]} MCP servers..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

for SERVER_NAME in "${!MCP_SERVERS[@]}"; do
    URL="${MCP_SERVERS[$SERVER_NAME]}"
    echo ""
    echo "📍 $SERVER_NAME: $URL"
    echo "───────────────────────────────"

    # Test 1: GET request (MCP info)
    echo -n "  GET Request: "
    STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$URL" 2>/dev/null)
    echo "HTTP $STATUS"

    # Test 2: OPTIONS (CORS preflight)
    echo -n "  OPTIONS Request: "
    OPTIONS_RESP=$(curl -s -I -X OPTIONS "$URL" -H "Origin: https://claude.ai" 2>/dev/null | head -20)
    OPTIONS_STATUS=$(echo "$OPTIONS_RESP" | grep "HTTP" | awk '{print $2}')
    echo "HTTP ${OPTIONS_STATUS:-N/A}"

    # Check CORS headers
    echo "  CORS Headers:"
    echo "$OPTIONS_RESP" | grep -i "access-control" | sed 's/^/    /' || echo "    None found"

    # Test 3: POST with MCP protocol
    echo -n "  POST tools/list: "
    POST_RESP=$(curl -s -X POST "$URL" \
        -H "Content-Type: application/json" \
        -H "Origin: https://claude.ai" \
        -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' \
        -w "\nHTTP_STATUS:%{http_code}" 2>/dev/null)

    POST_STATUS=$(echo "$POST_RESP" | grep "HTTP_STATUS:" | cut -d':' -f2)
    echo "HTTP $POST_STATUS"

    # Check response type
    RESP_BODY=$(echo "$POST_RESP" | sed '/HTTP_STATUS:/d')
    if echo "$RESP_BODY" | jq -e '.jsonrpc' >/dev/null 2>&1; then
        echo "  Response Type: JSON-RPC"
        TOOLS_COUNT=$(echo "$RESP_BODY" | jq '.result.tools | length' 2>/dev/null)
        [ -n "$TOOLS_COUNT" ] && echo "  Tools Available: $TOOLS_COUNT"
    elif echo "$RESP_BODY" | grep -q "^data:"; then
        echo "  Response Type: Server-Sent Events (SSE)"
    elif echo "$RESP_BODY" | grep -q "text/event-stream"; then
        echo "  Response Type: SSE (header)"
    else
        echo "  Response Type: Unknown/Error"
    fi

    # Test 4: Check for SSE endpoint
    if [[ "$URL" == *"/sse"* ]]; then
        echo "  SSE Endpoint: Yes (explicit)"
    else
        echo -n "  SSE Support: "
        SSE_TEST=$(curl -s -I "$URL" -H "Accept: text/event-stream" 2>/dev/null | grep -i "content-type")
        if echo "$SSE_TEST" | grep -q "text/event-stream"; then
            echo "Yes"
        else
            echo "No/Unknown"
        fi
    fi
done

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Analysis Complete"
echo ""

# Summary of findings
echo "Key Observations:"
echo "1. Most production MCP servers use SSE (Server-Sent Events)"
echo "2. URLs often end with /sse or /mcp"
echo "3. CORS headers are essential for Claude.ai"
echo "4. Support both GET (info) and POST (protocol) requests"
echo ""
echo "Recommendations for ChangeFlow:"
echo "1. Add CORS headers: Access-Control-Allow-Origin: https://claude.ai"
echo "2. Consider SSE support for real-time communication"
echo "3. Ensure GET /mcp returns server info (not 500)"
echo "4. Handle OPTIONS preflight requests"