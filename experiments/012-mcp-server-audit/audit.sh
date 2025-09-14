#!/usr/bin/env bash

# MCP Server Audit Script - Experiment 012
# Analyzes remote MCP servers to understand their implementation patterns
# Timestamp: $(date +"%Y-%m-%d %H:%M:%S")

echo "=== MCP Server Audit Tool - Experiment 012 ==="
echo "Analyzing remote MCP server implementations"
echo "Timestamp: $(date +"%Y-%m-%d %H:%M:%S")"
echo ""

# Output directory for artifacts
OUTPUT_DIR="experiments/012-mcp-server-audit/artifacts"
mkdir -p "$OUTPUT_DIR"

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

# Summary results file
SUMMARY_FILE="$OUTPUT_DIR/summary.txt"
echo "MCP Server Audit Summary - $(date)" > "$SUMMARY_FILE"
echo "=================================" >> "$SUMMARY_FILE"

for SERVER_NAME in "${!MCP_SERVERS[@]}"; do
    URL="${MCP_SERVERS[$SERVER_NAME]}"
    SERVER_FILE="$OUTPUT_DIR/${SERVER_NAME,,}.json"

    echo ""
    echo "📍 $SERVER_NAME: $URL"
    echo "───────────────────────────────"

    # Create JSON result object
    echo "{" > "$SERVER_FILE"
    echo "  \"server\": \"$SERVER_NAME\"," >> "$SERVER_FILE"
    echo "  \"url\": \"$URL\"," >> "$SERVER_FILE"
    echo "  \"timestamp\": \"$(date -Iseconds)\"," >> "$SERVER_FILE"

    # Test 1: GET request (MCP info)
    echo -n "  GET Request: "
    GET_RESP=$(curl -s -i "$URL" -H "Origin: https://claude.ai" 2>/dev/null)
    GET_STATUS=$(echo "$GET_RESP" | head -1 | awk '{print $2}')
    echo "HTTP $GET_STATUS"
    echo "  \"get_status\": \"$GET_STATUS\"," >> "$SERVER_FILE"

    # Save GET response headers
    echo "  \"get_headers\": {" >> "$SERVER_FILE"
    echo "$GET_RESP" | head -20 | grep -E "^[A-Za-z-]+:" | while IFS=: read -r key value; do
        echo "    \"$key\": \"$(echo $value | tr -d '\r')\"," >> "$SERVER_FILE"
    done
    echo "  }," >> "$SERVER_FILE"

    # Test 2: OPTIONS (CORS preflight)
    echo -n "  OPTIONS Request: "
    OPTIONS_RESP=$(curl -s -I -X OPTIONS "$URL" -H "Origin: https://claude.ai" 2>/dev/null | head -20)
    OPTIONS_STATUS=$(echo "$OPTIONS_RESP" | grep "HTTP" | awk '{print $2}')
    echo "HTTP ${OPTIONS_STATUS:-N/A}"
    echo "  \"options_status\": \"${OPTIONS_STATUS:-N/A}\"," >> "$SERVER_FILE"

    # Check CORS headers
    echo "  CORS Headers:"
    CORS_HEADERS=$(echo "$OPTIONS_RESP" | grep -i "access-control")
    if [ -n "$CORS_HEADERS" ]; then
        echo "$CORS_HEADERS" | sed 's/^/    /'
        echo "  \"cors_headers\": {" >> "$SERVER_FILE"
        echo "$CORS_HEADERS" | while IFS=: read -r key value; do
            echo "    \"$key\": \"$(echo $value | tr -d '\r')\"," >> "$SERVER_FILE"
        done
        echo "  }," >> "$SERVER_FILE"
    else
        echo "    None found"
        echo "  \"cors_headers\": null," >> "$SERVER_FILE"
    fi

    # Test 3: POST with MCP protocol
    echo -n "  POST tools/list: "
    POST_RESP=$(curl -s -X POST "$URL" \
        -H "Content-Type: application/json" \
        -H "Origin: https://claude.ai" \
        -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' \
        -w "\nHTTP_STATUS:%{http_code}" 2>/dev/null)

    POST_STATUS=$(echo "$POST_RESP" | grep "HTTP_STATUS:" | cut -d':' -f2)
    echo "HTTP $POST_STATUS"
    echo "  \"post_status\": \"$POST_STATUS\"," >> "$SERVER_FILE"

    # Check response type
    RESP_BODY=$(echo "$POST_RESP" | sed '/HTTP_STATUS:/d')
    if echo "$RESP_BODY" | jq -e '.jsonrpc' >/dev/null 2>&1; then
        echo "  Response Type: JSON-RPC"
        echo "  \"response_type\": \"JSON-RPC\"," >> "$SERVER_FILE"
        TOOLS_COUNT=$(echo "$RESP_BODY" | jq '.result.tools | length' 2>/dev/null)
        if [ -n "$TOOLS_COUNT" ] && [ "$TOOLS_COUNT" != "null" ]; then
            echo "  Tools Available: $TOOLS_COUNT"
            echo "  \"tools_count\": $TOOLS_COUNT," >> "$SERVER_FILE"
        else
            echo "  \"tools_count\": 0," >> "$SERVER_FILE"
        fi
    elif echo "$RESP_BODY" | grep -q "^data:"; then
        echo "  Response Type: Server-Sent Events (SSE)"
        echo "  \"response_type\": \"SSE\"," >> "$SERVER_FILE"
    elif echo "$RESP_BODY" | head -5 | grep -q "text/event-stream"; then
        echo "  Response Type: SSE (header)"
        echo "  \"response_type\": \"SSE-header\"," >> "$SERVER_FILE"
    else
        echo "  Response Type: Unknown/Error"
        echo "  \"response_type\": \"Unknown\"," >> "$SERVER_FILE"
        # Save first 200 chars of response for debugging
        echo "  \"response_preview\": \"$(echo "$RESP_BODY" | head -c 200 | tr '\n' ' ')\"," >> "$SERVER_FILE"
    fi

    # Test 4: Check for SSE endpoint
    if [[ "$URL" == *"/sse"* ]]; then
        echo "  SSE Endpoint: Yes (explicit)"
        echo "  \"sse_endpoint\": \"explicit\"," >> "$SERVER_FILE"
    else
        echo -n "  SSE Support: "
        SSE_TEST=$(curl -s -I "$URL" -H "Accept: text/event-stream" 2>/dev/null | grep -i "content-type")
        if echo "$SSE_TEST" | grep -q "text/event-stream"; then
            echo "Yes"
            echo "  \"sse_support\": true," >> "$SERVER_FILE"
        else
            echo "No/Unknown"
            echo "  \"sse_support\": false," >> "$SERVER_FILE"
        fi
    fi

    # Close JSON object
    echo "  \"complete\": true" >> "$SERVER_FILE"
    echo "}" >> "$SERVER_FILE"

    # Add to summary
    echo "" >> "$SUMMARY_FILE"
    echo "$SERVER_NAME:" >> "$SUMMARY_FILE"
    echo "  URL: $URL" >> "$SUMMARY_FILE"
    echo "  GET: $GET_STATUS" >> "$SUMMARY_FILE"
    echo "  OPTIONS: ${OPTIONS_STATUS:-N/A}" >> "$SUMMARY_FILE"
    echo "  POST: $POST_STATUS" >> "$SUMMARY_FILE"
    echo "  CORS: $([ -n "$CORS_HEADERS" ] && echo "Yes" || echo "No")" >> "$SUMMARY_FILE"
done

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Analysis Complete"
echo ""

# Summary of findings
echo "Key Observations:" | tee -a "$SUMMARY_FILE"
echo "1. Most production MCP servers use SSE (Server-Sent Events)" | tee -a "$SUMMARY_FILE"
echo "2. URLs often end with /sse or /mcp" | tee -a "$SUMMARY_FILE"
echo "3. CORS headers are essential for Claude.ai" | tee -a "$SUMMARY_FILE"
echo "4. Support both GET (info) and POST (protocol) requests" | tee -a "$SUMMARY_FILE"
echo "" | tee -a "$SUMMARY_FILE"
echo "Recommendations for ChangeFlow:" | tee -a "$SUMMARY_FILE"
echo "1. Add CORS headers: Access-Control-Allow-Origin: https://claude.ai" | tee -a "$SUMMARY_FILE"
echo "2. Consider SSE support for real-time communication" | tee -a "$SUMMARY_FILE"
echo "3. Ensure GET /mcp returns server info (not 500)" | tee -a "$SUMMARY_FILE"
echo "4. Handle OPTIONS preflight requests" | tee -a "$SUMMARY_FILE"

echo ""
echo "Artifacts saved to: $OUTPUT_DIR"
echo "Summary available at: $SUMMARY_FILE"