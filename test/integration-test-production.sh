#!/usr/bin/env bash
# Integration test for production routes
# Tests all endpoints and verifies response types

set -e

BASE_URL="https://api.changeflow.us"
PASS_COUNT=0
FAIL_COUNT=0

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=== ChangeFlow MCP Server Integration Test ==="
echo "Testing: $BASE_URL"
echo "Date: $(date)"
echo ""

# Test function
test_endpoint() {
    local method="$1"
    local path="$2"
    local expected_type="$3"
    local description="$4"

    echo -n "Testing $method $path - $description... "

    if [ "$method" = "GET" ]; then
        response=$(curl -s -w "\nHTTP_STATUS:%{http_code}" "$BASE_URL$path")
    else
        response=$(curl -s -X "$method" -w "\nHTTP_STATUS:%{http_code}" "$BASE_URL$path" \
            -H "Content-Type: application/json" \
            -d '{"jsonrpc":"2.0","method":"tools/list","id":1}')
    fi

    http_status=$(echo "$response" | tail -1 | cut -d':' -f2)
    body=$(echo "$response" | sed '$d')

    # Check status code
    if [ "$http_status" -ge 200 ] && [ "$http_status" -lt 400 ]; then
        # Check content type
        if [ "$expected_type" = "html" ]; then
            if echo "$body" | grep -q "<!DOCTYPE html>"; then
                echo -e "${GREEN}✓${NC} (HTTP $http_status)"
                ((PASS_COUNT++))
            else
                echo -e "${RED}✗${NC} Expected HTML, got: ${body:0:50}..."
                ((FAIL_COUNT++))
            fi
        elif [ "$expected_type" = "json" ]; then
            if echo "$body" | jq . >/dev/null 2>&1; then
                echo -e "${GREEN}✓${NC} (HTTP $http_status)"
                ((PASS_COUNT++))
            else
                echo -e "${RED}✗${NC} Invalid JSON response"
                ((FAIL_COUNT++))
            fi
        elif [ "$expected_type" = "redirect" ]; then
            if [ "$http_status" = "302" ] || [ "$http_status" = "301" ]; then
                echo -e "${GREEN}✓${NC} (HTTP $http_status redirect)"
                ((PASS_COUNT++))
            else
                echo -e "${RED}✗${NC} Expected redirect, got HTTP $http_status"
                ((FAIL_COUNT++))
            fi
        fi
    else
        echo -e "${RED}✗${NC} HTTP $http_status"
        ((FAIL_COUNT++))
    fi
}

# Test version check
echo "=== Version Check ==="
version=$(curl -s "$BASE_URL/health" | jq -r '.version')
echo "Deployed version: $version"
if [ "$version" = "1.2.0" ]; then
    echo -e "${GREEN}✓${NC} Version matches expected 1.2.0"
    ((PASS_COUNT++))
else
    echo -e "${YELLOW}⚠${NC} Version mismatch: expected 1.2.0, got $version"
fi
echo ""

# Test all routes
echo "=== Route Tests ==="
test_endpoint "GET" "/" "html" "Landing page"
test_endpoint "GET" "/health" "json" "Health check"
test_endpoint "GET" "/mcp" "json" "MCP info"
test_endpoint "POST" "/mcp" "json" "MCP protocol"
test_endpoint "GET" "/authorize" "redirect" "OAuth start (should redirect)"

echo ""
echo "=== Response Content Tests ==="

# Test health endpoint content
echo -n "Health endpoint content validation... "
health_response=$(curl -s "$BASE_URL/health")
if echo "$health_response" | jq -e '.status == "healthy" and .service and .version and .capabilities' >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Valid health response"
    ((PASS_COUNT++))
else
    echo -e "${RED}✗${NC} Invalid health response structure"
    ((FAIL_COUNT++))
fi

# Test MCP endpoint content
echo -n "MCP endpoint content validation... "
mcp_response=$(curl -s "$BASE_URL/mcp")
if echo "$mcp_response" | jq -e '.mcp_version and .server_name == "guile-changeflow" and .capabilities.tools == true' >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Valid MCP response"
    ((PASS_COUNT++))
else
    echo -e "${RED}✗${NC} Invalid MCP response structure"
    ((FAIL_COUNT++))
fi

# Test MCP tools list
echo -n "MCP tools list validation... "
tools_response=$(curl -s -X POST "$BASE_URL/mcp" \
    -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","method":"tools/list","id":1}')

tool_count=$(echo "$tools_response" | jq '.result.tools | length' 2>/dev/null)
if [ "$tool_count" -eq 8 ]; then
    echo -e "${GREEN}✓${NC} Found all 8 ITIL tools"
    ((PASS_COUNT++))
else
    echo -e "${RED}✗${NC} Expected 8 tools, found $tool_count"
    ((FAIL_COUNT++))
fi

echo ""
echo "=== Summary ==="
echo -e "Passed: ${GREEN}$PASS_COUNT${NC}"
echo -e "Failed: ${RED}$FAIL_COUNT${NC}"

if [ "$FAIL_COUNT" -eq 0 ]; then
    echo -e "${GREEN}✅ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed${NC}"
    exit 1
fi