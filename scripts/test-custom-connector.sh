#!/bin/bash
# Test script for Claude.ai Custom Connector
# This verifies our MCP server is ready for demo

set -e

API_URL="https://api.changeflow.us"

echo "═══════════════════════════════════════════════════════════"
echo "   Guile ChangeFlow MCP Server - Custom Connector Test"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 1. Health Check
echo "1. Testing Health Endpoint..."
HEALTH=$(curl -s $API_URL/health)
if echo "$HEALTH" | jq -e '.status == "healthy"' > /dev/null; then
    echo -e "${GREEN}✓${NC} Health check passed"
    echo "   Service: $(echo $HEALTH | jq -r '.service')"
    echo "   Version: $(echo $HEALTH | jq -r '.version')"
else
    echo -e "${RED}✗${NC} Health check failed"
    exit 1
fi
echo ""

# 2. MCP Protocol Check
echo "2. Testing MCP Protocol Endpoint..."
MCP_INFO=$(curl -s $API_URL/mcp)
if echo "$MCP_INFO" | jq -e '.mcp_version' > /dev/null; then
    echo -e "${GREEN}✓${NC} MCP protocol endpoint working"
    echo "   Version: $(echo $MCP_INFO | jq -r '.mcp_version')"
else
    echo -e "${RED}✗${NC} MCP protocol endpoint failed"
    exit 1
fi
echo ""

# 3. Tools Discovery
echo "3. Testing Tools Discovery..."
TOOLS=$(curl -s $API_URL/mcp/tools)
TOOL_COUNT=$(echo "$TOOLS" | jq '. | length')
if [ "$TOOL_COUNT" -eq 4 ]; then
    echo -e "${GREEN}✓${NC} Found all 4 MCP tools:"
    echo "$TOOLS" | jq -r '.[].name' | sed 's/^/   - /'
else
    echo -e "${RED}✗${NC} Expected 4 tools, found $TOOL_COUNT"
    exit 1
fi
echo ""

# 4. Test Tool Invocation - Low Risk
echo "4. Testing Low Risk Change Creation..."
LOW_RISK=$(curl -s -X POST $API_URL/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "create_change_request",
    "params": {
      "title": "Update documentation",
      "description": "Fix typos in README",
      "systems": ["docs"],
      "urgency": "low"
    }
  }')

if echo "$LOW_RISK" | jq -e '.id' > /dev/null; then
    ID=$(echo "$LOW_RISK" | jq -r '.id')
    SCORE=$(echo "$LOW_RISK" | jq -r '.risk_score')
    CATEGORY=$(echo "$LOW_RISK" | jq -r '.risk_category')
    echo -e "${GREEN}✓${NC} Created low risk change: $ID"
    echo "   Risk Score: $SCORE ($CATEGORY)"
    if [ "$CATEGORY" = "low" ]; then
        echo -e "   ${GREEN}✓${NC} Correctly categorized as LOW risk"
    else
        echo -e "   ${YELLOW}⚠${NC} Warning: Expected LOW risk category"
    fi
else
    echo -e "${RED}✗${NC} Failed to create low risk change"
    exit 1
fi
echo ""

# 5. Test Tool Invocation - High Risk
echo "5. Testing High Risk Change Creation..."
HIGH_RISK=$(curl -s -X POST $API_URL/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "create_change_request",
    "params": {
      "title": "Emergency production security patch",
      "description": "Fix critical authentication vulnerability",
      "systems": ["auth", "production", "security"],
      "urgency": "emergency"
    }
  }')

if echo "$HIGH_RISK" | jq -e '.id' > /dev/null; then
    ID=$(echo "$HIGH_RISK" | jq -r '.id')
    SCORE=$(echo "$HIGH_RISK" | jq -r '.risk_score')
    CATEGORY=$(echo "$HIGH_RISK" | jq -r '.risk_category')
    echo -e "${GREEN}✓${NC} Created high risk change: $ID"
    echo "   Risk Score: $SCORE ($CATEGORY)"
    if [ "$CATEGORY" = "high" ]; then
        echo -e "   ${GREEN}✓${NC} Correctly categorized as HIGH risk"
    else
        echo -e "   ${YELLOW}⚠${NC} Warning: Expected HIGH risk category"
    fi
else
    echo -e "${RED}✗${NC} Failed to create high risk change"
    exit 1
fi
echo ""

# 6. Test Change Retrieval
echo "6. Testing Change Retrieval..."
RETRIEVED=$(curl -s -X POST $API_URL/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -d "{
    \"tool\": \"get_change_request\",
    \"params\": {
      \"id\": \"$ID\"
    }
  }")

if echo "$RETRIEVED" | jq -e '.id' > /dev/null; then
    echo -e "${GREEN}✓${NC} Successfully retrieved change $ID"
    echo "   Status: $(echo $RETRIEVED | jq -r '.status')"
else
    echo -e "${RED}✗${NC} Failed to retrieve change"
    exit 1
fi
echo ""

# 7. Test Risk Assessment
echo "7. Testing Risk Assessment Tool..."
RISK_ASSESS=$(curl -s -X POST $API_URL/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "assess_risk",
    "params": {
      "title": "Payment system upgrade",
      "description": "Upgrade payment processing to new provider",
      "systems": ["payment", "financial"],
      "urgency": "normal"
    }
  }')

if echo "$RISK_ASSESS" | jq -e '.risk_score' > /dev/null; then
    SCORE=$(echo "$RISK_ASSESS" | jq -r '.risk_score')
    RECOMMENDATION=$(echo "$RISK_ASSESS" | jq -r '.recommendation')
    echo -e "${GREEN}✓${NC} Risk assessment completed"
    echo "   Score: $SCORE"
    echo "   Recommendation: $RECOMMENDATION"
else
    echo -e "${RED}✗${NC} Risk assessment failed"
    exit 1
fi
echo ""

# Summary
echo "═══════════════════════════════════════════════════════════"
echo -e "${GREEN}✓ All tests passed!${NC}"
echo ""
echo "Your MCP server is ready for Claude.ai Custom Connector demo."
echo ""
echo "Next steps:"
echo "1. Go to https://claude.ai/settings/connectors"
echo "2. Add custom connector with URL: $API_URL"
echo "3. Start chatting with your ITIL change management system!"
echo "═══════════════════════════════════════════════════════════"