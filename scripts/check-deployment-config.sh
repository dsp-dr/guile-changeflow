#!/usr/bin/env bash

# Check deployment configuration for missing values
# This script warns about environment variables that need manual intervention

echo "🔍 Checking ChangeFlow Deployment Configuration"
echo "================================================"

CONFIG_FILE="infra/cloudflare/wrangler.toml"
REQUIRED_VARS=(
    "GITHUB_CLIENT_ID"
    "ENVIRONMENT"
    "LOG_LEVEL"
)

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if wrangler.toml exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo -e "${RED}❌ ERROR: $CONFIG_FILE not found!${NC}"
    exit 1
fi

echo "📋 Checking required environment variables in wrangler.toml:"
echo ""

MISSING_COUNT=0
WARNING_COUNT=0

for var in "${REQUIRED_VARS[@]}"; do
    if grep -q "^$var = " "$CONFIG_FILE"; then
        VALUE=$(grep "^$var = " "$CONFIG_FILE" | cut -d'"' -f2)
        echo -e "${GREEN}✅ $var = $VALUE${NC}"
    elif grep -q "$var = " "$CONFIG_FILE"; then
        echo -e "${GREEN}✅ $var is defined in [vars] section${NC}"
    else
        echo -e "${RED}❌ $var is MISSING from wrangler.toml${NC}"
        MISSING_COUNT=$((MISSING_COUNT + 1))
    fi
done

echo ""
echo "📋 Checking Cloudflare Dashboard Requirements:"
echo ""

# Check for secrets that must be in dashboard
echo -e "${YELLOW}⚠️  GITHUB_CLIENT_SECRET must be set as Secret in Cloudflare dashboard${NC}"
echo "   This cannot be in wrangler.toml for security reasons"
echo ""

# Check current production values
echo "🌐 Testing production endpoints:"
echo ""

# Test OAuth endpoint
OAUTH_STATUS=$(curl -s -o /dev/null -w "%{http_code}" https://mcp.changeflow.us/authorize)
if [ "$OAUTH_STATUS" = "302" ]; then
    echo -e "${GREEN}✅ OAuth endpoint working (302 redirect)${NC}"
elif [ "$OAUTH_STATUS" = "500" ]; then
    echo -e "${RED}❌ OAuth endpoint ERROR - GITHUB_CLIENT_ID missing!${NC}"
    echo -e "${YELLOW}   ACTION REQUIRED: Add GITHUB_CLIENT_ID to wrangler.toml or dashboard${NC}"
    WARNING_COUNT=$((WARNING_COUNT + 1))
else
    echo -e "${YELLOW}⚠️  OAuth endpoint returned: $OAUTH_STATUS${NC}"
fi

# Test SSE endpoint
SSE_STATUS=$(curl -s -o /dev/null -w "%{http_code}" -H "Authorization: Bearer test" https://mcp.changeflow.us/v1/sse)
if [ "$SSE_STATUS" = "200" ]; then
    echo -e "${GREEN}✅ SSE endpoint working (200 OK)${NC}"
else
    echo -e "${YELLOW}⚠️  SSE endpoint returned: $SSE_STATUS${NC}"
fi

echo ""
echo "================================================"

if [ $MISSING_COUNT -gt 0 ]; then
    echo -e "${RED}❌ DEPLOYMENT WILL FAIL: $MISSING_COUNT required variables missing${NC}"
    echo ""
    echo "Fix by adding to $CONFIG_FILE [vars] section:"
    echo "GITHUB_CLIENT_ID = \"Ov23lir2JJgJffb51RPs\""
    exit 1
elif [ $WARNING_COUNT -gt 0 ]; then
    echo -e "${YELLOW}⚠️  MANUAL INTERVENTION REQUIRED${NC}"
    echo ""
    echo "1. Ensure GITHUB_CLIENT_SECRET is set in Cloudflare dashboard (Secret)"
    echo "2. Verify GITHUB_CLIENT_ID = Ov23lir2JJgJffb51RPs"
    exit 1
else
    echo -e "${GREEN}✅ Configuration looks good!${NC}"
    echo ""
    echo "Remember to set in Cloudflare Dashboard:"
    echo "  - GITHUB_CLIENT_SECRET (as Secret)"
fi

echo ""
echo "📝 Current wrangler.toml [vars] section:"
sed -n '/\[vars\]/,/^$/p' "$CONFIG_FILE" | head -10