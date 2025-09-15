#!/bin/bash
# Version validation script for Guile ChangeFlow
# Checks that all version numbers are synchronized

set -e

echo "🔍 Version Validation Script"
echo "════════════════════════════════════════"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Expected version from package.json
PACKAGE_VERSION=$(grep '"version"' package.json | head -1 | cut -d'"' -f4)
echo "Package version: $PACKAGE_VERSION"

# Check worker.js
WORKER_VERSION=$(grep "SERVER_VERSION = " infra/cloudflare/worker.js | cut -d"'" -f2)
echo "Worker version:  $WORKER_VERSION"

# Check deployed versions
check_deployed_version() {
    local env_name=$1
    local url=$2

    echo -n "[$env_name] Checking version... "

    # Method 1: Check /version endpoint
    local api_version=$(curl -s "$url/version" 2>/dev/null | jq -r .version)

    # Method 2: Parse HTML page with the new selector
    local html_version=$(curl -s "$url/" 2>/dev/null | grep -o 'id="semver">[^<]*' | cut -d'>' -f2)

    # Method 3: Check health endpoint
    local health_version=$(curl -s "$url/health" 2>/dev/null | jq -r .version)

    # Display results
    if [ "$api_version" = "$PACKAGE_VERSION" ]; then
        echo -e "${GREEN}✅ $api_version${NC}"
        return 0
    elif [ "$health_version" = "$PACKAGE_VERSION" ]; then
        echo -e "${GREEN}✅ $health_version (via health)${NC}"
        return 0
    elif [ "$html_version" = "$PACKAGE_VERSION" ]; then
        echo -e "${GREEN}✅ $html_version (via HTML)${NC}"
        return 0
    else
        echo -e "${RED}❌ Mismatch: API=$api_version, HTML=$html_version, Health=$health_version${NC}"
        return 1
    fi
}

# Validate local versions match
echo ""
echo "Local Version Check:"
if [ "$PACKAGE_VERSION" = "$WORKER_VERSION" ]; then
    echo -e "${GREEN}✅ Local versions match${NC}"
else
    echo -e "${RED}❌ Version mismatch!${NC}"
    echo "  package.json: $PACKAGE_VERSION"
    echo "  worker.js:    $WORKER_VERSION"
    exit 1
fi

# Check deployed versions (if available)
echo ""
echo "Deployed Version Check:"

# Check staging
if curl -s -o /dev/null -w "%{http_code}" https://staging.mcp.changeflow.us/health 2>/dev/null | grep -q "200"; then
    check_deployed_version "STAGING" "https://staging.mcp.changeflow.us"
else
    echo "[STAGING] Not accessible"
fi

# Check production
if curl -s -o /dev/null -w "%{http_code}" https://mcp.changeflow.us/health 2>/dev/null | grep -q "200"; then
    check_deployed_version "PRODUCTION" "https://mcp.changeflow.us"
else
    echo "[PRODUCTION] Not accessible"
fi

# Automation-friendly output
echo ""
echo "════════════════════════════════════════"
echo "Automation Data (JSON):"
cat <<EOF
{
  "package_version": "$PACKAGE_VERSION",
  "worker_version": "$WORKER_VERSION",
  "versions_match": $([ "$PACKAGE_VERSION" = "$WORKER_VERSION" ] && echo "true" || echo "false"),
  "semver": {
    "major": $(echo $PACKAGE_VERSION | cut -d. -f1),
    "minor": $(echo $PACKAGE_VERSION | cut -d. -f2),
    "patch": $(echo $PACKAGE_VERSION | cut -d. -f3)
  }
}
EOF

# Examples of how to extract version
echo ""
echo "════════════════════════════════════════"
echo "Extraction Examples:"
echo ""
echo "1. From HTML (CSS selector):"
echo "   document.querySelector('#semver').textContent"
echo "   // Returns: '$PACKAGE_VERSION'"
echo ""
echo "2. From HTML (data attribute):"
echo "   document.querySelector('[data-version]').dataset.version"
echo "   // Returns: '$PACKAGE_VERSION'"
echo ""
echo "3. From API:"
echo "   curl https://mcp.changeflow.us/version | jq -r .version"
echo "   // Returns: '$PACKAGE_VERSION'"
echo ""
echo "4. From Health:"
echo "   curl https://mcp.changeflow.us/health | jq -r .version"
echo "   // Returns: '$PACKAGE_VERSION'"

exit 0