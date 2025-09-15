#!/bin/bash
# Parallel validation script for both staging and production
# Ensures v1.4.4 is properly deployed and functional

set -e

echo "🔍 Guile ChangeFlow Parallel Validation"
echo "Expected Version: 1.4.4"
echo "Time: $(date +%H:%M:%S)"
echo "════════════════════════════════════════════════"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Configuration
EXPECTED_VERSION="1.4.4"
STAGING_URL="https://staging.mcp.changeflow.us"
PROD_URL="https://mcp.changeflow.us"

# Validation function
validate_environment() {
    local env_name=$1
    local base_url=$2
    local errors=0

    echo ""
    echo "[$env_name] Starting validation suite..."
    echo "[$env_name] URL: $base_url"
    echo "----------------------------------------"

    # 1. Health check
    echo -n "[$env_name] Health check... "
    local health_status=$(curl -s -o /dev/null -w "%{http_code}" "$base_url/health" 2>/dev/null)
    if [ "$health_status" = "200" ]; then
        echo -e "${GREEN}✅ OK${NC}"
    else
        echo -e "${RED}❌ FAILED (HTTP $health_status)${NC}"
        ((errors++))
    fi

    # 2. Version verification
    echo -n "[$env_name] Version check... "
    local version=$(curl -s "$base_url/version" 2>/dev/null | jq -r .version)
    if [ "$version" = "$EXPECTED_VERSION" ]; then
        echo -e "${GREEN}✅ v${version}${NC}"
    else
        echo -e "${RED}❌ MISMATCH (got: ${version}, expected: ${EXPECTED_VERSION})${NC}"
        ((errors++))
    fi

    # 3. MCP Protocol test
    echo -n "[$env_name] MCP protocol... "
    local mcp_response=$(curl -s -X POST "$base_url/mcp" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"1.0.0"},"id":1}' 2>/dev/null)

    if echo "$mcp_response" | jq -e .result &>/dev/null; then
        echo -e "${GREEN}✅ OK${NC}"
    else
        echo -e "${RED}❌ FAILED${NC}"
        ((errors++))
    fi

    # 4. Tools availability
    echo -n "[$env_name] MCP tools... "
    local tools_response=$(curl -s -X POST "$base_url/mcp" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}' 2>/dev/null)

    local tools_count=$(echo "$tools_response" | jq '.result.tools | length' 2>/dev/null)
    if [ "$tools_count" -gt 0 ]; then
        echo -e "${GREEN}✅ ${tools_count} tools available${NC}"
    else
        echo -e "${RED}❌ No tools found${NC}"
        ((errors++))
    fi

    # 5. Demo data check
    echo -n "[$env_name] Demo data... "
    local demo_response=$(curl -s "$base_url/api/demo-changes" 2>/dev/null)
    local demo_count=$(echo "$demo_response" | jq '. | length' 2>/dev/null)

    if [ "$demo_count" = "100" ]; then
        echo -e "${GREEN}✅ 100 PRs${NC}"
    else
        echo -e "${YELLOW}⚠️ ${demo_count} PRs (expected 100)${NC}"
        # Not a critical error
    fi

    # 6. Response time
    echo -n "[$env_name] Response time... "
    local response_time=$(curl -s -w "%{time_total}" -o /dev/null "$base_url/health" 2>/dev/null)
    local response_ms=$(echo "$response_time * 1000" | bc | cut -d. -f1)

    if [ "$response_ms" -lt 500 ]; then
        echo -e "${GREEN}✅ ${response_ms}ms${NC}"
    elif [ "$response_ms" -lt 1000 ]; then
        echo -e "${YELLOW}⚠️ ${response_ms}ms (slow)${NC}"
    else
        echo -e "${RED}❌ ${response_ms}ms (too slow)${NC}"
        ((errors++))
    fi

    # 7. Error rate check
    echo -n "[$env_name] Error rate... "
    local metrics=$(curl -s "$base_url/metrics" 2>/dev/null)
    local error_rate=$(echo "$metrics" | jq -r .error_rate 2>/dev/null)

    if [ -n "$error_rate" ] && (( $(echo "$error_rate < 0.001" | bc -l) )); then
        echo -e "${GREEN}✅ ${error_rate}%${NC}"
    else
        echo -e "${YELLOW}⚠️ ${error_rate:-unknown}%${NC}"
    fi

    # Summary
    echo "----------------------------------------"
    if [ $errors -eq 0 ]; then
        echo -e "[$env_name] ${GREEN}✅ All validations passed${NC}"
        return 0
    else
        echo -e "[$env_name] ${RED}❌ ${errors} validation(s) failed${NC}"
        return 1
    fi
}

# Production-specific enhanced validation
validate_production_enhanced() {
    echo ""
    echo "🔒 Enhanced Production Validation"
    echo "════════════════════════════════════════"

    # 1. Verify exact version in production
    echo -n "Production version confirmation... "
    local prod_version=$(curl -s "$PROD_URL/version" | jq -r .version)
    if [ "$prod_version" = "$EXPECTED_VERSION" ]; then
        echo -e "${GREEN}✅ Confirmed v${prod_version}${NC}"
    else
        echo -e "${RED}❌ Version mismatch: ${prod_version}${NC}"
        return 1
    fi

    # 2. Check rollback readiness
    echo -n "Rollback mechanism... "
    local rollback_status=$(curl -s "$PROD_URL/api/rollback/status" 2>/dev/null | jq -r .ready)
    if [ "$rollback_status" = "true" ]; then
        echo -e "${GREEN}✅ Ready${NC}"
    else
        echo -e "${YELLOW}⚠️ Not ready${NC}"
    fi

    # 3. Verify ITIL compliance endpoints
    echo -n "ITIL endpoints... "
    local itil_health=$(curl -s -o /dev/null -w "%{http_code}" "$PROD_URL/api/itil/health" 2>/dev/null)
    if [ "$itil_health" = "200" ]; then
        echo -e "${GREEN}✅ Operational${NC}"
    else
        echo -e "${YELLOW}⚠️ Not available${NC}"
    fi

    # 4. Load test (light)
    echo -n "Load test (10 requests)... "
    local total_time=0
    for i in {1..10}; do
        local req_time=$(curl -s -w "%{time_total}" -o /dev/null "$PROD_URL/health" 2>/dev/null)
        total_time=$(echo "$total_time + $req_time" | bc)
    done
    local avg_time=$(echo "scale=3; $total_time / 10 * 1000" | bc)
    echo -e "${GREEN}✅ Avg: ${avg_time}ms${NC}"

    echo "════════════════════════════════════════"
    echo -e "${GREEN}🎉 Production validation complete${NC}"
}

# Main execution
main() {
    # Run validations in parallel
    validate_environment "STAGING" "$STAGING_URL" &
    STG_PID=$!

    validate_environment "PRODUCTION" "$PROD_URL" &
    PRD_PID=$!

    # Wait for both to complete
    wait $STG_PID
    STG_RESULT=$?

    wait $PRD_PID
    PRD_RESULT=$?

    # Run enhanced production validation
    if [ $PRD_RESULT -eq 0 ]; then
        validate_production_enhanced
        PRD_ENHANCED=$?
    else
        PRD_ENHANCED=1
    fi

    # Final summary
    echo ""
    echo "════════════════════════════════════════"
    echo "📊 VALIDATION SUMMARY"
    echo "════════════════════════════════════════"
    echo -e "Staging:    $([ $STG_RESULT -eq 0 ] && echo '${GREEN}✅ PASS${NC}' || echo '${RED}❌ FAIL${NC}')"
    echo -e "Production: $([ $PRD_RESULT -eq 0 ] && echo '${GREEN}✅ PASS${NC}' || echo '${RED}❌ FAIL${NC}')"
    echo -e "Enhanced:   $([ $PRD_ENHANCED -eq 0 ] && echo '${GREEN}✅ PASS${NC}' || echo '${RED}❌ FAIL${NC}')"
    echo "════════════════════════════════════════"

    # Exit with appropriate code
    if [[ $STG_RESULT -eq 0 && $PRD_RESULT -eq 0 && $PRD_ENHANCED -eq 0 ]]; then
        echo -e "${GREEN}✅ All validations passed successfully${NC}"
        exit 0
    else
        echo -e "${RED}⚠️ Some validations failed${NC}"
        exit 1
    fi
}

# Run main
main "$@"