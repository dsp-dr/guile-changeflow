#!/usr/bin/env bash

# Deployment Validation Script for Guile ChangeFlow MCP Implementation
# Tests before/after deployment states and validates functionality

set -e

# Configuration
PROD_ENDPOINT="${CLOUDFLARE_WORKER_URL:-https://api.changeflow.us}"
LOCAL_ENDPOINT="http://localhost:8080"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
LOG_FILE="deployment-validation-${TIMESTAMP}.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$LOG_FILE"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $1" | tee -a "$LOG_FILE"; }
log_warning() { echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$LOG_FILE"; }
log_error() { echo -e "${RED}[FAIL]${NC} $1" | tee -a "$LOG_FILE"; }

# Test results tracking
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Test execution wrapper
run_test() {
    local test_name="$1"
    local test_command="$2"
    local endpoint="$3"

    log_info "Testing: $test_name on $endpoint"

    if eval "$test_command"; then
        log_success "$test_name"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "$test_name"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Health check test
test_health_check() {
    local endpoint="$1"
    local response=$(curl -s "$endpoint/" || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local status=$(echo "$response" | jq -r '.status // "unknown"')
    local message=$(echo "$response" | jq -r '.message // "unknown"')

    log_info "  Status: $status"
    log_info "  Message: $message"

    [[ "$status" == "healthy" ]]
}

# MCP Protocol validation
test_mcp_initialize() {
    local endpoint="$1"
    local response=$(curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05"},"id":1}' || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local protocol_version=$(echo "$response" | jq -r '.result.protocolVersion // "unknown"')
    local server_name=$(echo "$response" | jq -r '.result.serverInfo.name // "unknown"')

    log_info "  Protocol Version: $protocol_version"
    log_info "  Server Name: $server_name"

    [[ "$protocol_version" == "2024-11-05" ]]
}

# Tools validation
test_tools_list() {
    local endpoint="$1"
    local response=$(curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}' || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local tools_count=$(echo "$response" | jq '.result.tools | length // 0')

    log_info "  Tools Available: $tools_count"

    # Expected 8 tools in new implementation, 0 in old
    if [[ "$tools_count" -eq 8 ]]; then
        log_info "  New MCP implementation detected"
        return 0
    elif [[ "$tools_count" -eq 0 ]]; then
        log_info "  Legacy implementation detected"
        return 1
    else
        log_warning "  Unexpected tool count: $tools_count"
        return 1
    fi
}

# Test specific ITIL tool
test_tool_execution() {
    local endpoint="$1"
    local response=$(curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"assess_change_risk","arguments":{"change_type":"test","environment":"test"}},"id":3}' || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local risk_score=$(echo "$response" | jq '.result.risk_score // null')
    local risk_level=$(echo "$response" | jq -r '.result.risk_level // "unknown"')

    log_info "  Risk Score: $risk_score"
    log_info "  Risk Level: $risk_level"

    [[ "$risk_score" != "null" && "$risk_level" != "unknown" ]]
}

# Test MCP resources
test_resources() {
    local endpoint="$1"
    local response=$(curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"resources/list","params":{},"id":4}' || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local resources_count=$(echo "$response" | jq '.result.resources | length // 0')

    log_info "  Resources Available: $resources_count"

    [[ "$resources_count" -eq 5 ]]
}

# Test MCP prompts
test_prompts() {
    local endpoint="$1"
    local response=$(curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"prompts/list","params":{},"id":5}' || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local prompts_count=$(echo "$response" | jq '.result.prompts | length // 0')

    log_info "  Prompts Available: $prompts_count"

    [[ "$prompts_count" -eq 3 ]]
}

# Performance test
test_performance() {
    local endpoint="$1"
    local start_time=$(date +%s%3N)

    curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":6}' > /dev/null

    local end_time=$(date +%s%3N)
    local duration=$((end_time - start_time))

    log_info "  Response Time: ${duration}ms"

    # Pass if under 1000ms (very generous for network calls)
    [[ $duration -lt 1000 ]]
}

# Error handling test
test_error_handling() {
    local endpoint="$1"
    local response=$(curl -s -X POST "$endpoint/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"invalid_method","params":{},"id":7}' || echo "ERROR")

    if [[ "$response" == "ERROR" ]]; then
        return 1
    fi

    local error_code=$(echo "$response" | jq '.result.error.code // null')

    log_info "  Error Code: $error_code"

    [[ "$error_code" == "-32601" ]]
}

# Comprehensive test suite
run_test_suite() {
    local endpoint="$1"
    local suite_name="$2"

    echo ""
    log_info "========================================="
    log_info "Running Test Suite: $suite_name"
    log_info "Endpoint: $endpoint"
    log_info "Timestamp: $TIMESTAMP"
    log_info "========================================="

    # Core functionality tests
    run_test "Health Check" "test_health_check '$endpoint'" "$endpoint"
    run_test "MCP Initialize" "test_mcp_initialize '$endpoint'" "$endpoint"
    run_test "Tools List" "test_tools_list '$endpoint'" "$endpoint"
    run_test "Tool Execution" "test_tool_execution '$endpoint'" "$endpoint"
    run_test "Resources List" "test_resources '$endpoint'" "$endpoint"
    run_test "Prompts List" "test_prompts '$endpoint'" "$endpoint"
    run_test "Performance" "test_performance '$endpoint'" "$endpoint"
    run_test "Error Handling" "test_error_handling '$endpoint'" "$endpoint"
}

# Deployment comparison
compare_deployments() {
    log_info "========================================="
    log_info "DEPLOYMENT COMPARISON ANALYSIS"
    log_info "========================================="

    # Get current production state
    local prod_response=$(curl -s "$PROD_ENDPOINT/" || echo "ERROR")
    local prod_version=$(echo "$prod_response" | jq -r '.version // "unknown"')
    local prod_message=$(echo "$prod_response" | jq -r '.message // "unknown"')

    log_info "BEFORE (Production):"
    log_info "  Version: $prod_version"
    log_info "  Message: $prod_message"

    # Check if it's the old or new implementation
    local tools_response=$(curl -s -X POST "$PROD_ENDPOINT/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' || echo "ERROR")

    if [[ "$tools_response" != "ERROR" ]]; then
        local tools_count=$(echo "$tools_response" | jq '.result.tools | length // 0' 2>/dev/null || echo "0")
        log_info "  Tools Count: $tools_count"

        if [[ "$tools_count" == "8" ]]; then
            log_success "Production is already running NEW implementation"
        else
            log_warning "Production is running OLD implementation"
        fi
    else
        log_error "Production MCP protocol not responding"
    fi
}

# Generate deployment report
generate_report() {
    local report_file="deployment-report-${TIMESTAMP}.md"

    cat > "$report_file" << EOF
# Deployment Validation Report

**Date:** $TIMESTAMP
**Production Endpoint:** $PROD_ENDPOINT

## Test Results Summary

- ✅ **Passed:** $TESTS_PASSED
- ❌ **Failed:** $TESTS_FAILED
- ⏭️ **Skipped:** $TESTS_SKIPPED
- 📊 **Total:** $((TESTS_PASSED + TESTS_FAILED + TESTS_SKIPPED))

## Overall Status

EOF

    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo "🎉 **DEPLOYMENT SUCCESSFUL** - All tests passing" >> "$report_file"
    else
        echo "💥 **DEPLOYMENT ISSUES DETECTED** - $TESTS_FAILED tests failing" >> "$report_file"
    fi

    cat >> "$report_file" << EOF

## Detailed Results

See full logs in: \`$LOG_FILE\`

## Next Steps

EOF

    if [[ $TESTS_FAILED -eq 0 ]]; then
        cat >> "$report_file" << EOF
- ✅ Deployment validation successful
- ✅ MCP protocol fully functional
- ✅ All ITIL tools operational
- ✅ Ready for 7 AM demo

**Recommendation:** Proceed with confidence
EOF
    else
        cat >> "$report_file" << EOF
- ❌ Issues detected in deployment
- 🔄 Consider rollback if critical
- 🔍 Investigate failed tests
- ⏰ May need delay for 7 AM demo

**Recommendation:** Review failures before proceeding
EOF
    fi

    log_info "Report generated: $report_file"
}

# Main execution
main() {
    local command="${1:-full}"

    case "$command" in
        "pre-merge")
            log_info "PRE-MERGE BASELINE TEST"
            compare_deployments
            run_test_suite "$PROD_ENDPOINT" "Pre-Merge Production Baseline"
            ;;
        "post-merge")
            log_info "POST-MERGE VALIDATION TEST"
            compare_deployments
            run_test_suite "$PROD_ENDPOINT" "Post-Merge Production Validation"
            ;;
        "local")
            log_info "LOCAL DEVELOPMENT TEST"
            run_test_suite "$LOCAL_ENDPOINT" "Local Development Server"
            ;;
        "full")
            log_info "COMPREHENSIVE DEPLOYMENT VALIDATION"
            compare_deployments

            log_info ""
            log_info "Testing LOCAL server first..."
            if curl -s "$LOCAL_ENDPOINT/" > /dev/null 2>&1; then
                run_test_suite "$LOCAL_ENDPOINT" "Local Development Server"
            else
                log_warning "Local server not running at $LOCAL_ENDPOINT"
                ((TESTS_SKIPPED += 8))
            fi

            log_info ""
            log_info "Testing PRODUCTION server..."
            run_test_suite "$PROD_ENDPOINT" "Production Server"
            ;;
        *)
            echo "Usage: $0 [pre-merge|post-merge|local|full]"
            echo ""
            echo "Commands:"
            echo "  pre-merge   - Test current production state before merge"
            echo "  post-merge  - Validate production after deployment"
            echo "  local       - Test local development server"
            echo "  full        - Test both local and production (default)"
            exit 1
            ;;
    esac

    echo ""
    log_info "========================================="
    log_info "TEST EXECUTION COMPLETE"
    log_info "========================================="
    log_info "Passed: $TESTS_PASSED"
    log_info "Failed: $TESTS_FAILED"
    log_info "Skipped: $TESTS_SKIPPED"

    if [[ $TESTS_FAILED -eq 0 ]]; then
        log_success "ALL TESTS PASSED ✅"
        generate_report
        exit 0
    else
        log_error "$TESTS_FAILED TESTS FAILED ❌"
        generate_report
        exit 1
    fi
}

# Execute main function with all arguments
main "$@"