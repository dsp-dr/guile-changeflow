#!/usr/bin/env bash

# Production Monitoring Script for MCP Deployment
# Continuous monitoring of production endpoint health and functionality

ENDPOINT="${CLOUDFLARE_WORKER_URL:-https://api.changeflow.us}"
CHECK_INTERVAL="${MONITOR_INTERVAL:-30}"
ALERT_THRESHOLD="${ALERT_THRESHOLD:-3}"
LOG_FILE="production-monitor-$(date +%Y%m%d).log"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Counters
FAILURES=0
TOTAL_CHECKS=0
START_TIME=$(date +%s)

log() {
    echo "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] $1" | tee -a "$LOG_FILE"
}

check_health() {
    local response=$(curl -s --max-time 10 "$ENDPOINT/" 2>/dev/null)
    local exit_code=$?

    if [[ $exit_code -ne 0 ]]; then
        return 1
    fi

    local status=$(echo "$response" | jq -r '.status // "unknown"' 2>/dev/null)
    [[ "$status" == "healthy" ]]
}

check_mcp_tools() {
    local response=$(curl -s --max-time 10 -X POST "$ENDPOINT/" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' 2>/dev/null)
    local exit_code=$?

    if [[ $exit_code -ne 0 ]]; then
        return 1
    fi

    local tools_count=$(echo "$response" | jq '.result.tools | length // 0' 2>/dev/null)
    [[ "$tools_count" -eq 8 ]]
}

check_performance() {
    local start_time=$(date +%s%3N)
    curl -s --max-time 5 "$ENDPOINT/" > /dev/null 2>&1
    local exit_code=$?
    local end_time=$(date +%s%3N)
    local duration=$((end_time - start_time))

    if [[ $exit_code -ne 0 ]]; then
        return 1
    fi

    # Pass if under 1000ms
    [[ $duration -lt 1000 ]]
}

send_alert() {
    local message="$1"
    log "🚨 ALERT: $message"

    # Could integrate with Slack, email, etc.
    # For now, just log prominently
    echo -e "${RED}🚨 PRODUCTION ALERT 🚨${NC}"
    echo -e "${RED}$message${NC}"
}

run_checks() {
    ((TOTAL_CHECKS++))
    local check_time=$(date +%s)
    local errors=0

    # Health check
    if check_health; then
        echo -e "${GREEN}✅ Health: OK${NC}"
    else
        echo -e "${RED}❌ Health: FAIL${NC}"
        ((errors++))
    fi

    # MCP tools check
    if check_mcp_tools; then
        echo -e "${GREEN}✅ MCP Tools: OK (8 available)${NC}"
    else
        echo -e "${RED}❌ MCP Tools: FAIL${NC}"
        ((errors++))
    fi

    # Performance check
    if check_performance; then
        echo -e "${GREEN}✅ Performance: OK${NC}"
    else
        echo -e "${YELLOW}⚠️ Performance: SLOW${NC}"
        # Don't count as failure, just warning
    fi

    if [[ $errors -gt 0 ]]; then
        ((FAILURES++))
        log "❌ Check failed ($errors errors) - Total failures: $FAILURES"

        if [[ $FAILURES -ge $ALERT_THRESHOLD ]]; then
            send_alert "Production endpoint failing - $FAILURES consecutive failures"
        fi
    else
        FAILURES=0  # Reset failure count on success
        log "✅ All checks passed"
    fi

    # Calculate uptime stats
    local uptime=$((check_time - START_TIME))
    local success_rate=$((($TOTAL_CHECKS - $FAILURES) * 100 / $TOTAL_CHECKS))

    echo "📊 Stats: ${TOTAL_CHECKS} checks, ${success_rate}% success, ${uptime}s uptime"
}

main() {
    echo "🔍 Starting production monitoring..."
    echo "📡 Endpoint: $ENDPOINT"
    echo "⏱️ Check interval: ${CHECK_INTERVAL}s"
    echo "🚨 Alert threshold: $ALERT_THRESHOLD failures"
    echo "📝 Log file: $LOG_FILE"
    echo ""

    log "Production monitoring started - PID: $$"

    while true; do
        echo "⏰ $(date -u +%H:%M:%S) UTC - Running checks..."
        run_checks
        echo ""
        sleep "$CHECK_INTERVAL"
    done
}

# Handle signals gracefully
trap 'echo ""; log "Monitoring stopped"; exit 0' INT TERM

# Start monitoring
main "$@"