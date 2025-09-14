#!/usr/bin/env bash

# Integration Test Runner for Guile ChangeFlow
# Agent 5 - Ensuring 99.97% uptime for 7 AM demo

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

echo "╔═══════════════════════════════════════════════╗"
echo "║   GUILE CHANGEFLOW INTEGRATION TEST RUNNER   ║"
echo "║           Target: 99.97% Uptime              ║"
echo "╚═══════════════════════════════════════════════╝"
echo

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Check for required services
check_service() {
    local port=$1
    local name=$2
    if nc -z localhost $port 2>/dev/null; then
        echo -e "${GREEN}✓${NC} $name is running on port $port"
        return 0
    else
        echo -e "${RED}✗${NC} $name is NOT running on port $port"
        return 1
    fi
}

# Service check function
check_all_services() {
    local all_up=true

    echo "Checking services..."
    check_service 8080 "Web Server" || all_up=false
    check_service 8081 "MCP Server" || all_up=false
    check_service 8082 "Webhook Server" || all_up=false

    if $all_up; then
        echo -e "${GREEN}All services are running!${NC}"
        return 0
    else
        echo -e "${YELLOW}Some services are not running${NC}"
        return 1
    fi
}

# Start services if not running
start_services() {
    echo -e "${BLUE}Starting services...${NC}"

    # Check if services need starting
    if ! check_all_services; then
        echo "Please start the services in separate terminals:"
        echo "  Terminal 1: guile -L . -L ./src src/main.scm --server"
        echo "  Terminal 2: guile -L . -L ./src mcp-server.scm"
        echo "  Terminal 3: guile -L . -L ./src web-server.scm"
        echo
        echo "Waiting for services to start..."

        # Wait up to 30 seconds for services
        for i in {1..30}; do
            sleep 1
            if check_all_services 2>/dev/null; then
                echo -e "${GREEN}Services are now ready!${NC}"
                break
            fi
            if [ $i -eq 30 ]; then
                echo -e "${RED}Timeout waiting for services${NC}"
                exit 1
            fi
        done
    fi
}

# Run specific test phase
run_test_phase() {
    local phase=$1
    local test_file="$PROJECT_ROOT/test/integration-tests.scm"

    echo
    echo -e "${BLUE}Running $phase...${NC}"

    case $phase in
        "smoke")
            guile -L . -L ./src -c "(use-modules (test integration-tests)) (run-smoke-tests)"
            ;;
        "integration")
            guile -L . -L ./src -c "(use-modules (test integration-tests)) (run-integration-tests)"
            ;;
        "chaos")
            guile -L . -L ./src -s test/chaos-scenarios.scm
            ;;
        "demo")
            guile -L . -L ./src -c "(use-modules (test integration-tests)) (run-demo-dryrun)"
            ;;
        "all")
            guile -L . -L ./src -s test/integration-tests.scm
            ;;
        *)
            echo "Unknown test phase: $phase"
            return 1
            ;;
    esac
}

# Main execution
main() {
    local test_phase="${1:-all}"

    echo "Test phase: $test_phase"
    echo "Time: $(date '+%Y-%m-%d %H:%M:%S')"
    echo

    # Check/start services
    start_services

    # Run tests
    run_test_phase "$test_phase"

    # Show results
    if [ $? -eq 0 ]; then
        echo
        echo -e "${GREEN}╔═══════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║            TESTS PASSED!                     ║${NC}"
        echo -e "${GREEN}║         Demo Ready for 7 AM                  ║${NC}"
        echo -e "${GREEN}╚═══════════════════════════════════════════════╝${NC}"
    else
        echo
        echo -e "${RED}╔═══════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║            TESTS FAILED!                     ║${NC}"
        echo -e "${RED}║        Fix issues before demo                ║${NC}"
        echo -e "${RED}╚═══════════════════════════════════════════════╝${NC}"
        exit 1
    fi
}

# Handle arguments
case "${1:-}" in
    smoke|integration|chaos|demo|all)
        main "$1"
        ;;
    --help|-h)
        echo "Usage: $0 [smoke|integration|chaos|demo|all]"
        echo
        echo "Test phases:"
        echo "  smoke       - Run component smoke tests"
        echo "  integration - Run integration tests"
        echo "  chaos       - Run chaos engineering tests"
        echo "  demo        - Run demo dry run"
        echo "  all         - Run complete test suite (default)"
        ;;
    *)
        main "all"
        ;;
esac