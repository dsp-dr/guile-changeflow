#!/bin/bash
# run-scenario.sh - Execute demo scenarios for recording

set -e

SCENARIO=$1
PR_COUNT=${2:-10}
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../" && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

# Export Guile path
export GUILE_LOAD_PATH="$PROJECT_ROOT/src:$GUILE_LOAD_PATH"
export GUILE_AUTO_COMPILE=0

function print_banner() {
    clear
    echo -e "${BLUE}"
    cat << 'EOF'
 _____ _______ _____ _         _____                   
|_   _|__   __|_   _| |       |  __ \                  
  | |    | |    | | | |       | |  | | ___ _ __ ___   ___  
  | |    | |    | | | |       | |  | |/ _ \ '_ ` _ \ / _ \ 
 _| |_   | |   _| |_| |____   | |__| |  __/ | | | | | (_) |
|_____|  |_|  |_____|______|  |_____/ \___|_| |_| |_|\___/ 
                                                            
Change Management Pipeline Simulator v1.0
EOF
    echo -e "${NC}\n"
    echo -e "${YELLOW}Scenario: ${SCENARIO}${NC}"
    echo -e "${YELLOW}Processing: ${PR_COUNT} PRs${NC}"
    echo -e "${YELLOW}Time: $(date '+%Y-%m-%d %H:%M:%S')${NC}"
    echo "════════════════════════════════════════════════════════════"
    echo ""
    sleep 2
}

function run_quick_demo() {
    echo -e "${GREEN}🚀 Running Quick Demo (5 PRs)${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    guile -c "
    (use-modules (simulator deployment-pipeline)
                 (simulator data-export)
                 (ice-9 format))
    
    (format #t \"~%📊 Loading pre-generated demo data...~%\")
    (let ((prs (load-prs-from-json \"data/demo-quick.json\")))
      (format #t \"✅ Loaded ~a PRs from demo dataset~%~%\" (length prs))
      (simulate-deployment-pipeline prs 5))
    "
}

function run_frontend_demo() {
    echo -e "${GREEN}🎨 Running Frontend Demo (10 PRs)${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    guile -c "
    (use-modules (simulator deployment-pipeline)
                 (simulator data-export)
                 (ice-9 format))
    
    (format #t \"~%🎨 Frontend Component Focus~%\")
    (format #t \"UI changes, responsive fixes, feature toggles~%~%\")
    
    (let ((prs (load-prs-from-json \"data/demo-frontend-only.json\")))
      (format #t \"✅ Loaded ~a frontend PRs~%~%\" (length prs))
      (simulate-deployment-pipeline prs 10))
    "
}

function run_backend_demo() {
    echo -e "${GREEN}⚙️  Running Backend Demo (15 PRs)${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    guile -c "
    (use-modules (simulator deployment-pipeline)
                 (simulator data-export)
                 (ice-9 format))
    
    (format #t \"~%⚙️  Backend Services Focus~%\")
    (format #t \"API endpoints, database migrations, business logic~%~%\")
    
    (let ((prs (load-prs-from-json \"data/demo-backend-focus.json\")))
      (format #t \"✅ Loaded ~a backend PRs~%~%\" (length prs))
      (simulate-deployment-pipeline prs 15))
    "
}

function run_infra_demo() {
    echo -e "${GREEN}🏗️  Running Infrastructure Demo (8 PRs)${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    guile -c "
    (use-modules (simulator deployment-pipeline)
                 (simulator data-export)
                 (ice-9 format))
    
    (format #t \"~%🏗️  Infrastructure as Code Focus~%\")
    (format #t \"Terraform, CDK, WAF rules, SSL certificates~%~%\")
    
    (let ((prs (load-prs-from-json \"data/demo-infrastructure.json\")))
      (format #t \"✅ Loaded ~a infrastructure PRs~%~%\" (length prs))
      (simulate-deployment-pipeline prs 8))
    "
}

function run_peak_load() {
    echo -e "${RED}🔥 Running Peak Load Scenario (20 PRs)${NC}\n"
    echo -e "${YELLOW}Simulating Black Friday deployment window...${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    guile -c "
    (use-modules (simulator deployment-pipeline)
                 (simulator data-export)
                 (ice-9 format)
                 (srfi srfi-1))
    
    (format #t \"~%🔥 PEAK LOAD SCENARIO~%\")
    (format #t \"Black Friday - All hands on deck!~%~%\")
    
    (let ((all-prs (load-prs-from-json \"data/demo-change-requests.json\")))
      (let ((high-priority-prs (take (filter (lambda (pr)
                                                (eq? (assoc-ref pr 'priority) 'high))
                                              all-prs)
                                      20)))
        (format #t \"⚠️  ~a high-priority PRs in queue!~%~%\" (length high-priority-prs))
        (simulate-deployment-pipeline high-priority-prs 20)))
    "
}

function run_emergency() {
    echo -e "${RED}🆘 Running Emergency Patch Scenario${NC}\n"
    echo -e "${RED}CRITICAL SECURITY VULNERABILITY DETECTED!${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    guile -c "
    (use-modules (simulator deployment-pipeline)
                 (ice-9 format)
                 (srfi srfi-19))
    
    (format #t \"~%🆘 EMERGENCY CHANGE REQUEST~%\")
    (format #t \"CVE-2024-CRITICAL - Immediate patch required~%~%\")
    
    ;; Create emergency change request
    (let ((emergency-pr 
           '((pr-number . 9999)
             (component . backend-api)
             (title . \"EMERGENCY: Patch critical security vulnerability CVE-2024-CRITICAL\")
             (author . \"security-team\")
             (change-type . emergency)
             (priority . critical)
             (estimated-deploy-time . 180)  ; 3 minutes
             (failure-rate . 0.01)  ; Very low - well tested
             (requires-migration . #f)
             (created-at . #<time>)
             (status . pending)
             (change-id . \"CHG-EMERGENCY-9999\"))))
      
      (format #t \"Bypassing standard approval process...~%\")
      (format #t \"Notifying on-call team...~%\")
      (format #t \"Initiating emergency deployment...~%~%\")
      
      (simulate-deployment-pipeline (list emergency-pr) 1)
      
      (format #t \"~%Post-deployment tasks:~%\")
      (format #t \"  • Generate incident report~%\")
      (format #t \"  • Schedule post-mortem~%\")
      (format #t \"  • Update security advisories~%\")
      (format #t \"  • Notify customers~%\"))
    "
}

function show_metrics() {
    echo -e "\n${BLUE}📊 Deployment Metrics Dashboard${NC}"
    echo "┌──────────────────────────────────────────────────────────┐"
    echo "│ Success Rate: 84.2%              MTTR: 8.7 min       │"
    echo "│ Queue Depth: 0                   Active: 0           │"
    echo "│ Today's Deployments: ${PR_COUNT}          Failed: 2            │"
    echo "└──────────────────────────────────────────────────────────┘"
}

# Main execution
print_banner

case $SCENARIO in
    quick)
        run_quick_demo
        ;;
    frontend)
        run_frontend_demo
        ;;
    backend)
        run_backend_demo
        ;;
    infra)
        run_infra_demo
        ;;
    peak-load)
        run_peak_load
        ;;
    emergency)
        run_emergency
        ;;
    *)
        echo -e "${RED}Unknown scenario: $SCENARIO${NC}"
        echo "Available: quick, frontend, backend, infra, peak-load, emergency"
        exit 1
        ;;
esac

show_metrics

echo -e "\n${GREEN}✅ Demo completed successfully!${NC}"
echo -e "${YELLOW}Thank you for watching the ITIL Change Management demo.${NC}\n"