# 🎬 ITIL Deployment Pipeline Demo

## Overview

This demo showcases a realistic deployment pipeline simulator that processes 100 mock PRs across different component types, implementing ITIL change management with blocking staging deployments.

## What It Simulates

### 📦 Component Distribution (100 PRs)
- **Frontend** (30 PRs): UI changes, responsive fixes, feature toggles
- **Backend API** (25 PRs): Endpoints, business logic, optimizations  
- **Backend DB** (15 PRs): Migrations, schema changes, indexing
- **IaC App** (20 PRs): ECS tasks, auto-scaling, monitoring
- **IaC Global** (10 PRs): WAF rules, regions, SSL certificates

### ⚡ Realistic Characteristics
- **Deploy Times**: 5min (frontend) to 90min (global IaC)
- **Failure Rates**: 8% (frontend) to 25% (global IaC)
- **Change Types**: Standard vs Normal (with migrations)
- **Priority**: High (DB/Global) vs Medium/Low

### 🔄 Deployment Pipeline
- **Blocking Staging**: Only 1 deployment active at a time
- **2-Hour Windows**: 1 hour deploy + 1 hour rollback buffer
- **Queue Management**: Priority-based processing
- **ITIL Integration**: Change requests generated for each PR

## Quick Start

### 1. Basic Test
```bash
# Test the simulator loads correctly
guile -L src -c "(use-modules (simulator deployment-pipeline)) (format #t 'Ready!~%')"
```

### 2. Generate 100 Mock PRs
```bash
guile -L src -c "(use-modules (simulator deployment-pipeline)) (generate-mock-prs)"
```

### 3. Run 20-Deployment Demo
```bash
guile -L src demo-deployment-pipeline.scm
```

## Full Demo Setup

### Prerequisites
```bash
# Install asciicinema (optional for recording)
pip install asciicinema
# or: brew install asciinema
# or: apt install asciinema
```

### 1. Create Tmux Session
```bash
./demo-session.sh
```

This creates a tmux session with multiple windows:
- **Window 0**: Main demo interface
- **Window 1**: Simulator console 
- **Window 2**: System monitor
- **Window 3**: Deployment logs
- **Window 4**: ITIL change requests

### 2. Start Recording (Optional)
```bash
# In the tmux session
asciicinema rec deployment-demo.cast
```

### 3. Run the Demo
```bash
# In tmux window 1 (simulator)
guile -L src demo-deployment-pipeline.scm
```

## Demo Output

The demo will show:

```
🎬 ITIL Deployment Pipeline Simulator - Live Demo
══════════════════════════════════════════════════════════════════════════
│  🟢 Simulating 100 PRs across Frontend, Backend, IaC                  │
│  🔄 Blocking staging deployments with 2-hour windows               │
│  📋 ITIL change requests for each deployment                       │
└──────────────────────────────────────────────────────────────────────────┘

🔧 Generating 100 mock PRs across components...

📦 frontend: 30 PRs
📦 backend-api: 25 PRs  
📦 backend-db: 15 PRs
📦 iac-app: 20 PRs
📦 iac-global: 10 PRs

📊 Total PRs generated: 100

🚀 Starting Deployment Pipeline Simulation
Processing 20 PRs from 100 total

📋 Deployment Queue initialized with 20 PRs

🔧 [2025-09-14T10:30:00] Starting deployment: Add dark mode toggle to settings (#1001) (frontend)
   📋 Change Request: CHG-frontend-1001
   ⏰ Deploy window: 12 minutes (ends 2025-09-14T11:42:00)

✅ [2025-09-14T10:42:00] Deployment succeeded: Add dark mode toggle to settings (#1001) (frontend)
   🆓 Staging environment released

🔧 [2025-09-14T10:42:00] Starting deployment: Migrate legacy user schema (#1045) (backend-db)
   📋 Change Request: CHG-backend-db-1045
   ⏰ Deploy window: 34 minutes (ends 2025-09-14T12:16:00)

❌ [2025-09-14T12:16:00] Deployment failed: Migrate legacy user schema (#1045) (backend-db)
   🔄 Initiating rollback procedure...
   🆓 Staging environment released

⏱️  Progress: 5 deployments started, 3 completed
...

✅ Pipeline simulation complete!
📊 Final stats: 20 deployments processed

📊 DEPLOYMENT PIPELINE SUMMARY
================================
Total deployments: 20
Successful: 16 (80.0%)
Failed: 4 (20.0%)

📦 Component Breakdown:
  frontend: 6 deployments (5 successful)
  backend-api: 5 deployments (4 successful)  
  backend-db: 3 deployments (2 successful)
  iac-app: 4 deployments (3 successful)
  iac-global: 2 deployments (2 successful)
```

## Architecture Details

### Mock PR Structure
Each PR includes:
```scheme
((pr-number . 1001)
 (component . frontend)
 (title . "Add dark mode toggle to settings (#1001)")
 (author . "alice-dev")
 (change-type . standard)  ; or 'normal for migrations
 (priority . medium)       ; high/medium/low
 (estimated-deploy-time . 720)  ; seconds
 (failure-rate . 0.08)     ; 8%
 (requires-migration . #f)
 (created-at . #<time>)
 (status . pending)
 (change-id . "CHG-frontend-1001"))
```

### ITIL Change Request Generation
Each PR automatically generates:
- **Change ID**: `CHG-{component}-{pr-number}`
- **Type**: Standard (quick) vs Normal (requires CAB)
- **Risk Level**: Based on component type
- **Priority**: High for DB/Global, Medium/Low for others
- **Environment**: Staging → Production path

### Deployment Queue Logic
1. PRs sorted by priority (High → Medium → Low)
2. Only 1 staging deployment active at a time
3. 2-hour windows: deploy time + 1-hour rollback buffer
4. Failed deployments trigger rollback procedures
5. Staging environment released after completion

## Real-World Integration Points

In production, this simulator would connect to:
- **GitHub API**: Real PR data and webhooks
- **CI/CD Systems**: GitHub Actions, Jenkins, CircleCI
- **Infrastructure**: Terraform, AWS CDK, Pulumi
- **Monitoring**: Datadog, New Relic, CloudWatch
- **Communication**: Slack, PagerDuty, email
- **Approval Systems**: ServiceNow, Jira Service Management

## Key Insights Demonstrated

1. **Blocking Deployments**: Staging bottleneck realistic
2. **Component Differences**: DB migrations take longer, fail more
3. **Queue Management**: Priority-based processing essential
4. **ITIL Integration**: Every deployment needs change management
5. **Failure Rates**: Global infrastructure changes are riskiest
6. **Time Windows**: 2-hour deployment slots prevent overlap

## Files Created

- `src/simulator/deployment-pipeline.scm` - Main simulator
- `demo-deployment-pipeline.scm` - Interactive demo script
- `demo-session.sh` - Tmux session setup
- `DEMO-README.md` - This documentation

## Extending the Demo

### Add New Component Types
```scheme
(define component-types
  '((mobile-app . ((count . 15)
                   (deploy-time . (900 2700))
                   (failure-rate . 0.18)
                   (requires-migration . #f)
                   (rollback-time . 600)))
    ...)
```

### Custom Failure Scenarios
```scheme
(define (inject-custom-failure pr)
  "Add custom failure injection logic"
  ...)
```

### External System Integration
```scheme
(define (notify-slack deployment-result)
  "Send deployment notifications to Slack"
  ...)
```

---

🎯 This demo showcases the power of combining:
- **Guile Scheme** for elegant functional programming
- **ITIL 4** for structured change management  
- **Chaos Engineering** for realistic failure simulation
- **DevOps Pipelines** for modern deployment practices

Perfect for demonstrating how traditional ITIL processes can be modernized with functional programming and simulation techniques!