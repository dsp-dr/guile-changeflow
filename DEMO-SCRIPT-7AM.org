# Guile ChangeFlow - 7 AM Demo Script

## Pre-Demo Setup (6:45 AM)

### 1. Deploy Worker to Production
```bash
# Deploy the Worker
./deploy.sh

# Verify deployment
curl https://api.changeflow.us/health | jq .
```

### 2. Open Dashboard
- Open `dashboard.html` in browser
- Keep it visible during demo
- Shows real-time updates every 2 seconds

### 3. Terminal Setup
- Terminal 1: Show deployment logs (`wrangler tail`)
- Terminal 2: Ready for test commands
- Terminal 3: Show this demo script

---

## Demo Flow (7:00 AM - 7:05 AM)

### Introduction (30 seconds)
"Good morning! I'm demonstrating Guile ChangeFlow, our AI-powered ITIL 4 change management system that integrates with Claude AI through the Model Context Protocol."

**Key Points:**
- Automatic risk assessment
- Real-time monitoring with Workers Logs
- Production-ready on Cloudflare Workers
- api.changeflow.us is live

---

### Part 1: MCP Integration (1 minute)

#### Show MCP Discovery
```bash
curl -s https://api.changeflow.us/mcp | jq .
```

**Explain:**
- "MCP allows Claude AI to discover and use our tools"
- "We expose 4 tools: create, get, list, and assess risk"

#### Show Available Tools
```bash
curl -s https://api.changeflow.us/tools | jq '.[].name'
```

---

### Part 2: Risk Assessment Demo (1 minute)

#### Low Risk Change
```bash
curl -X POST https://api.changeflow.us/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "create_change_request",
    "params": {
      "title": "Update API documentation",
      "description": "Add new endpoint documentation",
      "systems": ["docs"],
      "urgency": "low"
    }
  }' | jq .
```

**Show:** Dashboard updates with GREEN change (risk < 30)

#### High Risk Change
```bash
curl -X POST https://api.changeflow.us/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -d '{
    "tool": "create_change_request",
    "params": {
      "title": "Update production payment gateway",
      "description": "Critical security patch for payment processing",
      "systems": ["payment", "api", "database"],
      "urgency": "emergency"
    }
  }' | jq .
```

**Show:** Dashboard updates with RED change (risk > 70)

---

### Part 3: Workers Logs (30 seconds)

#### Show Live Logs
```bash
# In Terminal 1, logs should be streaming
wrangler tail
```

**Explain:**
- "10% sampling rate for normal requests"
- "100% logging for tool invocations"
- "Structured JSON logs for analysis"

#### Demonstrate Log Output
```bash
# Create a change to trigger logs
./test-mcp.sh https://api.changeflow.us
```

---

### Part 4: GitHub Integration (1 minute)

#### Simulate GitHub PR Webhook
```bash
curl -X POST https://api.changeflow.us/webhooks/github \
  -H "Content-Type: application/json" \
  -d '{
    "action": "opened",
    "pull_request": {
      "number": 42,
      "title": "Fix critical security vulnerability",
      "body": "Patches authentication bypass in production",
      "html_url": "https://github.com/org/repo/pull/42",
      "user": {"login": "developer"},
      "base": {"repo": {"name": "main-app"}},
      "labels": [{"name": "urgent"}]
    }
  }' | jq .
```

**Show:** Dashboard updates with GitHub-sourced change

---

### Part 5: Claude AI Integration (1 minute)

**Say:** "Now Claude can create changes directly"

#### Show Claude Creating a Change
```bash
# Simulate what Claude would send
curl -X POST https://api.changeflow.us/mcp/tools/invoke \
  -H "Content-Type: application/json" \
  -H "User-Agent: Claude-AI/1.0" \
  -d '{
    "tool": "create_change_request",
    "params": {
      "title": "Deploy new ML model to production",
      "description": "Claude recommends deploying the new fraud detection model",
      "systems": ["ml-pipeline", "api", "production"],
      "urgency": "high"
    }
  }' | jq .
```

**Show:**
- Risk automatically calculated
- Change appears in dashboard
- Logs show Claude interaction

---

### Part 6: Production Metrics (30 seconds)

#### Show System Health
```bash
# Check response times
time curl -s https://api.changeflow.us/health > /dev/null
```

**Metrics to Highlight:**
- < 50ms response time globally (Cloudflare edge)
- 100% uptime
- Automatic scaling
- DDoS protection included

---

## Closing (30 seconds)

### Summary Points:
1. ✅ **MCP Protocol:** Full implementation, Claude-ready
2. ✅ **Risk Engine:** Automatic assessment based on content
3. ✅ **Workers Logs:** 10% sampling + full tool tracking
4. ✅ **Production Ready:** Live at api.changeflow.us
5. ✅ **Integrations:** GitHub webhooks working

### Key Benefits:
- **No Infrastructure:** Serverless on Cloudflare
- **Global Performance:** Edge deployment
- **AI-Native:** Designed for Claude integration
- **ITIL Compliant:** Follows change management best practices

### Next Steps:
- Add persistence with Cloudflare KV
- Implement approval workflows
- Add more integrations (Jira, Slack)
- Enhanced risk algorithms

---

## Troubleshooting

### If Worker isn't responding:
```bash
# Quick redeploy
wrangler deploy

# Check status
curl https://api.changeflow.us/health
```

### If logs aren't showing:
```bash
# Restart tail
wrangler tail --format json
```

### If dashboard isn't updating:
- Check browser console for errors
- Verify API URL in dashboard.html
- Test API directly with curl

---

## Demo Success Criteria

✅ MCP endpoints responding
✅ Risk calculation working
✅ Dashboard showing changes
✅ Logs capturing requests
✅ < 100ms response times
✅ No errors during demo

---

## Quick Test Commands

```bash
# Health check
curl https://api.changeflow.us/health

# Create low risk
curl -X POST https://api.changeflow.us/api/changes \
  -H "Content-Type: application/json" \
  -d '{"title":"Test","description":"Test change","urgency":"low"}'

# Create high risk
curl -X POST https://api.changeflow.us/api/changes \
  -H "Content-Type: application/json" \
  -d '{"title":"Production payment update","description":"Critical","urgency":"emergency"}'

# List all changes
curl https://api.changeflow.us/api/changes
```

---

## Post-Demo

1. Save logs for analysis
2. Document any issues
3. Gather feedback
4. Plan improvements

Good luck with the demo! 🚀