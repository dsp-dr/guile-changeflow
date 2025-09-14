# Guile ChangeFlow - Project Intent & AI Agent Onboarding

## Core Purpose
Build an ITIL 4 compliant change management system that exposes 8 critical tools via MCP (Model Context Protocol) for AI agents to manage enterprise change workflows.

## What This Project IS
- **MCP Server**: JSON-RPC 2.0 compliant server exposing ITIL tools
- **Cloudflare Worker**: Serverless deployment for global availability
- **AI Integration**: Built specifically for AI agents to manage changes
- **ITIL Compliant**: Follows ITIL 4 change management best practices

## What This Project IS NOT
- Not a GUI application (no web interface needed)
- Not a standalone Guile application (Guile modules exist but aren't deployed)
- Not a traditional REST API (uses MCP/JSON-RPC protocol)

## Critical Success Metrics
1. **8 ITIL Tools Available**: Must expose exactly 8 tools via MCP
2. **Response Time < 200ms**: Performance is critical
3. **100% Cloudflare Worker**: All logic runs in worker.js
4. **MCP Protocol 2024-11-05**: Must be protocol compliant

## The 8 ITIL Tools (MUST HAVE ALL)
1. `create_change_request` - Standard change creation
2. `assess_change_risk` - Risk scoring algorithm
3. `check_freeze_period` - Deployment window validation
4. `get_cab_members` - Change Advisory Board routing
5. `schedule_change` - Change scheduling system
6. `create_emergency_change` - Fast-track emergency changes
7. `get_change_metrics` - Performance metrics
8. `generate_audit_report` - Compliance reporting

## Technical Architecture

### What Actually Runs in Production
```
infra/cloudflare/worker.js  <- THIS IS THE ENTIRE SYSTEM
```

### Supporting Files (for context/future)
```
src/                <- Guile modules (not deployed, future work)
├── models/         <- Change request models
├── mcp/            <- MCP protocol implementation
├── risk/           <- Risk calculation engine
└── web/            <- Web server (not used)
```

## Deployment Architecture
```
api.changeflow.us
    ↓
Cloudflare DNS
    ↓
guile-changeflow-prod (Worker)
    ↓
worker.js (All logic here)
```

## Key Design Decisions

### Why Cloudflare Worker?
- Global edge deployment
- No infrastructure to manage
- Sub-50ms response times
- Free tier sufficient

### Why MCP Protocol?
- Native AI agent integration
- Anthropic's standard for tool use
- JSON-RPC for structured communication
- Tool discovery built-in

### Why Mock Implementation?
- Rapid prototype for demo
- Proves MCP integration works
- Real Guile implementation can follow
- Worker limitations prevent Guile runtime

## Common Pitfalls to Avoid

### DON'T
- Try to run Guile in Cloudflare Worker (not supported)
- Add a web UI (this is for AI agents only)
- Change the 8 tool names (they're ITIL standard)
- Remove mock data (needed for demos)

### DO
- Keep all logic in worker.js
- Maintain MCP protocol compliance
- Test with `curl` and MCP clients
- Focus on the 8 ITIL tools

## Testing & Validation

### Quick Health Check
```bash
curl https://api.changeflow.us/
# Should return: {"status":"healthy","message":"MCP Server Ready","version":"2024-11-05"}
```

### Verify 8 Tools
```bash
curl -X POST https://api.changeflow.us/ \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' | jq '.result.tools | length'
# Must return: 8
```

### Run Full Test Suite
```bash
./scripts/test-endpoints.sh
./scripts/verify-deployment.sh
```

## Future Evolution Path

### Phase 1 (Current) ✅
- Mock implementation in worker.js
- 8 ITIL tools working
- MCP protocol compliant
- Deployed to production

### Phase 2 (Next)
- Add persistent storage (Cloudflare KV/D1)
- Real change tracking
- Webhook integrations
- Authentication layer

### Phase 3 (Future)
- Guile modules as microservices
- Multi-region deployment
- Enterprise SSO
- Audit compliance reporting

## For AI Agents Working on This

### Your Primary Objective
Maintain a working MCP server with 8 ITIL tools at api.changeflow.us

### Key Files to Understand
1. `infra/cloudflare/worker.js` - The entire production system
2. `scripts/test-endpoints.sh` - Validation suite
3. `scripts/verify-deployment.sh` - Deployment checker

### Development Workflow
1. Edit `infra/cloudflare/worker.js`
2. Test locally: `node scripts/mcp-local-server.js`
3. Deploy: `cd infra/cloudflare && wrangler deploy`
4. Verify: `./scripts/verify-deployment.sh`

### Success Criteria
- `api.changeflow.us` returns 8 tools
- All tests in `test-endpoints.sh` pass
- Response time < 200ms
- No JavaScript errors in worker

## Environment Variables Needed
```
CLOUDFLARE_API_TOKEN=<token>
CLOUDFLARE_ACCOUNT_ID=<account>
```

## Critical URLs
- Production: https://api.changeflow.us
- Staging: https://guile-changeflow-staging.jasonwalsh.workers.dev
- Worker: https://guile-changeflow-prod.jasonwalsh.workers.dev

## Remember
This is an MCP server for AI agents, not a traditional web app. The Guile name is aspirational - the current implementation is 100% JavaScript in a Cloudflare Worker. The 8 ITIL tools are the core value proposition.