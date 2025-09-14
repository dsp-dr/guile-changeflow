# MCP Development Guide for Guile ChangeFlow

*Complete guide for developing, testing, integrating, deploying, using, and debugging the MCP protocol implementation*

## Table of Contents

1. [Overview](#overview)
2. [Development Setup](#development-setup)
3. [Architecture](#architecture)
4. [Testing Guide](#testing-guide)
5. [Integration Guide](#integration-guide)
6. [Deployment Guide](#deployment-guide)
7. [Usage Guide](#usage-guide)
8. [Debugging Guide](#debugging-guide)
9. [API Reference](#api-reference)
10. [Troubleshooting](#troubleshooting)

## Overview

The Guile ChangeFlow MCP (Model Context Protocol) implementation provides a complete ITIL 4 change management system accessible to AI agents via standardized protocol endpoints.

### Key Features

- **8 ITIL Tools**: Complete change management workflow
- **5 MCP Resources**: Configuration and documentation access
- **3 MCP Prompts**: AI-assisted change operations
- **JSON-RPC 2.0**: Standards-compliant protocol implementation
- **Dual Runtime**: CloudFlare Worker + Local Node.js server

### Protocol Compliance

- MCP Protocol Version: `2024-11-05`
- JSON-RPC 2.0 compliant
- Full tools, resources, prompts support
- Error handling per MCP specification

## Development Setup

### Prerequisites

```bash
# System requirements
node --version    # v18+ required
guile --version   # v3.0+ required (for Guile implementation)
jq --version      # For JSON processing in tests

# Project dependencies
cd /path/to/guile-changeflow
npm install       # If package.json exists
```

### Local Development Environment

1. **Clone and Setup**
```bash
git clone https://github.com/dsp-dr/guile-changeflow
cd guile-changeflow
git checkout feat/cloudflare-mcp-implementation
```

2. **Start Local MCP Server**
```bash
# Method 1: Direct execution
node scripts/mcp-local-server.js

# Method 2: Background with logs
node scripts/mcp-local-server.js > mcp-server.log 2>&1 &

# Method 3: Development mode (with restart on changes)
# Install nodemon: npm install -g nodemon
nodemon scripts/mcp-local-server.js
```

3. **Verify Server**
```bash
# Health check
curl http://localhost:8080/

# MCP protocol test
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'
```

### Development Workflow

```bash
# 1. Make changes to worker implementation
vim infra/cloudflare/worker-mcp-poc.js

# 2. Update production worker
cp infra/cloudflare/worker-mcp-poc.js infra/cloudflare/worker.js

# 3. Test locally
node test/mcp-comprehensive-test.js

# 4. Test with live server
node scripts/mcp-local-server.js &
curl -X POST http://localhost:8080/ -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

# 5. Deploy to CloudFlare (if needed)
cd infra/cloudflare && wrangler deploy
```

## Architecture

### System Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   AI Clients    │    │   MCP Server    │    │  ITIL Backend   │
│                 │────│                 │────│                 │
│ Claude Code     │    │ Node.js/CF      │    │ Risk Engine     │
│ MCP Debugger    │    │ JSON-RPC 2.0    │    │ Change Models   │
│ Custom Tools    │    │ Tools/Resources │    │ Approval Logic  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Component Structure

```
src/
├── mcp/                      # Guile MCP implementation
│   ├── server.scm           # HTTP server and routing
│   ├── handlers.scm         # Request handlers
│   ├── discovery.scm        # Service discovery
│   └── tools.scm           # Tool implementations
├── models/                   # Data models
├── risk/                    # Risk assessment engine
└── web/                     # Web API components

infra/cloudflare/
├── worker.js               # Production CloudFlare Worker
├── worker-mcp-poc.js       # Development version
├── wrangler.toml          # CloudFlare configuration
└── package.json           # Dependencies

scripts/
├── mcp-local-server.js     # Local development server
└── test-endpoints.sh       # Integration tests

test/
└── mcp-comprehensive-test.js # Full test suite
```

### MCP Protocol Implementation

#### Capabilities

```json
{
  "tools": {},        // 8 ITIL change management tools
  "resources": {},    // 5 configuration and documentation resources
  "prompts": {},      // 3 AI-assisted operation prompts
  "notifications": {} // Future: Real-time change notifications
}
```

#### Tool Categories

1. **Change Creation**
   - `create_change_request` - Standard change requests
   - `create_emergency_change` - Expedited emergency changes

2. **Risk Management**
   - `assess_change_risk` - Multi-factor risk assessment
   - `check_freeze_period` - Deployment window validation

3. **Workflow Management**
   - `get_cab_members` - Change Advisory Board routing
   - `schedule_change` - Change scheduling with notifications

4. **Reporting & Compliance**
   - `get_change_metrics` - Performance and success metrics
   - `generate_audit_report` - ITIL compliance reporting

## Testing Guide

### Test Suite Overview

The comprehensive test suite covers all MCP protocol features:

```bash
# Run full test suite
node test/mcp-comprehensive-test.js

# Expected output:
🧪 Starting Comprehensive MCP Test Suite
==========================================
✅ Worker loaded successfully
✅ Health Check (2ms)
✅ MCP Initialize (0ms)
✅ Tools List (1ms)
✅ Tool Execution (0ms)
✅ Resources List (0ms)
✅ Resource Read (0ms)
✅ Prompts List (0ms)
✅ Prompt Get (0ms)
✅ Error Handling (0ms)
✅ Performance (1ms)

📊 Test Results Summary
=======================
✅ Passed: 10
❌ Failed: 0
⏭️  Skipped: 0
📈 Total: 10
🎉 Test suite PASSED
```

### Manual Testing

#### 1. Protocol-Level Tests

```bash
# Initialize handshake
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {}
  },
  "id": 1
}'

# List capabilities
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "params": {},
  "id": 2
}'
```

#### 2. Tool Testing

```bash
# Risk Assessment
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "assess_change_risk",
    "arguments": {
      "change_type": "database_schema",
      "environment": "production",
      "components_affected": 8,
      "has_rollback": true,
      "tested_in_staging": false
    }
  },
  "id": 3
}'

# Emergency Change
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "create_emergency_change",
    "arguments": {
      "title": "Security Patch CVE-2025-DEMO",
      "description": "Critical authentication bypass fix",
      "justification": "Active exploitation detected",
      "impact": "2-minute service restart",
      "environment": "production",
      "requester": "security@company.com"
    }
  },
  "id": 4
}'
```

#### 3. Resource Testing

```bash
# List resources
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "resources/list",
  "params": {},
  "id": 5
}'

# Read ITIL compliance guide
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "resources/read",
  "params": {
    "uri": "changeflow://docs/itil-compliance"
  },
  "id": 6
}'
```

#### 4. Prompt Testing

```bash
# Generate risk analysis prompt
curl -X POST http://localhost:8080/ -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "method": "prompts/get",
  "params": {
    "name": "analyze-change-risk",
    "arguments": {
      "change_description": "Deploy new microservice to production",
      "environment": "production"
    }
  },
  "id": 7
}'
```

### Performance Testing

```bash
# Load test with Apache Bench
ab -n 1000 -c 10 -H "Content-Type: application/json" \
   -p <(echo '{"jsonrpc":"2.0","method":"tools/list","id":1}') \
   http://localhost:8080/

# Expected results:
# - Requests per second: >500/s
# - 99th percentile latency: <10ms
# - No failed requests
```

### Error Testing

```bash
# Invalid method
curl -X POST http://localhost:8080/ -d '{"jsonrpc":"2.0","method":"invalid","id":1}'
# Expected: {"error":{"code":-32601,"message":"Method not found: invalid"}}

# Invalid tool
curl -X POST http://localhost:8080/ -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"invalid_tool"},"id":2}'
# Expected: {"result":{"error":"Tool not implemented: invalid_tool"}}

# Invalid resource
curl -X POST http://localhost:8080/ -d '{"jsonrpc":"2.0","method":"resources/read","params":{"uri":"invalid://resource"},"id":3}'
# Expected: {"result":{"error":"Resource not found: invalid://resource"}}
```

## Integration Guide

### Claude Code Integration

#### 1. MCP Configuration

Create `.claude_code_mcp_config.json` in your project root:

```json
{
  "mcpServers": {
    "guile-changeflow": {
      "command": "node",
      "args": ["scripts/mcp-local-server.js"],
      "cwd": "/path/to/guile-changeflow",
      "env": {
        "MCP_PORT": "8080"
      }
    }
  }
}
```

#### 2. Usage in Claude Code

Once configured, Claude Code will automatically discover and use the tools:

```
User: "Please assess the risk of deploying our authentication microservice to production"

Claude: I'll assess the risk of your authentication microservice deployment using the ITIL change management tools.

[Uses assess_change_risk tool automatically]

Based on the risk assessment:
- Risk Score: 85/100 (Critical)
- Risk Level: Critical
- Key Risk Factors:
  - Production environment (+40 risk)
  - Authentication service criticality (+25 risk)
  - Weekend deployment timing (+10 risk)
  - Multiple dependencies (+10 risk)

Recommendations:
- Schedule during maintenance window
- Ensure full rollback plan tested
- Require C-level approval (critical risk)
- Have incident response team on standby
```

### MCP Inspector Integration

```bash
# Install MCP Inspector
npm install -g @anthropic/mcp-inspector

# Inspect our server
mcp-inspector http://localhost:8080

# Or use the development server directly
mcp-inspector --command "node" --args "scripts/mcp-local-server.js"
```

### Custom Client Integration

```javascript
// Example Node.js MCP client
const axios = require('axios');

class ChangeFlowMCPClient {
  constructor(baseURL = 'http://localhost:8080') {
    this.baseURL = baseURL;
    this.id = 0;
  }

  async call(method, params = {}) {
    const response = await axios.post(this.baseURL, {
      jsonrpc: '2.0',
      method,
      params,
      id: ++this.id
    });
    return response.data.result;
  }

  async assessRisk(changeType, environment, options = {}) {
    return await this.call('tools/call', {
      name: 'assess_change_risk',
      arguments: {
        change_type: changeType,
        environment: environment,
        ...options
      }
    });
  }

  async createEmergencyChange(title, description, justification, impact, environment, requester) {
    return await this.call('tools/call', {
      name: 'create_emergency_change',
      arguments: { title, description, justification, impact, environment, requester }
    });
  }
}

// Usage
const client = new ChangeFlowMCPClient();
const risk = await client.assessRisk('microservice', 'production', {
  components_affected: 5,
  has_rollback: true,
  tested_in_staging: false
});
```

## Deployment Guide

### Local Development Deployment

```bash
# Start development server
node scripts/mcp-local-server.js

# Server will be available at http://localhost:8080
# Logs show request handling in real-time
```

### CloudFlare Worker Deployment

#### 1. Prerequisites

```bash
# Install Wrangler CLI
npm install -g wrangler

# Authenticate with CloudFlare
wrangler auth
```

#### 2. Configure Deployment

```bash
cd infra/cloudflare

# Review wrangler.toml configuration
cat wrangler.toml

# Update worker.js with latest implementation
cp worker-mcp-poc.js worker.js
```

#### 3. Deploy

```bash
# Development deployment
wrangler deploy --env dev

# Production deployment
wrangler deploy --env production

# View deployment
wrangler tail  # Live logs
```

#### 4. Verify Deployment

```bash
# Test deployed endpoint
curl https://your-worker.your-subdomain.workers.dev/

# Run full test suite against deployed version
CLOUDFLARE_WORKER_URL=https://your-worker.your-subdomain.workers.dev \
  node test/mcp-comprehensive-test.js
```

### Production Configuration

#### Environment Variables

```bash
# Local development
export MCP_PORT=8080
export MCP_LOG_LEVEL=info

# CloudFlare Worker (via wrangler.toml)
[env.production.vars]
MCP_LOG_LEVEL = "warn"
RATE_LIMIT_REQUESTS = "1000"
RATE_LIMIT_WINDOW = "3600"
```

#### Security Configuration

```javascript
// Add to worker.js for production
const SECURITY_CONFIG = {
  rateLimiting: {
    requestsPerHour: 1000,
    burstLimit: 100
  },
  authentication: {
    required: process.env.NODE_ENV === 'production',
    apiKeyHeader: 'X-API-Key'
  },
  cors: {
    allowOrigin: ['https://claude.ai', 'https://your-domain.com'],
    allowMethods: ['GET', 'POST', 'OPTIONS']
  }
};
```

### Monitoring and Observability

#### CloudFlare Analytics

```bash
# View worker analytics
wrangler tail --format pretty

# Worker metrics
wrangler pages deployment list
```

#### Custom Logging

```javascript
// Enhanced logging in worker
console.log(JSON.stringify({
  timestamp: new Date().toISOString(),
  method: request.method,
  path: request.url,
  userAgent: request.headers.get('user-agent'),
  duration: Date.now() - startTime,
  status: response.status
}));
```

## Usage Guide

### Available Tools

#### 1. create_change_request

Create standard ITIL change requests.

```json
{
  "name": "create_change_request",
  "arguments": {
    "title": "Deploy user authentication service v2.1",
    "description": "Update authentication microservice with OAuth 2.1 support",
    "risk_level": "medium",
    "environment": "production"
  }
}
```

**Response:**
```json
{
  "change_id": "CHG-1757819511107",
  "status": "pending",
  "risk_score": 45,
  "created_at": "2025-09-14T03:15:00.000Z"
}
```

#### 2. assess_change_risk

Evaluate risk factors for proposed changes.

```json
{
  "name": "assess_change_risk",
  "arguments": {
    "change_type": "database_migration",
    "environment": "production",
    "components_affected": 12,
    "has_rollback": true,
    "tested_in_staging": false
  }
}
```

**Response:**
```json
{
  "risk_score": 75,
  "risk_level": "high",
  "factors": [
    "Production environment (+40 risk)",
    "Not tested in staging (+20 risk)",
    "High component count (+15 risk)"
  ]
}
```

#### 3. create_emergency_change

Create expedited emergency changes.

```json
{
  "name": "create_emergency_change",
  "arguments": {
    "title": "Critical Log4j Vulnerability Patch",
    "description": "Apply security patch for CVE-2021-44228",
    "justification": "Critical vulnerability with active exploitation",
    "impact": "Brief service restart required",
    "environment": "production",
    "requester": "security-team@company.com"
  }
}
```

### Available Resources

#### 1. Configuration Resources

```json
// changeflow://config/change-types
{
  "standard": {"risk_base": 10, "approval_required": false},
  "normal": {"risk_base": 30, "approval_required": true},
  "emergency": {"risk_base": 80, "approval_required": true, "expedited": true}
}

// changeflow://config/approval-matrix
{
  "low": ["tech-lead@company.com"],
  "medium": ["tech-lead@company.com", "ops-manager@company.com"],
  "high": ["tech-lead@company.com", "ops-manager@company.com", "cto@company.com"],
  "critical": ["tech-lead@company.com", "ops-manager@company.com", "cto@company.com", "ceo@company.com"]
}
```

#### 2. Documentation Resources

```markdown
// changeflow://docs/itil-compliance
# ITIL 4 Compliance Guide

## Risk Assessment Requirements
- All production changes require risk assessment
- Risk factors: environment, complexity, dependencies, timing
- Scores: 0-39 (low), 40-59 (medium), 60-79 (high), 80-100 (critical)

## Approval Workflows
- Low risk: Tech lead approval
- Medium risk: Tech lead + Ops manager
- High risk: Tech lead + Ops manager + CTO
- Critical risk: Full CAB approval including CEO
```

### Available Prompts

#### 1. analyze-change-risk

Generate comprehensive risk analysis prompts:

```json
{
  "name": "analyze-change-risk",
  "arguments": {
    "change_description": "Deploy machine learning model update to recommendation engine",
    "environment": "production"
  }
}
```

#### 2. generate-rollback-plan

Create detailed rollback procedures:

```json
{
  "name": "generate-rollback-plan",
  "arguments": {
    "change_details": "Kubernetes deployment of new payment service version",
    "system_context": "High-availability payment processing cluster"
  }
}
```

#### 3. create-change-summary

Generate executive summaries for approval workflows:

```json
{
  "name": "create-change-summary",
  "arguments": {
    "change_request": "Full change request details...",
    "audience": "executive"
  }
}
```

## Debugging Guide

### Common Issues and Solutions

#### 1. Server Won't Start

**Problem:** `Error: Failed to load worker`

```bash
# Check Node.js version
node --version  # Should be v18+

# Check file permissions
ls -la scripts/mcp-local-server.js
chmod +x scripts/mcp-local-server.js

# Check syntax
node --check scripts/mcp-local-server.js
node --check infra/cloudflare/worker.js
```

#### 2. MCP Protocol Errors

**Problem:** `Method not found` errors

```bash
# Verify JSON-RPC format
curl -X POST http://localhost:8080/ -d '{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "params": {},
  "id": 1
}' | jq .

# Check available methods
grep -n "case.*:" infra/cloudflare/worker.js
```

#### 3. Tool Execution Failures

**Problem:** Tools return errors or unexpected results

```bash
# Test individual tools
curl -X POST http://localhost:8080/ -d '{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "assess_change_risk",
    "arguments": {
      "change_type": "test",
      "environment": "test"
    }
  },
  "id": 1
}' | jq .result

# Validate tool schemas
node -e "
const fs = require('fs');
const worker = fs.readFileSync('infra/cloudflare/worker.js', 'utf8');
const toolsMatch = worker.match(/const TOOLS = \[(.*?)\];/s);
console.log('Found tools:', toolsMatch ? 'yes' : 'no');
"
```

### Debugging Tools

#### 1. Server Logs

```bash
# Real-time logging
node scripts/mcp-local-server.js | tee mcp-debug.log

# Enhanced debug logging
MCP_DEBUG=true node scripts/mcp-local-server.js
```

#### 2. Protocol Inspector

```bash
# Install and use MCP Inspector
npm install -g @anthropic/mcp-inspector
mcp-inspector http://localhost:8080

# Or inspect the command directly
mcp-inspector --command node --args scripts/mcp-local-server.js
```

#### 3. Request Tracing

```javascript
// Add to worker for detailed tracing
function traceRequest(request, body) {
  console.log('=== REQUEST TRACE ===');
  console.log('Method:', request.method);
  console.log('URL:', request.url);
  console.log('Headers:', JSON.stringify(request.headers, null, 2));
  console.log('Body:', JSON.stringify(body, null, 2));
  console.log('===================');
}
```

### Performance Debugging

#### 1. Response Time Analysis

```bash
# Measure response times
curl -w "@curl-format.txt" -X POST http://localhost:8080/ -d '{"jsonrpc":"2.0","method":"tools/list","id":1}' -o /dev/null -s

# curl-format.txt content:
time_namelookup:  %{time_namelookup}\n
time_connect:     %{time_connect}\n
time_appconnect:  %{time_appconnect}\n
time_pretransfer: %{time_pretransfer}\n
time_redirect:    %{time_redirect}\n
time_starttransfer: %{time_starttransfer}\n
----------\n
time_total:       %{time_total}\n
```

#### 2. Memory Usage

```bash
# Monitor Node.js memory
node --inspect scripts/mcp-local-server.js

# Or use basic monitoring
node -e "
setInterval(() => {
  const mem = process.memoryUsage();
  console.log('Memory:', {
    rss: Math.round(mem.rss / 1024 / 1024) + 'MB',
    heapUsed: Math.round(mem.heapUsed / 1024 / 1024) + 'MB'
  });
}, 5000);
require('./scripts/mcp-local-server.js');
"
```

### CloudFlare Debugging

#### 1. Worker Logs

```bash
# Real-time logs
wrangler tail --format pretty

# Filter by status
wrangler tail --status error

# Debug specific requests
wrangler tail --grep "tools/call"
```

#### 2. Local Development

```bash
# Run worker locally with Wrangler
cd infra/cloudflare
wrangler dev --local

# Test against local Wrangler
curl http://localhost:8787/ -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'
```

## API Reference

### MCP Protocol Methods

| Method | Description | Parameters | Response |
|--------|-------------|------------|----------|
| `initialize` | Protocol handshake | `protocolVersion`, `capabilities` | Server info and capabilities |
| `tools/list` | List available tools | None | Array of tool definitions |
| `tools/call` | Execute a tool | `name`, `arguments` | Tool execution result |
| `resources/list` | List available resources | None | Array of resource definitions |
| `resources/read` | Read resource content | `uri` | Resource contents |
| `prompts/list` | List available prompts | None | Array of prompt definitions |
| `prompts/get` | Get prompt template | `name`, `arguments` | Formatted prompt |

### Tool Reference

#### Risk Assessment Tools

```typescript
// assess_change_risk
interface RiskAssessmentArgs {
  change_type: string;
  environment: "dev" | "test" | "staging" | "production";
  components_affected?: number;
  has_rollback?: boolean;
  tested_in_staging?: boolean;
}

interface RiskAssessmentResult {
  risk_score: number;        // 0-100
  risk_level: "low" | "medium" | "high" | "critical";
  factors: string[];         // Risk factor descriptions
}
```

#### Change Management Tools

```typescript
// create_change_request
interface ChangeRequestArgs {
  title: string;
  description: string;
  risk_level: "low" | "medium" | "high" | "critical";
  environment: "dev" | "test" | "staging" | "production";
}

interface ChangeRequestResult {
  change_id: string;         // Format: CHG-{timestamp}
  status: "pending" | "approved" | "rejected";
  risk_score: number;
  created_at: string;        // ISO timestamp
}

// create_emergency_change
interface EmergencyChangeArgs {
  title: string;
  description: string;
  justification: string;
  impact: string;
  environment: "dev" | "test" | "staging" | "production";
  requester: string;         // Email address
}

interface EmergencyChangeResult {
  change_id: string;         // Format: EMG-{timestamp}
  type: "emergency";
  status: "pending_emergency_approval";
  risk_level: "high";        // Emergency changes are always high risk
  approval_required: boolean;
  emergency_contacts: string[];
  sla_approval_minutes: number; // 30 for emergencies
  auto_rollback_enabled: boolean;
  created_at: string;
}
```

### Resource URIs

| URI | Content Type | Description |
|-----|-------------|-------------|
| `changeflow://config/change-types` | JSON | Standard change type definitions |
| `changeflow://config/approval-matrix` | JSON | CAB approval requirements by risk |
| `changeflow://config/freeze-calendar` | JSON | Scheduled freeze periods |
| `changeflow://templates/emergency-change` | JSON | Emergency change request template |
| `changeflow://docs/itil-compliance` | Markdown | ITIL 4 compliance guidelines |

### Error Codes

| Code | Meaning | Description |
|------|---------|-------------|
| `-32601` | Method not found | Invalid MCP method |
| `-32602` | Invalid params | Missing or invalid parameters |
| `-32603` | Internal error | Server-side error |
| `-32700` | Parse error | Invalid JSON-RPC format |

## Troubleshooting

### FAQ

**Q: Why is the server returning "Method not found" for valid methods?**

A: Check the JSON-RPC format. Ensure you're sending:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "params": {},
  "id": 1
}
```

**Q: Tools are listed but execution fails?**

A: Verify the tool arguments match the expected schema. Use `tools/list` to see required parameters.

**Q: Resources return empty or error responses?**

A: Check the exact URI format. Resources use the `changeflow://` scheme.

**Q: Performance is slower than expected?**

A: For production, ensure you're using the CloudFlare Worker deployment, not the local Node.js server.

### Logging Levels

```bash
# Local development - verbose logging
MCP_LOG_LEVEL=debug node scripts/mcp-local-server.js

# Production - minimal logging
MCP_LOG_LEVEL=error node scripts/mcp-local-server.js
```

### Health Monitoring

```bash
# Basic health check
curl -f http://localhost:8080/ || echo "Server down"

# Advanced health monitoring
curl -s http://localhost:8080/ | jq -r '.status' | grep -q "healthy" && echo "OK" || echo "FAIL"
```

### Support

For issues not covered in this guide:

1. Check the [GitHub Issues](https://github.com/dsp-dr/guile-changeflow/issues)
2. Review server logs for error details
3. Test with the comprehensive test suite
4. Verify MCP protocol compliance with inspector tools

---

*Generated: 2025-09-14 | Version: 1.0.0 | Protocol: MCP 2024-11-05*