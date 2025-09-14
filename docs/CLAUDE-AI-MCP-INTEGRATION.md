# Claude.ai MCP Integration Guide

## Overview

This document explains the complete integration flow between Claude.ai and the ChangeFlow MCP server, including troubleshooting the sequence diagrams and protocol requirements.

## Architecture

```
┌─────────────┐    OAuth     ┌──────────────┐    MCP/SSE    ┌─────────────┐
│  Claude.ai  │ ──────────── │   GitHub     │ ───────────── │ ChangeFlow  │
│   Client    │              │    OAuth     │               │ MCP Server  │
└─────────────┘              └──────────────┘               └─────────────┘
      │                             │                             │
      │ 1. Add Custom Connector     │                             │
      │ https://mcp.changeflow.us/v1/sse                          │
      │                             │                             │
      │ 2. OAuth Authorization      │                             │
      │ ──────────────────────────► │ 3. GitHub Login            │
      │                             │ ──────────────────────────► │
      │                             │ 4. Success + Token         │
      │ 5. MCP Initialize           │ ◄────────────────────────── │
      │ ──────────────────────────────────────────────────────► │
      │ 6. MCP Tools List           │                             │
      │ ──────────────────────────────────────────────────────► │
      │ 7. MCP Tool Calls           │                             │
      │ ──────────────────────────────────────────────────────► │
```

## Complete Flow Sequence

### Phase 1: OAuth Authentication
1. User visits: `https://mcp.changeflow.us/authorize`
2. Redirects to GitHub OAuth
3. User authorizes application
4. Callback to: `https://mcp.changeflow.us/callback`
5. Success page shows: `https://mcp.changeflow.us/v1/sse`

### Phase 2: Claude.ai MCP Connection
1. User adds Custom Connector: `https://mcp.changeflow.us/v1/sse`
2. Claude.ai sends MCP Initialize handshake
3. Server responds with capabilities
4. Claude.ai requests tools list
5. Server responds with 8 ITIL tools
6. Tools are available in Claude.ai interface

## Critical Protocol Requirements

### 1. MCP Initialize Handshake

**Claude.ai sends:**
```json
{
  "jsonrpc": "2.0",
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {"tools": {}}
  },
  "id": 1
}
```

**Server must respond:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools": {"listChanged": false},
      "resources": {},
      "prompts": {},
      "logging": {}
    },
    "serverInfo": {
      "name": "guile-changeflow",
      "version": "1.3.1"
    }
  }
}
```

### 2. SSE Transport Format

All responses over `/v1/sse` use Server-Sent Events format:
```
data: {"type":"connected"}

data: {"jsonrpc":"2.0","id":1,"result":{...}}

data: {"type":"done"}
```

### 3. Authentication Headers

**Required:**
- `Authorization: Bearer <token>`
- `Content-Type: application/json`

**Returns 401 without Bearer token.**

## Endpoints

| Endpoint | Method | Purpose | Format |
|----------|--------|---------|--------|
| `/` | GET | Landing page | HTML |
| `/authorize` | GET | GitHub OAuth start | 302 Redirect |
| `/callback` | GET | OAuth callback | HTML Success |
| `/v1/sse` | GET | SSE connection info | SSE Stream |
| `/v1/sse` | POST | MCP protocol over SSE | SSE Stream |
| `/mcp` | GET | Server info (legacy) | JSON |
| `/mcp` | POST | MCP protocol (legacy) | JSON |
| `/health` | GET | Health check | JSON |
| `/favicon.ico` | GET | ChangeFlow logo | ICO |

## Troubleshooting

### Issue: "OAuth not configured"
**Symptom**: `/authorize` returns 500 error
**Cause**: Missing `GITHUB_CLIENT_ID` environment variable
**Fix**: Add to Cloudflare Workers environment or fallback in code

### Issue: "Connection failed after OAuth"
**Symptom**: OAuth works but Claude.ai can't connect
**Cause**: Missing MCP `initialize` method handler
**Fix**: Server must handle `initialize` before `tools/list`

### Issue: "Tools not discoverable"
**Symptom**: Connection works but no tools appear
**Cause**: Incorrect `tools/list` response format
**Fix**: Ensure proper JSON-RPC response with `result.tools` array

### Issue: "SSE format errors"
**Symptom**: Connection attempts but protocol errors
**Cause**: Missing `data: ` prefix or incorrect SSE format
**Fix**: All SSE responses must be prefixed with `data: `

## Testing Commands

### 1. Test OAuth Flow
```bash
curl -I https://mcp.changeflow.us/authorize
# Should return: 302 Found (redirect to GitHub)
```

### 2. Test MCP Initialize
```bash
curl -X POST -H "Authorization: Bearer test" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}}},"id":1}' \
     https://mcp.changeflow.us/v1/sse
```

**Expected:**
```
data: {"type":"connected"}

data: {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05",...}}

data: {"type":"done"}
```

### 3. Test Tools List
```bash
curl -X POST -H "Authorization: Bearer test" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"tools/list","id":2}' \
     https://mcp.changeflow.us/v1/sse
```

**Expected**: 8 ITIL tools in SSE format

## Available Tools

1. **create_change_request** - Create ITIL change request
2. **assess_risk** - Evaluate change risk levels
3. **check_freeze_period** - Verify deployment windows
4. **get_change_request** - Retrieve change details
5. **list_change_requests** - List all changes
6. **get_approval_status** - Check CAB approval
7. **emergency_override** - Request emergency changes
8. **audit_trail** - View audit history

## Implementation Notes

- **Single File**: Everything in `infra/cloudflare/worker.js`
- **No Build System**: Monolithic deployment approach
- **Fallback Auth**: Hardcoded Client ID if env var missing
- **CORS Headers**: Specific to `https://claude.ai`
- **Version**: 1.3.1 with SSE support

## Success Validation

✅ **OAuth**: GitHub authorization working
✅ **SSE**: Server-Sent Events format correct
✅ **Initialize**: MCP handshake implemented
✅ **Tools**: All 8 ITIL tools discoverable
✅ **Auth**: Bearer token validation working

---

**Last Updated**: 2025-09-14
**Version**: 1.3.1
**Status**: Production Ready