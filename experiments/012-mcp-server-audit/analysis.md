# MCP Server Audit Analysis - Experiment 012

## Executive Summary

Audit of 8 production MCP servers reveals key patterns for successful Claude.ai integration:

### Key Findings

1. **SSE (Server-Sent Events) is Standard**
   - 5/8 servers use explicit `/sse` endpoints
   - Linear, Square, Asana all follow this pattern
   - Our ChangeFlow server lacks SSE support

2. **CORS Configuration Critical**
   - Working servers (Linear, Square, Asana) all have:
     - `Access-Control-Allow-Origin: https://claude.ai`
     - `Access-Control-Allow-Headers: Authorization, *`
     - `Access-Control-Allow-Methods: *`
   - Our server has CORS headers but may need adjustment

3. **Authentication Required**
   - All servers return 401 without proper auth
   - OAuth flow is prerequisite for access
   - ChangeFlow is only server returning 200 (public access)

4. **Response Types**
   - Production servers use SSE for streaming
   - We use JSON-RPC (synchronous)
   - This mismatch may cause Claude.ai connection issues

## Server Comparison

| Server | SSE | CORS | Auth | Status |
|--------|-----|------|------|--------|
| Linear | ✅ | ✅ | 401 | Working |
| Square | ✅ | ✅ | 401 | Working |
| Asana | ✅ | ✅ | 401 | Working |
| Stripe | ❌ | ❌ | 401 | Unknown |
| Vercel | ❌ | ❌ | 401 | Unknown |
| Plaid | ✅ | ❌ | 403 | Unknown |
| HubSpot | ❌ | ⚠️ | 401 | Unknown |
| **ChangeFlow** | ❌ | ✅ | 200 | Not Working |

## Required Changes for ChangeFlow

### Immediate (Fix Claude.ai Connection)
1. **Add SSE Support**
   - Change endpoint from `/mcp` to `/mcp/sse`
   - Implement Server-Sent Events protocol
   - Stream responses instead of single JSON-RPC

2. **Update CORS Headers**
   - Match working servers exactly:
   ```javascript
   'Access-Control-Allow-Origin': 'https://claude.ai',
   'Access-Control-Allow-Headers': 'Authorization, *',
   'Access-Control-Allow-Methods': '*',
   'Access-Control-Max-Age': '86400'
   ```

3. **Require Authentication**
   - Return 401 for unauthenticated requests
   - Only serve after OAuth validation

### Architecture Pattern

Working servers follow this pattern:
```
GET /mcp/sse → 401 (requires auth)
OPTIONS /mcp/sse → 204 (CORS preflight)
POST /mcp/sse (with auth) → SSE stream
```

Our current pattern:
```
GET /mcp → 200 (public info)
OPTIONS /mcp → 204 (CORS)
POST /mcp → 200 (JSON-RPC response)
```

## Recommendation

Transform our synchronous JSON-RPC endpoint into an SSE streaming endpoint to match the pattern used by Linear, Square, and Asana. This is likely why Claude.ai can't establish a connection - it expects SSE streaming, not synchronous responses.