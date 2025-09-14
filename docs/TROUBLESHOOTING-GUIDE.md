# ChangeFlow MCP Troubleshooting Guide

## Quick Diagnostics

### 1. Check All Endpoints
```bash
gmake check-prod
```

**Expected Results:**
- Landing Page: 200 ✅
- Health Check: 200 ✅
- MCP Legacy: 200 ✅
- MCP SSE (no auth): 401 ✅ (correct - requires auth)
- Favicon: 200 ✅
- OAuth: 302 ✅ (redirect to GitHub)

### 2. Test MCP Protocol Sequence
```bash
# 1. Initialize handshake
curl -X POST -H "Authorization: Bearer test" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}}},"id":1}' \
     https://mcp.changeflow.us/v1/sse

# 2. List tools
curl -X POST -H "Authorization: Bearer test" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"tools/list","id":2}' \
     https://mcp.changeflow.us/v1/sse
```

## Common Issues & Solutions

### Issue 1: "OAuth not configured" (500 Error)

**Symptoms:**
- `/authorize` returns "OAuth not configured"
- OAuth flow doesn't start

**Cause:**
Missing `GITHUB_CLIENT_ID` environment variable in Cloudflare Workers

**Solutions:**
1. **Preferred**: Add to Cloudflare dashboard environment variables:
   - Key: `GITHUB_CLIENT_ID`
   - Value: `Ov23lir2JJgJffb51RPs`

2. **Fallback**: Already in code as hardcoded fallback

**Test Fix:**
```bash
curl -I https://mcp.changeflow.us/authorize
# Should return: HTTP/1.1 302 Found (redirect to GitHub)
```

### Issue 2: Claude.ai Connection Fails After OAuth

**Symptoms:**
- OAuth login successful
- Claude.ai shows "Connection failed" or similar error

**Cause:**
Missing MCP `initialize` handshake handler

**Solution:**
Ensure server handles `initialize` method before `tools/list`

**Test Fix:**
```bash
curl -X POST -H "Authorization: Bearer test" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}}},"id":1}' \
     https://mcp.changeflow.us/v1/sse
```

**Expected Response:**
```
data: {"type":"connected"}

data: {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05",...}}

data: {"type":"done"}
```

### Issue 3: Tools Not Discoverable

**Symptoms:**
- Connection successful but no tools appear in Claude.ai
- Empty tools list

**Cause:**
Incorrect `tools/list` response format or missing tools array

**Test:**
```bash
curl -X POST -H "Authorization: Bearer test" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"tools/list","id":2}' \
     https://mcp.changeflow.us/v1/sse | grep -o '"name":"[^"]*"'
```

**Expected**: 8 tool names including `create_change_request`, `assess_risk`, etc.

### Issue 4: SSE Format Errors

**Symptoms:**
- Protocol errors in Claude.ai
- Connection attempts but malformed responses

**Cause:**
Missing `data: ` prefix or incorrect SSE formatting

**Check:**
All SSE responses must be:
```
data: {"jsonrpc":"2.0",...}

data: {"type":"done"}

```

Note the `data: ` prefix and double newline endings.

### Issue 5: Authentication Failures

**Symptoms:**
- 401 Unauthorized on `/v1/sse`
- "Bearer token required" messages

**Expected Behavior:**
- `/v1/sse` without auth: 401 ✅ (correct)
- `/v1/sse` with `Authorization: Bearer <token>`: 200 ✅

**Test:**
```bash
# Should fail (correct)
curl https://mcp.changeflow.us/v1/sse
# Should work
curl -H "Authorization: Bearer test" https://mcp.changeflow.us/v1/sse
```

### Issue 6: Version Mismatches

**Symptoms:**
- Different versions reported by different endpoints
- Deployment not updating

**Check Version Consistency:**
```bash
curl -s https://mcp.changeflow.us/health | jq -r '.version'
curl -s https://mcp.changeflow.us/mcp | jq -r '.server_version'
```

**Both should return**: `1.3.1`

**Fix**: Redeploy with production environment:
```bash
gh workflow run "Deploy to Cloudflare Workers" --field environment=production
```

### Issue 7: CORS Errors

**Symptoms:**
- Browser console shows CORS errors
- Preflight OPTIONS requests failing

**Check CORS Headers:**
```bash
curl -I -X OPTIONS https://mcp.changeflow.us/v1/sse -H "Origin: https://claude.ai"
```

**Expected Headers:**
```
Access-Control-Allow-Origin: https://claude.ai
Access-Control-Allow-Methods: GET, POST, OPTIONS
Access-Control-Allow-Headers: Content-Type, Authorization, anthropic-version
```

## Environment Debugging

### Check Deployment Environment
```bash
curl -s https://mcp.changeflow.us/health | jq '.environment'
# Should return: "production"
```

### Check GitHub Actions Status
```bash
gh run list --repo dsp-dr/guile-changeflow --limit 5
```

### Check Cloudflare Workers Logs
Access Cloudflare dashboard → Workers → guile-changeflow-prod → Logs

## Sequence Validation

The complete flow should follow this order:

1. **OAuth Phase** ✅
   - `/authorize` → GitHub → `/callback` → Success

2. **MCP Initialize Phase** ✅
   - `POST /v1/sse` with `initialize` method
   - Server responds with capabilities

3. **Tools Discovery Phase** ✅
   - `POST /v1/sse` with `tools/list` method
   - Server responds with 8 tools

4. **Tool Usage Phase** ✅
   - `POST /v1/sse` with `tools/call` method
   - Server executes and responds

## Emergency Recovery

### Redeploy Latest Version
```bash
gh workflow run "Deploy to Cloudflare Workers" --repo dsp-dr/guile-changeflow --field environment=production
```

### Check Latest Release
```bash
gh release view --repo dsp-dr/guile-changeflow
```

### Validate Production Endpoints
```bash
gmake check-prod
```

## Support Information

- **Repository**: https://github.com/dsp-dr/guile-changeflow
- **Issues**: https://github.com/dsp-dr/guile-changeflow/issues
- **Latest Release**: https://github.com/dsp-dr/guile-changeflow/releases/latest
- **Documentation**: `docs/CLAUDE-AI-MCP-INTEGRATION.md`
- **Sequence Diagrams**: `docs/MCP-SEQUENCE-DIAGRAMS.md`

---

**Last Updated**: 2025-09-14
**Version**: 1.3.1
**Status**: Production Ready ✅