# Complete MCP Remote Server Implementation Guide

## Critical Discovery: Two Specification Versions!

### Old Spec (2024-11-05) - What We're Using
- HTTP + SSE transport
- Basic OAuth flow
- Persistent connections required

### New Spec (2025-03-26) - What Claude Supports
- Streamable HTTP transport (SSE optional)
- OAuth 2.1 with PKCE mandatory
- Resource indicators (RFC 8707)
- Dynamic Client Registration (DCR)

## Claude.ai OAuth Flow (Current Implementation)

### 1. Discovery Phase
Claude.ai first checks:
```
GET /.well-known/oauth-authorization-server
```

Expected response:
```json
{
  "issuer": "https://mcp.changeflow.us",
  "authorization_endpoint": "https://mcp.changeflow.us/authorize",
  "token_endpoint": "https://mcp.changeflow.us/token",
  "registration_endpoint": "https://mcp.changeflow.us/register",
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "refresh_token"],
  "code_challenge_methods_supported": ["S256"],
  "token_endpoint_auth_methods_supported": ["none"]
}
```

### 2. Dynamic Client Registration (Optional)
If no pre-configured client:
```
POST /register
Content-Type: application/json

{
  "client_name": "Claude",
  "redirect_uris": ["https://claude.ai/api/mcp/auth_callback"],
  "grant_types": ["authorization_code"],
  "response_types": ["code"],
  "token_endpoint_auth_method": "none"
}
```

Response:
```json
{
  "client_id": "8f73a025-a1f2-4c84-8f2d-43b77ec9117f",
  "client_name": "Claude",
  "redirect_uris": ["https://claude.ai/api/mcp/auth_callback"]
}
```

### 3. Authorization Request
Claude.ai redirects user to:
```
GET /authorize?
  response_type=code
  &client_id=8f73a025-a1f2-4c84-8f2d-43b77ec9117f
  &redirect_uri=https://claude.ai/api/mcp/auth_callback
  &code_challenge=oeqH6ISYQcPPFt9pmvzU1rqJEMiMC4ZyaelL1HJMaug
  &code_challenge_method=S256
  &state=CXHJxTJtGDwnubO5omp-45zXcf7YmRRKEFtKr-B1Xe4
  &scope=mcp
```

### 4. User Consent
Show consent page, then redirect:
```
302 Location: https://claude.ai/api/mcp/auth_callback?
  code=AUTH_CODE_HERE
  &state=CXHJxTJtGDwnubO5omp-45zXcf7YmRRKEFtKr-B1Xe4
```

### 5. Token Exchange
Claude.ai exchanges code:
```
POST /token
Content-Type: application/json

{
  "grant_type": "authorization_code",
  "code": "AUTH_CODE_HERE",
  "redirect_uri": "https://claude.ai/api/mcp/auth_callback",
  "code_verifier": "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk",
  "client_id": "8f73a025-a1f2-4c84-8f2d-43b77ec9117f"
}
```

Response:
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIs...",
  "token_type": "Bearer",
  "expires_in": 3600,
  "refresh_token": "8xLOxBtZp8",
  "scope": "mcp"
}
```

### 6. MCP Protocol Access
Claude.ai connects with token:
```
GET /v1/sse
Authorization: Bearer eyJhbGciOiJIUzI1NiIs...
```

Or for new spec (Streamable HTTP):
```
POST /
Authorization: Bearer eyJhbGciOiJIUzI1NiIs...
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-03-26",
    "capabilities": {}
  },
  "id": 1
}
```

## Critical Implementation Requirements

### 1. PKCE Validation (MANDATORY)
```javascript
// Store with auth code
authCodes.set(code, {
  challenge: code_challenge,
  method: code_challenge_method
});

// Validate in token endpoint
const verifier = body.code_verifier;
const challenge = authCode.challenge;

if (authCode.method === 'S256') {
  const encoder = new TextEncoder();
  const data = encoder.encode(verifier);
  const hashBuffer = await crypto.subtle.digest('SHA-256', data);
  const hashArray = new Uint8Array(hashBuffer);

  // Base64URL encode
  const computed = btoa(String.fromCharCode(...hashArray))
    .replace(/=/g, '')
    .replace(/\+/g, '-')
    .replace(/\//g, '_');

  if (computed !== challenge) {
    return error(400, 'invalid_grant', 'PKCE validation failed');
  }
}
```

### 2. Token Expiry and Refresh
```javascript
// Include refresh token
{
  "access_token": "...",
  "refresh_token": "...",
  "expires_in": 3600
}

// Handle refresh
if (body.grant_type === 'refresh_token') {
  const refreshToken = body.refresh_token;
  // Validate and issue new tokens
}
```

### 3. Error Responses
When auth fails:
```javascript
// 401 with specific error
return new Response(JSON.stringify({
  "error": "invalid_token",
  "error_description": "The access token expired"
}), {
  status: 401,
  headers: {
    'WWW-Authenticate': 'Bearer error="invalid_token"',
    'Content-Type': 'application/json'
  }
});
```

For invalid client (triggers re-auth):
```javascript
{
  "error": "invalid_client",
  "error_description": "Client authentication failed"
}
```

## Working Implementation Pattern

### For Services Requiring External Auth (Gmail, GitHub, etc.)

```javascript
// Step 1: Claude OAuth (Layer 1)
case '/authorize':
  if (isClaudeRequest(request)) {
    // Show consent explaining external service access
    return showConsentPage({
      services: ['GitHub', 'Gmail'],
      permissions: ['read:email', 'read:repos']
    });
  }

// Step 2: External Service OAuth (Layer 2)
case '/consent-approved':
  // Store Claude callback
  const session = {
    claudeCallback: params.redirect_uri,
    claudeState: params.state,
    claudeChallenge: params.code_challenge
  };
  sessions.set(sessionId, session);

  // Redirect to external service
  return redirectToGitHub(sessionId);

// Step 3: External Callback
case '/github-callback':
  const githubToken = await exchangeGitHubCode(code);
  const session = sessions.get(sessionId);

  // Generate our auth code
  const mcpCode = generateCode();
  authCodes.set(mcpCode, {
    ...session,
    githubToken
  });

  // Return to Claude
  return redirect(`${session.claudeCallback}?code=${mcpCode}&state=${session.claudeState}`);

// Step 4: Token Exchange
case '/token':
  const authCode = authCodes.get(body.code);
  // Validate PKCE...

  const token = generateToken();
  tokens.set(token, {
    githubToken: authCode.githubToken,
    expires: Date.now() + 3600000
  });

  return { access_token: token };

// Step 5: Use External Service
case '/v1/sse':
  const token = getBearer(request);
  const tokenData = tokens.get(token);

  // Use tokenData.githubToken for GitHub API calls
  // in response to MCP tool invocations
```

### For Simple Auth (No External Services)

```javascript
// Simplified for services that don't need external OAuth
case '/authorize':
  // Just show consent and approve
  if (action === 'approve') {
    const code = generateCode();
    storeCode(code, params);
    return redirect(`${params.redirect_uri}?code=${code}&state=${params.state}`);
  }

case '/token':
  // Validate code and PKCE
  const token = generateToken();
  return { access_token: token, token_type: 'Bearer' };

case '/v1/sse':
  // Validate token and provide MCP tools
  if (validateToken(request)) {
    return handleMCPProtocol(request);
  }
  return unauthorized();
```

## Common Pitfalls

1. **Missing PKCE validation** - Claude.ai REQUIRES it
2. **Not storing auth codes** - Stateless workers need KV/Durable Objects
3. **Mixing OAuth flows** - Don't chain external OAuth incorrectly
4. **Missing discovery endpoint** - Claude needs `/.well-known/oauth-authorization-server`
5. **Wrong redirect URI** - Must match `https://claude.ai/api/mcp/auth_callback`
6. **Invalid token format** - Use proper Bearer token format
7. **Not handling token expiry** - Implement refresh tokens

## Testing

1. Use MCP Inspector: `npx @modelcontextprotocol/inspector`
2. Add directly in Claude.ai settings
3. Check browser network tab for OAuth flow
4. Monitor server logs for errors

## Resources

- [MCP Specification](https://modelcontextprotocol.io/specification)
- [OAuth 2.1 Spec](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-v2-1-07)
- [PKCE RFC 7636](https://datatracker.ietf.org/doc/html/rfc7636)
- [Claude Support](https://support.claude.com/en/articles/11503834)

---
Generated: 2025-09-15 02:20 UTC