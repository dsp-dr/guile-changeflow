# Claude.ai Connection Troubleshooting

## Error Message
"Error connecting to ChangeFlow ITIL. Please confirm that you have permission to access the service, that you're using the correct credentials, and that your server handles auth correctly."

## What's Working ✅

1. **OAuth Discovery**: `/.well-known/oauth-authorization-server` returns correct endpoints
2. **401 Response**: `/v1/sse` correctly returns 401 with WWW-Authenticate header
3. **Authorization Page**: `/authorize` shows consent page
4. **Token Exchange**: `/token` generates valid tokens
5. **SSE Stream**: Authenticated requests get MCP ready message
6. **CORS Headers**: Properly configured for claude.ai origin

## Potential Issues

### 1. Token Format
Claude.ai might expect specific token formats:
- We're using base64-encoded JSON (stateless)
- Claude might expect JWT or opaque tokens
- Solution: Try standard JWT format

### 2. PKCE Validation
Our PKCE might not match exactly what Claude sends:
- Base64 vs Base64URL encoding differences
- Padding issues
- Solution: Log actual values Claude sends

### 3. Session Management
Claude might expect:
- Persistent token storage
- Refresh token support
- Session cookies

### 4. Content-Type Headers
Token endpoint might need to support both:
- `application/json`
- `application/x-www-form-urlencoded`

## Debug Steps

### 1. Browser Console
Open browser console when adding the MCP server and look for:
```javascript
// Network tab - check these requests:
GET /v1/sse → 401
GET /.well-known/oauth-authorization-server → 200
POST /register → 201 (if DCR used)
GET /authorize → 200 (consent page)
POST /token → 200 (token response)
GET /v1/sse → 200 (with Bearer token)
```

### 2. Check Exact Error
In browser console, look for:
- Specific error messages
- Failed requests
- CORS errors
- Token validation errors

### 3. Test with Simpler Token
Try hardcoding a simple token validation:
```javascript
if (token === 'test-token-claude') {
  isAuthenticated = true;
}
```

### 4. Add Logging
Add console logging to see what's happening:
```javascript
console.log('Token received:', token);
console.log('Token validation result:', isAuthenticated);
```

## Quick Fixes to Try

### Fix 1: Support Form-Encoded Token Request
```javascript
let params;
if (contentType?.includes('application/json')) {
  params = await request.json();
} else if (contentType?.includes('application/x-www-form-urlencoded')) {
  const formData = await request.formData();
  params = Object.fromEntries(formData);
}
```

### Fix 2: Add Refresh Token Support
```javascript
if (grantType === 'refresh_token') {
  // Generate new access token
  return { access_token: newToken, ... };
}
```

### Fix 3: Improve Error Messages
```javascript
return new Response(JSON.stringify({
  error: 'invalid_grant',
  error_description: 'The authorization code is invalid or expired',
  error_uri: 'https://github.com/dsp-dr/guile-changeflow/issues'
}), { status: 400 });
```

## Testing Without Claude

### Using MCP Inspector
```bash
npx @modelcontextprotocol/inspector
# Connect to: https://mcp.changeflow.us/v1/sse
```

### Manual OAuth Flow
1. Open: `https://mcp.changeflow.us/authorize?response_type=code&client_id=test&redirect_uri=https://claude.ai/api/mcp/auth_callback&state=test`
2. Approve consent
3. Copy code from redirect URL
4. Exchange for token
5. Use token with SSE endpoint

## Known Working MCP Servers

Compare our implementation with:
- `bindings.mcp.cloudflare.com`
- `mcp.atlassian.com`
- `huggingface.co/mcp`

These all use similar OAuth flows but might have subtle differences in:
- Token format
- PKCE validation
- Error responses
- Session handling

## Next Steps

1. **Check Browser Console**: Most important - see actual error
2. **Test with curl**: Verify each endpoint independently
3. **Compare with Working Servers**: Use browser network tab
4. **Simplify Token Validation**: Temporarily accept any token
5. **Add Detailed Logging**: See what Claude is actually sending

The OAuth flow IS working technically, but Claude.ai might have specific requirements we're not meeting.

---
Generated: 2025-09-24 23:20 UTC