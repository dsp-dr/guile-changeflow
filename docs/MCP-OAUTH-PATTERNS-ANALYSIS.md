# OAuth Patterns for Remote MCP Servers

## How Different Services Handle OAuth for MCP

### 1. Google/Gmail MCP Servers

When an MCP server needs to access Gmail or Google services, the flow is:

```
Claude.ai → Your MCP Server → Google OAuth → Back to MCP → Claude.ai
```

**Two-Layer OAuth Pattern:**

1. **Layer 1: Claude.ai ↔ Your MCP Server**
   - Claude.ai authenticates with YOUR server
   - Uses OAuth 2.0 + PKCE
   - Scope: `claudeai` or custom scope
   - Purpose: Authorize Claude to use your MCP server

2. **Layer 2: Your MCP Server ↔ Google**
   - Your server authenticates with Google
   - Uses standard Google OAuth 2.0
   - Scope: `https://www.googleapis.com/auth/gmail.readonly` etc.
   - Purpose: Allow your server to access user's Gmail

**Implementation Pattern:**

```javascript
case '/authorize':
  const scope = url.searchParams.get('scope');

  if (scope === 'claudeai') {
    // Step 1: Show consent page explaining Google access
    return showConsentPage({
      message: "This MCP server needs access to your Gmail",
      willRedirectTo: "Google OAuth"
    });
  }

  // After user consents, in POST handler:
  if (action === 'approve') {
    // Store Claude's callback for later
    const sessionId = crypto.randomUUID();
    sessions.set(sessionId, {
      claudeRedirect: formData.get('redirect_uri'),
      claudeState: formData.get('state'),
      claudeChallenge: formData.get('code_challenge')
    });

    // Redirect to Google OAuth
    const googleAuthUrl = new URL('https://accounts.google.com/o/oauth2/v2/auth');
    googleAuthUrl.searchParams.set('client_id', GOOGLE_CLIENT_ID);
    googleAuthUrl.searchParams.set('redirect_uri', `${origin}/google-callback`);
    googleAuthUrl.searchParams.set('scope', 'https://www.googleapis.com/auth/gmail.readonly');
    googleAuthUrl.searchParams.set('state', sessionId);
    googleAuthUrl.searchParams.set('response_type', 'code');

    return Response.redirect(googleAuthUrl, 302);
  }

case '/google-callback':
  // Google returns here with authorization code
  const googleCode = url.searchParams.get('code');
  const sessionId = url.searchParams.get('state');

  // Exchange for Google tokens
  const googleTokens = await exchangeGoogleCode(googleCode);

  // Store Google tokens associated with session
  const session = sessions.get(sessionId);
  session.googleAccessToken = googleTokens.access_token;
  session.googleRefreshToken = googleTokens.refresh_token;

  // Generate OUR auth code for Claude
  const mcpAuthCode = crypto.randomUUID();
  authCodes.set(mcpAuthCode, session);

  // Redirect back to Claude.ai with our code
  return Response.redirect(
    `${session.claudeRedirect}?code=${mcpAuthCode}&state=${session.claudeState}`,
    302
  );

case '/token':
  // Claude exchanges our code for access token
  const code = body.code;
  const session = authCodes.get(code);

  // Generate access token that includes Google token reference
  const accessToken = crypto.randomUUID();
  tokens.set(accessToken, {
    googleAccessToken: session.googleAccessToken,
    googleRefreshToken: session.googleRefreshToken,
    claudeClient: body.client_id
  });

  return {
    access_token: accessToken,
    token_type: 'Bearer',
    scope: 'claudeai gmail.readonly'
  };

case '/v1/sse':
  // When Claude uses the MCP server
  const token = getBearerToken(request);
  const tokenData = tokens.get(token);

  // Now we can use tokenData.googleAccessToken to call Gmail APIs
  // in response to MCP tool calls
```

### 2. Microsoft/Office 365 MCP Servers

Similar pattern but with Azure AD:

```javascript
// Microsoft OAuth URLs
const MICROSOFT_AUTH_URL = 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';
const MICROSOFT_TOKEN_URL = 'https://login.microsoftonline.com/common/oauth2/v2.0/token';

// Scopes needed
const MICROSOFT_SCOPES = [
  'https://graph.microsoft.com/Mail.Read',
  'https://graph.microsoft.com/Calendars.Read'
];
```

### 3. Slack MCP Servers

Slack uses OAuth 2.0 with workspace installation:

```javascript
// Slack OAuth
const SLACK_AUTH_URL = 'https://slack.com/oauth/v2/authorize';
const SLACK_SCOPES = 'channels:read,chat:write,users:read';

// After OAuth, store workspace token
workspaceTokens.set(teamId, {
  accessToken: slackToken,
  teamName: teamName,
  claudeToken: mcpToken
});
```

### 4. GitHub MCP Servers (Fixed Implementation)

For GitHub integration, you have two choices:

**Option A: User's Own GitHub Account**
```javascript
// Each user authenticates with their GitHub
if (scope === 'claudeai') {
  // Redirect to GitHub OAuth for that user
  redirectToGitHub(userSpecificClientId);
}
```

**Option B: Service Account**
```javascript
// Use a single GitHub App or service account
const GITHUB_APP_TOKEN = env.GITHUB_APP_PRIVATE_KEY;
// All MCP operations use this service account
```

### 5. Database/Internal Services

For internal services without OAuth:

```javascript
case '/authorize':
  // Simple API key or password authentication
  if (scope === 'claudeai') {
    // Show login form
    return showLoginPage();
  }

case '/authenticate':
  // Validate credentials
  const { username, password } = await request.json();
  if (validateCredentials(username, password)) {
    const code = generateAuthCode();
    return redirectToClaude(code);
  }
```

## Key Patterns

### Pattern 1: Token Chain
```
Claude Token → MCP Server Token → External Service Token
```

Each token authorizes the next layer:
- Claude's token authorizes MCP access
- MCP's token contains reference to external service token
- External token used for actual API calls

### Pattern 2: Consent Aggregation

Show all permissions upfront:
```html
<div class="consent-page">
  <h1>MCP Server requires access to:</h1>
  <ul>
    <li>✓ Your Gmail messages</li>
    <li>✓ Your Google Calendar</li>
    <li>✓ Your Google Drive files</li>
  </ul>
  <button>Approve All</button>
</div>
```

### Pattern 3: Token Storage Strategies

**Stateless (JWT-like):**
```javascript
const token = {
  iss: 'mcp-server',
  sub: clientId,
  google_token: encryptedGoogleToken,
  exp: Date.now() + 3600000
};
const accessToken = btoa(JSON.stringify(token));
```

**Stateful (KV/Database):**
```javascript
await env.TOKENS.put(accessToken, JSON.stringify({
  googleToken,
  msToken,
  slackToken
}), { expirationTtl: 3600 });
```

### Pattern 4: Refresh Token Handling

```javascript
async function callGoogleAPI(token) {
  try {
    return await fetch(googleAPI, {
      headers: { Authorization: `Bearer ${token.googleAccessToken}` }
    });
  } catch (e) {
    if (e.status === 401) {
      // Refresh the Google token
      const newToken = await refreshGoogleToken(token.googleRefreshToken);
      token.googleAccessToken = newToken;
      return await fetch(googleAPI, {
        headers: { Authorization: `Bearer ${newToken}` }
      });
    }
  }
}
```

## Security Considerations

1. **Never expose service tokens to Claude.ai**
   - Claude gets MCP tokens
   - MCP server holds service tokens
   - Service tokens never sent to client

2. **Scope minimization**
   - Request minimum necessary scopes
   - Separate read/write permissions
   - Time-limited tokens

3. **Token isolation**
   - Each Claude session gets unique tokens
   - Tokens bound to specific client_id
   - Regular token rotation

## Implementation Recommendations

### For Gmail/Google Services:
1. Use Google OAuth 2.0 for user consent
2. Store refresh tokens securely
3. Implement token refresh logic
4. Use Google Client Library

### For Microsoft/Office:
1. Register app in Azure AD
2. Use MSAL library for auth
3. Handle multi-tenant scenarios
4. Implement conditional access

### For Slack/Discord:
1. Use OAuth 2.0 with bot scopes
2. Store tokens per workspace/guild
3. Handle workspace/guild removal
4. Implement event webhooks

### For Custom Services:
1. Implement standard OAuth 2.0
2. Use PKCE for security
3. Short-lived access tokens
4. Secure token storage

## The Critical Insight

**MCP servers are OAuth middlemen:**
- They receive OAuth from Claude.ai
- They perform OAuth with external services
- They bridge the two authentication systems

This is why our current implementation fails - we're trying to pass through GitHub OAuth instead of properly bridging it!

---
Generated: 2025-09-15 02:10 UTC