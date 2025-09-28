/**
 * ChangeFlow MCP Server - Complete OAuth + MCP in ONE FILE
 * No imports, no build system, everything inline
 */

// Inline HTML for landing page (as function to use SERVER_VERSION)
const getLandingHTML = (hostname) => {
  // Check if this is the root domain request
  const isRootDomain = hostname === 'www.changeflow.us' || hostname === 'changeflow.us';

  if (isRootDomain) {
    // Enhanced landing page for root domain
    return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>ChangeFlow - ITIL 4 Change Management MCP Server</title>
<link rel="icon" type="image/x-icon" href="/favicon.ico">
<meta name="description" content="ChangeFlow MCP Server for ITIL 4 Change Management integration with Claude.ai">
<meta property="og:title" content="ChangeFlow MCP Server">
<meta property="og:description" content="ITIL 4 Change Management tools for Claude.ai">
<meta property="og:url" content="https://changeflow.us">
<style>
body{font-family:system-ui,-apple-system,sans-serif;background:linear-gradient(135deg,#667eea 0%,#764ba2 100%);color:white;margin:0;padding:0;display:flex;align-items:center;justify-content:center;min-height:100vh}
.container{text-align:center;padding:2rem;background:rgba(255,255,255,0.1);border-radius:1rem;backdrop-filter:blur(10px);max-width:600px;margin:1rem}
h1{margin:0 0 1rem 0;font-size:2.5rem}
.logo{font-size:4rem;margin-bottom:1rem}
p{margin:1rem 0;font-size:1.1rem;line-height:1.6}
a{color:#60a5fa;text-decoration:none;font-weight:bold}
a:hover{text-decoration:underline}
.features{background:rgba(0,0,0,0.2);padding:1.5rem;border-radius:0.5rem;margin:2rem 0}
.features h2{margin-top:0;color:#93c5fd}
.features ul{text-align:left;margin:0 auto;max-width:400px;list-style:none;padding:0}
.features li{padding:0.5rem 0;position:relative;padding-left:1.5rem}
.features li:before{content:"✓";position:absolute;left:0;color:#34d399;font-weight:bold}
.cta{margin-top:2rem;padding:1rem;background:rgba(59,130,246,0.2);border-radius:0.5rem}
.version{font-size:0.9rem;opacity:0.8;margin-top:2rem}
.gh-link{position:fixed;top:1rem;right:1rem;opacity:0.7;transition:opacity 0.3s}
.gh-link:hover{opacity:1}
</style>
</head>
<body>
<a href="https://github.com/dsp-dr/guile-changeflow" class="gh-link">
<svg width="32" height="32" viewBox="0 0 24 24" fill="white"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>
</a>
<div class="container">
  <div class="logo">🔄</div>
  <h1>ChangeFlow</h1>
  <p><strong>ITIL 4 Change Management MCP Server</strong></p>
  <p>Enterprise-grade change management capabilities for Claude.ai through the Model Context Protocol.</p>

  <div class="features">
    <h2>Features</h2>
    <ul>
      <li>Create and manage change requests</li>
      <li>Risk assessment and analysis</li>
      <li>Freeze period management</li>
      <li>CAB approval workflows</li>
      <li>Emergency change handling</li>
      <li>Audit trail and compliance</li>
    </ul>
  </div>

  <div class="cta">
    <p><strong>Connect with Claude.ai:</strong></p>
    <p><code style="background:rgba(0,0,0,0.3);padding:0.25rem 0.5rem;border-radius:0.25rem;display:inline-block">https://mcp.changeflow.us</code></p>
    <p style="margin-top:1rem">
      <a href="https://mcp.changeflow.us" style="background:rgba(255,255,255,0.2);padding:0.75rem 1.5rem;border-radius:0.5rem;display:inline-block">Visit MCP Server →</a>
    </p>
  </div>

  <p class="version">Version ${SERVER_VERSION} • OAuth 2.0 + SSE Ready</p>
</div>
</body>
</html>`;
  }

  // Default landing page for MCP subdomain
  return `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>ChangeFlow MCP</title>
<link rel="icon" type="image/x-icon" href="/favicon.ico">
<style>body{font-family:system-ui;background:#0f172a;color:#e2e8f0;margin:0;padding:2rem;text-align:center}h1{color:#3b82f6}a{color:#3b82f6;text-decoration:none;padding:1rem 2rem;background:#1e293b;display:inline-block;margin:1rem;border-radius:0.5rem}a:hover{background:#334155}.version{color:#64748b;font-size:0.9rem}.gh-link{position:absolute;top:1rem;right:1rem;color:#64748b;text-decoration:none}.gh-link:hover{color:#94a3b8}</style>
</head>
<body>
<a href="https://github.com/dsp-dr/guile-changeflow" class="gh-link">
<svg width="24" height="24" viewBox="0 0 24 24" fill="currentColor"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>
</a>
<h1>🔄 ChangeFlow MCP Server</h1>
<p>ITIL 4 Change Management for AI</p>
<p class="version"><span data-version="${SERVER_VERSION}" id="semver">${SERVER_VERSION}</span> - OAuth + SSE Ready!</p>
<p><a href="/authorize">🔑 Authorize with OAuth</a></p>
</body>
</html>`;
};

// Success page HTML
const SUCCESS_HTML = `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Authorization Successful</title>
<style>body{font-family:system-ui;background:#0f172a;color:#e2e8f0;margin:0;padding:2rem;text-align:center}h1{color:#10b981}code{background:#1e293b;padding:1rem;display:block;margin:2rem;border-radius:0.5rem}</style>
</head>
<body>
<h1>✅ Authorization Successful!</h1>
<p>You can now use ChangeFlow MCP with Claude.ai</p>
<p>Add this URL to Claude.ai Custom Connectors:</p>
<code>https://mcp.changeflow.us/v1/sse</code>
<p><a href="https://claude.ai/settings/connectors" style="color:#3b82f6">Open Claude.ai Settings</a></p>
</body>
</html>`;

// Error page HTML
const ERROR_HTML = `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Authorization Failed</title>
<style>body{font-family:system-ui;background:#0f172a;color:#e2e8f0;margin:0;padding:2rem;text-align:center}h1{color:#ef4444}</style>
</head>
<body>
<h1>❌ Authorization Failed</h1>
<p>Something went wrong during authorization.</p>
<p><a href="/" style="color:#3b82f6">Try Again</a></p>
</body>
</html>`;

// Server Configuration
const SERVER_VERSION = '1.6.3';

// Inline ITIL Service Class
class ITILService {
  constructor(kv) {
    this.kv = kv;
    this.FREEZE_PERIODS = [
      { name: 'Black Friday', start: '2025-11-25', end: '2025-12-02' },
      { name: 'Year End', start: '2025-12-20', end: '2026-01-05' },
      { name: 'Quarterly Close', pattern: 'lastWeekOfQuarter' }
    ];
  }

  async createChangeRequest(data) {
    const changeId = `CHG-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    const change = {
      id: changeId,
      title: data.title,
      description: data.description,
      environment: data.environment,
      changeType: data.changeType || 'normal',
      state: 'new',
      priority: data.priority || 3,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString()
    };

    if (this.kv) {
      await this.kv.put(changeId, JSON.stringify(change));
    }
    return change;
  }

  async assessRisk(changeId) {
    const change = await this.getChange(changeId);
    if (!change) throw new Error('Change not found');

    const factors = {
      environment: { production: 3, staging: 2, development: 1 },
      changeType: { emergency: 4, normal: 2, standard: 1 }
    };

    let riskScore = 0;
    riskScore += factors.environment[change.environment] || 2;
    riskScore += factors.changeType[change.changeType] || 2;

    let riskLevel;
    if (riskScore <= 3) riskLevel = 'LOW';
    else if (riskScore <= 5) riskLevel = 'MEDIUM';
    else if (riskScore <= 7) riskLevel = 'HIGH';
    else riskLevel = 'CRITICAL';

    return { score: riskScore, level: riskLevel, assessedAt: new Date().toISOString() };
  }

  checkFreezePeriod(date) {
    const checkDate = new Date(date);
    for (const freeze of this.FREEZE_PERIODS) {
      if (freeze.pattern === 'lastWeekOfQuarter') {
        const month = checkDate.getMonth();
        const day = checkDate.getDate();
        if ([2, 5, 8, 11].includes(month)) {
          const lastDay = new Date(checkDate.getFullYear(), month + 1, 0).getDate();
          if (day > lastDay - 7) {
            return { inFreeze: true, period: freeze.name, reason: 'Quarterly close freeze period' };
          }
        }
      } else if (freeze.start && freeze.end) {
        const start = new Date(freeze.start);
        const end = new Date(freeze.end);
        if (checkDate >= start && checkDate <= end) {
          return { inFreeze: true, period: freeze.name, reason: `${freeze.name} freeze period` };
        }
      }
    }
    return { inFreeze: false };
  }

  async getChange(changeId) {
    if (!this.kv) return null;
    const data = await this.kv.get(changeId);
    return data ? JSON.parse(data) : null;
  }

  async getActiveChanges() {
    // Simplified version - in production would use proper indexing
    return [];
  }
}

// OAuth URLs
const GITHUB_OAUTH_URL = 'https://github.com/login/oauth/authorize';
const GITHUB_TOKEN_URL = 'https://github.com/login/oauth/access_token';

// ITIL Change Management - Risk factors
const RISK_FACTORS = {
  production: 40,
  security: 30,
  payment: 20,
  emergency: 25,
  systemImpact: 10,
  baseScore: 10
};

// In-memory storage (use KV in production)
const changeRequests = new Map();
const auditTrail = [];

// Freeze periods
const freezePeriods = [
  { start: '2025-12-20', end: '2026-01-05', name: 'Holiday Season Freeze', type: 'full' },
  { start: '2025-11-01', end: '2025-12-02', name: 'Pre-Cyber Monday Freeze', type: 'partial' },
  { start: '2025-11-24', end: '2025-12-02', name: 'Black Friday/Cyber Monday', type: 'full' },
  { start: '2025-11-25', end: '2025-11-30', name: 'Thanksgiving Weekend', type: 'full' },
  { start: '2025-07-03', end: '2025-07-06', name: 'Independence Day Weekend', type: 'full' }
];

// CAB approval states
const cabStates = new Map([
  ['LOW', 'AUTO_APPROVED'],
  ['MEDIUM', 'PENDING_REVIEW'],
  ['HIGH', 'REQUIRES_CAB'],
  ['CRITICAL', 'EMERGENCY_CAB']
]);

// Main request handler
export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    const path = url.pathname;

    // Add CORS headers for all responses
    const corsHeaders = {
      'Access-Control-Allow-Origin': 'https://claude.ai',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization, anthropic-version'
    };

    // Handle OPTIONS for CORS preflight
    if (request.method === 'OPTIONS') {
      return new Response(null, { status: 204, headers: corsHeaders });
    }

    // Route handling
    switch (path) {
      case '/':
        // Landing page - enhanced for root domain
        return new Response(getLandingHTML(url.hostname), {
          headers: { 'Content-Type': 'text/html; charset=UTF-8', ...corsHeaders }
        });

      case '/health':
        // Health check endpoint
        return new Response(JSON.stringify({
          status: 'healthy',
          service: 'Guile ChangeFlow MCP Server',
          version: SERVER_VERSION,
          timestamp: new Date().toISOString(),
          environment: env.ENVIRONMENT || 'production',
          capabilities: ['mcp', 'change_management', 'risk_assessment', 'oauth']
        }), {
          headers: { 'Content-Type': 'application/json', ...corsHeaders }
        });

      case '/version':
        // Version endpoint - returns clean semver for automation
        return new Response(JSON.stringify({
          version: SERVER_VERSION,
          semver: SERVER_VERSION,
          major: parseInt(SERVER_VERSION.split('.')[0]),
          minor: parseInt(SERVER_VERSION.split('.')[1]),
          patch: parseInt(SERVER_VERSION.split('.')[2])
        }), {
          headers: { 'Content-Type': 'application/json', ...corsHeaders }
        });

      case '/authorize':
        // OAuth Step 1: Show consent page like Atlassian
        if (!env.GITHUB_CLIENT_ID) {
          return new Response('OAuth not configured - GITHUB_CLIENT_ID missing', { status: 500 });
        }

        // Get OAuth params from Claude.ai
        const responseType = url.searchParams.get('response_type');
        const clientId = url.searchParams.get('client_id');
        const redirectUri = url.searchParams.get('redirect_uri');
        const state = url.searchParams.get('state');
        const codeChallenge = url.searchParams.get('code_challenge');
        const codeChallengeMethod = url.searchParams.get('code_challenge_method');
        const scope = url.searchParams.get('scope');

        // If OAuth params present, show consent page
        if (responseType === 'code' && clientId && redirectUri) {
          // Generate consent page HTML
          const consentHTML = `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Authorize ChangeFlow MCP</title>
<style>
body{font-family:system-ui;background:#0f172a;color:#e2e8f0;margin:0;padding:2rem;max-width:600px;margin:0 auto}
h1{color:#3b82f6;font-size:1.5rem}
.client-name{color:#10b981;font-weight:bold}
.box{background:#1e293b;padding:1.5rem;border-radius:0.5rem;margin:1rem 0}
.redirect-uri{font-family:monospace;color:#94a3b8;font-size:0.9rem;margin:0.5rem 0}
.features{list-style:none;padding:0}
.features li{padding:0.5rem 0;color:#cbd5e1}
.features li:before{content:"✓ ";color:#10b981;font-weight:bold}
.buttons{display:flex;gap:1rem;margin-top:2rem}
button{padding:0.75rem 2rem;border:none;border-radius:0.5rem;font-size:1rem;cursor:pointer;font-weight:500}
.approve{background:#3b82f6;color:white}
.approve:hover{background:#2563eb}
.cancel{background:#475569;color:#e2e8f0}
.cancel:hover{background:#334155}
.logo{font-size:3rem;text-align:center;margin-bottom:1rem}
</style>
</head>
<body>
<div class="logo">🔄</div>
<h1>ChangeFlow MCP Server</h1>
<p><span class="client-name">Claude</span> is requesting access</p>

<div class="box">
<p>This MCP Client is requesting to be authorized on ChangeFlow MCP server. If you approve, you will be redirected to GitHub to complete authentication.</p>

<p><strong>Details</strong></p>
<p>Name: <span class="client-name">Claude</span></p>
<p class="redirect-uri">Redirect URI: ${redirectUri}</p>
</div>

<div class="box">
<p><strong>ChangeFlow will provide access to:</strong></p>
<ul class="features">
<li>ITIL 4 Change Management Tools</li>
<li>Risk Assessment & Freeze Periods</li>
<li>CAB Approval Workflows</li>
<li>Audit Trail & Compliance</li>
</ul>
</div>

<form method="POST" action="/authorize">
<input type="hidden" name="response_type" value="${responseType}">
<input type="hidden" name="client_id" value="${clientId}">
<input type="hidden" name="redirect_uri" value="${redirectUri}">
<input type="hidden" name="state" value="${state}">
<input type="hidden" name="code_challenge" value="${codeChallenge || ''}">
<input type="hidden" name="code_challenge_method" value="${codeChallengeMethod || ''}">
<input type="hidden" name="scope" value="${scope || ''}">
<div class="buttons">
<button type="submit" name="action" value="approve" class="approve">Approve</button>
<button type="submit" name="action" value="cancel" class="cancel">Cancel</button>
</div>
</form>
</body>
</html>`;

          return new Response(consentHTML, {
            headers: { 'Content-Type': 'text/html', ...corsHeaders }
          });
        }

        // Handle form submission (POST)
        if (request.method === 'POST') {
          const formData = await request.formData();
          const action = formData.get('action');
          const redirectUri = formData.get('redirect_uri');
          const state = formData.get('state');

          if (action === 'approve') {
            // For Claude.ai - skip GitHub OAuth entirely
            if (redirectUri && (redirectUri.includes('claude.ai') || redirectUri.includes('claude.com'))) {
              // Generate our own authorization code
              const authCode = crypto.randomUUID();

              // Encode auth data in the code itself (stateless)
              const codeData = {
                client_id: formData.get('client_id'),
                redirect_uri: redirectUri,
                code_challenge: formData.get('code_challenge'),
                code_challenge_method: formData.get('code_challenge_method'),
                scope: formData.get('scope') || 'mcp',
                exp: Date.now() + 600000 // 10 minutes
              };

              const encodedCode = btoa(JSON.stringify(codeData));

              // Redirect directly back to Claude with our code
              const redirectUrl = `${redirectUri}?code=${encodedCode}&state=${state}`;

              // Use 303 See Other for POST->GET redirect pattern
              // This ensures browser properly follows the redirect after form POST
              return new Response(null, {
                status: 303,
                headers: {
                  'Location': redirectUrl,
                  ...corsHeaders
                }
              });
            }

            // For non-Claude requests, continue with GitHub OAuth
            const githubParams = new URLSearchParams({
              client_id: env.GITHUB_CLIENT_ID,
              redirect_uri: `${url.origin}/callback`,
              scope: 'read:user',
              state: btoa(JSON.stringify({
                claudeRedirect: redirectUri,
                claudeState: state,
                codeChallenge: formData.get('code_challenge'),
                codeChallengeMethod: formData.get('code_challenge_method'),
                timestamp: Date.now()
              }))
            });

            return Response.redirect(`${GITHUB_OAUTH_URL}?${githubParams}`, 302);
          } else {
            // User cancelled - use 303 for POST->GET redirect
            return Response.redirect(`${redirectUri}?error=access_denied&state=${state}`, 303);
          }
        }

        // Direct browser access - show simple redirect
        const authParams = new URLSearchParams({
          client_id: env.GITHUB_CLIENT_ID,
          redirect_uri: `${url.origin}/callback`,
          scope: 'read:user',
          state: crypto.randomUUID()
        });

        return Response.redirect(`${GITHUB_OAUTH_URL}?${authParams}`, 302);

      case '/callback':
        // OAuth Step 2: Handle GitHub callback
        const code = url.searchParams.get('code');
        const callbackState = url.searchParams.get('state');

        if (!code) {
          return new Response(ERROR_HTML, {
            status: 400,
            headers: { 'Content-Type': 'text/html' }
          });
        }

        try {
          // Exchange code for token
          const tokenResponse = await fetch(GITHUB_TOKEN_URL, {
            method: 'POST',
            headers: {
              'Accept': 'application/json',
              'Content-Type': 'application/json'
            },
            body: JSON.stringify({
              client_id: env.GITHUB_CLIENT_ID,
              client_secret: env.GITHUB_CLIENT_SECRET,
              code: code
            })
          });

          const tokenData = await tokenResponse.json();

          if (tokenData.access_token) {
            // Success! Set a session cookie so /v1/sse knows user is authenticated
            const sessionId = crypto.randomUUID();

            // Check if we need to redirect back to Claude.ai
            let isClaudeCallback = false;
            let claudeRedirectUri = null;
            let claudeState = null;

            try {
              if (callbackState) {
                const stateData = JSON.parse(atob(callbackState));
                if (stateData.claudeRedirect) {
                  isClaudeCallback = true;
                  claudeRedirectUri = stateData.claudeRedirect;
                  claudeState = stateData.claudeState;
                }
              }
            } catch (e) {
              // Not from Claude, regular OAuth flow
            }

            // Set session cookie
            const cookieHeader = `mcp_session=${sessionId}; Path=/; HttpOnly; Secure; SameSite=None; Max-Age=86400`;

            // If this is Claude.ai callback, redirect with their expected params
            if (isClaudeCallback && claudeRedirectUri) {
              const callbackParams = new URLSearchParams({
                code: sessionId, // Use session ID as authorization code
                state: claudeState || ''
              });

              return new Response(null, {
                status: 302,
                headers: {
                  'Location': `${claudeRedirectUri}?${callbackParams}`,
                  'Set-Cookie': cookieHeader
                }
              });
            }

            // Regular success page for direct browser access
            return new Response(SUCCESS_HTML, {
              headers: {
                'Content-Type': 'text/html',
                'Set-Cookie': cookieHeader
              }
            });
          } else {
            return new Response(ERROR_HTML, {
              status: 400,
              headers: { 'Content-Type': 'text/html' }
            });
          }
        } catch (error) {
          return new Response(ERROR_HTML, {
            status: 500,
            headers: { 'Content-Type': 'text/html' }
          });
        }

      case '/mcp':
        // Legacy JSON-RPC endpoint (backward compatibility)
        if (request.method === 'GET') {
          // MCP info endpoint
          return new Response(JSON.stringify({
            mcp_version: '1.0.0',
            server_name: 'guile-changeflow',
            server_version: SERVER_VERSION,
            description: 'ITIL 4-compliant change management system with automatic risk assessment',
            capabilities: {
              tools: true,
              resources: false,
              prompts: false,
              notifications: true
            },
            endpoints: {
              sse: '/v1/sse',
              legacy: '/mcp'
            }
          }), {
            headers: { 'Content-Type': 'application/json', ...corsHeaders }
          });
        } else if (request.method === 'POST') {
          // Handle MCP protocol messages
          try {
            const body = await request.json();
            const method = body.method;

            if (method === 'initialize') {
              // MCP initialize handshake
              return new Response(JSON.stringify({
                jsonrpc: '2.0',
                id: body.id,
                result: {
                  protocolVersion: '2024-11-05',
                  capabilities: {
                    tools: {
                      listChanged: false
                    },
                    resources: {},
                    prompts: {},
                    logging: {}
                  },
                  serverInfo: {
                    name: 'guile-changeflow',
                    version: SERVER_VERSION
                  }
                }
              }), {
                headers: { 'Content-Type': 'application/json', ...corsHeaders }
              });
            } else if (method === 'tools/list') {
              // Return available tools
              return new Response(JSON.stringify({
                jsonrpc: '2.0',
                id: body.id,
                result: {
                  tools: [
                    {
                      name: 'create_change_request',
                      description: 'Create a new ITIL change request',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          title: { type: 'string', description: 'Change title' },
                          description: { type: 'string', description: 'Detailed description' },
                          environment: { type: 'string', enum: ['development', 'staging', 'production'] },
                          changeType: { type: 'string', enum: ['standard', 'normal', 'emergency'] },
                          implementationDate: { type: 'string', format: 'date' }
                        },
                        required: ['title', 'description', 'environment']
                      }
                    },
                    {
                      name: 'assess_risk',
                      description: 'Assess risk for a change request',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        },
                        required: ['changeId']
                      }
                    },
                    {
                      name: 'check_freeze_period',
                      description: 'Check if date falls within a freeze period',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          date: { type: 'string', format: 'date' }
                        },
                        required: ['date']
                      }
                    },
                    {
                      name: 'get_change_request',
                      description: 'Get details of a change request',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        },
                        required: ['changeId']
                      }
                    },
                    {
                      name: 'list_change_requests',
                      description: 'List all change requests',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          status: { type: 'string', enum: ['pending', 'approved', 'rejected', 'completed'] }
                        }
                      }
                    },
                    {
                      name: 'get_approval_status',
                      description: 'Get CAB approval status',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        },
                        required: ['changeId']
                      }
                    },
                    {
                      name: 'emergency_override',
                      description: 'Request emergency override for critical changes',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' },
                          justification: { type: 'string' }
                        },
                        required: ['changeId', 'justification']
                      }
                    },
                    {
                      name: 'audit_trail',
                      description: 'Get audit trail for a change',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        }
                      }
                    }
                  ]
                }
              }), {
                headers: { 'Content-Type': 'application/json', ...corsHeaders }
              });
            } else if (method === 'tools/call') {
              // Execute a tool
              const toolName = body.params?.name;
              const toolParams = body.params?.arguments || {};

              // Initialize ITIL service with KV storage
              const itilService = new ITILService(env.CHANGES_KV);
              let result;

              try {
                switch (toolName) {
                  case 'create_change_request':
                    const change = await itilService.createChangeRequest(toolParams);
                    result = {
                      content: [{
                        type: 'text',
                        text: `Change request created: ${change.id}\n\n${JSON.stringify(change, null, 2)}`
                      }]
                    };
                    break;

                  case 'assess_risk':
                    const riskAssessment = await itilService.assessRisk(toolParams.changeId);
                    result = {
                      content: [{
                        type: 'text',
                        text: `Risk Assessment:\n${JSON.stringify(riskAssessment, null, 2)}`
                      }]
                    };
                    break;

                  case 'check_freeze_period':
                    const freezeCheck = itilService.checkFreezePeriod(toolParams.date);
                    result = {
                      content: [{
                        type: 'text',
                        text: freezeCheck.inFreeze
                          ? `⚠️ Date is within ${freezeCheck.period} freeze period!\nReason: ${freezeCheck.reason}`
                          : `✅ Date is clear for changes.${freezeCheck.nextFreeze ? `\nNext freeze: ${freezeCheck.nextFreeze.name} in ${freezeCheck.nextFreeze.daysUntil} days` : ''}`
                      }]
                    };
                    break;

                  case 'get_change_request':
                    const changeDetails = await itilService.getChange(toolParams.changeId);
                    result = {
                      content: [{
                        type: 'text',
                        text: changeDetails
                          ? JSON.stringify(changeDetails, null, 2)
                          : 'Change request not found'
                      }]
                    };
                    break;

                  case 'list_change_requests':
                    const activeChanges = await itilService.getActiveChanges();
                    result = {
                      content: [{
                        type: 'text',
                        text: `Active Changes (${activeChanges.length}):\n${JSON.stringify(activeChanges, null, 2)}`
                      }]
                    };
                    break;

                  case 'get_approval_status':
                    const changeForApproval = await itilService.getChange(toolParams.changeId);
                    const approvalStatus = changeForApproval?.cabRequest || { status: 'not_requested' };
                    result = {
                      content: [{
                        type: 'text',
                        text: `CAB Approval Status:\n${JSON.stringify(approvalStatus, null, 2)}`
                      }]
                    };
                    break;

                  case 'emergency_override':
                    const emergencyChange = await itilService.getChange(toolParams.changeId);
                    if (emergencyChange) {
                      emergencyChange.changeType = 'emergency';
                      emergencyChange.emergencyJustification = toolParams.justification;
                      emergencyChange.auditLog.push({
                        timestamp: new Date().toISOString(),
                        user: 'mcp-client',
                        action: 'EMERGENCY_OVERRIDE',
                        details: toolParams.justification
                      });
                      if (env.CHANGES_KV) {
                        await env.CHANGES_KV.put(toolParams.changeId, JSON.stringify(emergencyChange));
                      }
                      result = {
                        content: [{
                          type: 'text',
                          text: `Emergency override applied. Change ${toolParams.changeId} marked as emergency.`
                        }]
                      };
                    } else {
                      result = {
                        content: [{
                          type: 'text',
                          text: 'Change request not found'
                        }]
                      };
                    }
                    break;

                  default:
                    result = { content: [{ type: 'text', text: `Tool ${toolName} not implemented yet` }] };
                }
              } catch (error) {
                result = {
                  content: [{
                    type: 'text',
                    text: `Error executing ${toolName}: ${error.message}`
                  }]
                };
              }

              return new Response(JSON.stringify({
                jsonrpc: '2.0',
                id: body.id,
                result
              }), {
                headers: { 'Content-Type': 'application/json', ...corsHeaders }
              });
            }

            // Default MCP response
            return new Response(JSON.stringify({
              jsonrpc: '2.0',
              id: body.id,
              result: {}
            }), {
              headers: { 'Content-Type': 'application/json', ...corsHeaders }
            });
          } catch (error) {
            return new Response(JSON.stringify({
              jsonrpc: '2.0',
              id: null,
              error: {
                code: -32603,
                message: 'Internal error',
                data: error.message
              }
            }), {
              status: 500,
              headers: { 'Content-Type': 'application/json', ...corsHeaders }
            });
          }
        }
        break;

      case '/v1/sse':
        // SSE endpoint for Claude.ai integration
        const authHeader = request.headers.get('authorization');
        let isAuthenticated = false;

        // Check Bearer token
        if (authHeader?.startsWith('Bearer ')) {
          const token = authHeader.substring(7);

          try {
            // Try to decode as our token format
            const tokenData = JSON.parse(atob(token));

            // Validate expiration
            if (!tokenData.exp || Date.now() < tokenData.exp) {
              isAuthenticated = true;
            }
          } catch (e) {
            // Token might be a simple UUID from fallback, accept it for now
            if (token.match(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i)) {
              isAuthenticated = true;
            }
          }
        }

        // Check session cookie as fallback
        if (!isAuthenticated) {
          const cookies = request.headers.get('cookie') || '';
          isAuthenticated = cookies.includes('mcp_session=');
        }

        // If no auth, return 401 with WWW-Authenticate header
        if (!isAuthenticated) {
          return new Response(JSON.stringify({
            error: 'invalid_token',
            error_description: 'Authentication required'
          }), {
            status: 401,
            headers: {
              'Content-Type': 'application/json',
              'WWW-Authenticate': 'Bearer realm="OAuth", error="invalid_token"',
              ...corsHeaders
            }
          });
        }

        // For authenticated GET requests, return SSE stream
        if (request.method === 'GET') {
          return new Response(`data: {"type":"ready","version":"${SERVER_VERSION}"}\n\n`, {
            headers: {
              'Content-Type': 'text/event-stream',
              'Cache-Control': 'no-cache',
              'Connection': 'keep-alive',
              ...corsHeaders
            }
          });
        }

        // For POST, handle MCP protocol over SSE
        if (request.method === 'POST') {
          try {
            const body = await request.json();
            const method = body.method;

            // Create SSE response
            const encoder = new TextEncoder();
            const stream = new ReadableStream({
              async start(controller) {
                // Send initial connection
                controller.enqueue(encoder.encode('data: {"type":"connected"}\n\n'));

                if (method === 'initialize') {
                  // MCP initialize handshake
                  const initResponse = {
                    jsonrpc: '2.0',
                    id: body.id,
                    result: {
                      protocolVersion: '2024-11-05',
                      capabilities: {
                        tools: {
                          listChanged: false
                        },
                        resources: {},
                        prompts: {},
                        logging: {}
                      },
                      serverInfo: {
                        name: 'guile-changeflow',
                        version: SERVER_VERSION
                      }
                    }
                  };
                  controller.enqueue(encoder.encode(`data: ${JSON.stringify(initResponse)}\n\n`));
                } else if (method === 'tools/list') {
                  const tools = [
                    {
                      name: 'create_change_request',
                      description: 'Create a new ITIL change request',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          title: { type: 'string', description: 'Change title' },
                          description: { type: 'string', description: 'Detailed description' },
                          environment: { type: 'string', enum: ['development', 'staging', 'production'] },
                          changeType: { type: 'string', enum: ['standard', 'normal', 'emergency'] },
                          implementationDate: { type: 'string', format: 'date' }
                        },
                        required: ['title', 'description', 'environment']
                      }
                    },
                    {
                      name: 'assess_risk',
                      description: 'Assess risk for a change request',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        },
                        required: ['changeId']
                      }
                    },
                    {
                      name: 'check_freeze_period',
                      description: 'Check if date falls within a freeze period',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          date: { type: 'string', format: 'date' }
                        },
                        required: ['date']
                      }
                    },
                    {
                      name: 'get_change_request',
                      description: 'Get details of a change request',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        },
                        required: ['changeId']
                      }
                    },
                    {
                      name: 'list_change_requests',
                      description: 'List all change requests',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          status: { type: 'string', enum: ['pending', 'approved', 'rejected', 'completed'] }
                        }
                      }
                    },
                    {
                      name: 'get_approval_status',
                      description: 'Get CAB approval status',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        },
                        required: ['changeId']
                      }
                    },
                    {
                      name: 'emergency_override',
                      description: 'Request emergency override for critical changes',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' },
                          justification: { type: 'string' }
                        },
                        required: ['changeId', 'justification']
                      }
                    },
                    {
                      name: 'audit_trail',
                      description: 'Get audit trail for a change',
                      inputSchema: {
                        type: 'object',
                        properties: {
                          changeId: { type: 'string' }
                        }
                      }
                    }
                  ];

                  const response = {
                    jsonrpc: '2.0',
                    id: body.id,
                    result: { tools }
                  };
                  controller.enqueue(encoder.encode(`data: ${JSON.stringify(response)}\n\n`));
                } else if (method === 'tools/call') {
                  // Execute tool
                  const toolName = body.params?.name;
                  const toolParams = body.params?.arguments || {};

                  // Initialize ITIL service with KV storage
                  const itilService = new ITILService(env.CHANGES_KV);
                  let result;

                  try {
                    switch (toolName) {
                      case 'create_change_request':
                        const change = await itilService.createChangeRequest(toolParams);
                        result = {
                          content: [{
                            type: 'text',
                            text: `Change request created: ${change.id}\n\n${JSON.stringify(change, null, 2)}`
                          }]
                        };
                        break;

                      case 'assess_risk':
                        const riskAssessment = await itilService.assessRisk(toolParams.changeId);
                        result = {
                          content: [{
                            type: 'text',
                            text: `Risk Assessment:\n${JSON.stringify(riskAssessment, null, 2)}`
                          }]
                        };
                        break;

                      case 'check_freeze_period':
                        const freezeCheck = itilService.checkFreezePeriod(toolParams.date);
                        result = {
                          content: [{
                            type: 'text',
                            text: freezeCheck.inFreeze
                              ? `⚠️ Date is within ${freezeCheck.period} freeze period!\nReason: ${freezeCheck.reason}`
                              : `✅ Date is clear for changes.${freezeCheck.nextFreeze ? `\nNext freeze: ${freezeCheck.nextFreeze.name} in ${freezeCheck.nextFreeze.daysUntil} days` : ''}`
                          }]
                        };
                        break;

                      case 'get_change_request':
                        const changeDetails = await itilService.getChange(toolParams.changeId);
                        result = {
                          content: [{
                            type: 'text',
                            text: changeDetails
                              ? JSON.stringify(changeDetails, null, 2)
                              : 'Change request not found'
                          }]
                        };
                        break;

                      case 'list_change_requests':
                        const activeChanges = await itilService.getActiveChanges();
                        result = {
                          content: [{
                            type: 'text',
                            text: `Active Changes (${activeChanges.length}):\n${JSON.stringify(activeChanges, null, 2)}`
                          }]
                        };
                        break;

                      case 'get_approval_status':
                        const changeForApproval = await itilService.getChange(toolParams.changeId);
                        const approvalStatus = changeForApproval?.cabRequest || { status: 'not_requested' };
                        result = {
                          content: [{
                            type: 'text',
                            text: `CAB Approval Status:\n${JSON.stringify(approvalStatus, null, 2)}`
                          }]
                        };
                        break;

                      case 'emergency_override':
                        const emergencyChange = await itilService.getChange(toolParams.changeId);
                        if (emergencyChange) {
                          emergencyChange.changeType = 'emergency';
                          emergencyChange.emergencyJustification = toolParams.justification;
                          emergencyChange.auditLog.push({
                            timestamp: new Date().toISOString(),
                            user: 'mcp-client',
                            action: 'EMERGENCY_OVERRIDE',
                            details: toolParams.justification
                          });
                          if (env.CHANGES_KV) {
                            await env.CHANGES_KV.put(toolParams.changeId, JSON.stringify(emergencyChange));
                          }
                          result = {
                            content: [{
                              type: 'text',
                              text: `Emergency override applied. Change ${toolParams.changeId} marked as emergency.`
                            }]
                          };
                        } else {
                          result = {
                            content: [{
                              type: 'text',
                              text: 'Change request not found'
                            }]
                          };
                        }
                        break;

                      default:
                        result = { content: [{ type: 'text', text: `Tool ${toolName} not implemented yet` }] };
                    }
                  } catch (error) {
                    result = {
                      content: [{
                        type: 'text',
                        text: `Error executing ${toolName}: ${error.message}`
                      }]
                    };
                  }

                  const response = {
                    jsonrpc: '2.0',
                    id: body.id,
                    result
                  };
                  controller.enqueue(encoder.encode(`data: ${JSON.stringify(response)}\n\n`));
                }

                // Send completion
                controller.enqueue(encoder.encode('data: {"type":"done"}\n\n'));
                controller.close();
              }
            });

            return new Response(stream, {
              headers: {
                'Content-Type': 'text/event-stream',
                'Cache-Control': 'no-cache',
                'Connection': 'keep-alive',
                ...corsHeaders
              }
            });
          } catch (error) {
            return new Response(`data: ${JSON.stringify({
              jsonrpc: '2.0',
              id: null,
              error: {
                code: -32603,
                message: 'Internal error',
                data: error.message
              }
            })}\n\n`, {
              status: 500,
              headers: {
                'Content-Type': 'text/event-stream',
                ...corsHeaders
              }
            });
          }
        }
        break;

      case '/.well-known/oauth-authorization-server':
        // OAuth discovery endpoint for Claude.ai
        return new Response(JSON.stringify({
          issuer: url.origin,
          authorization_endpoint: `${url.origin}/authorize`,
          token_endpoint: `${url.origin}/token`,
          registration_endpoint: `${url.origin}/register`,
          response_types_supported: ['code'],
          response_modes_supported: ['query'],
          grant_types_supported: ['authorization_code', 'refresh_token'],
          token_endpoint_auth_methods_supported: ['client_secret_basic', 'client_secret_post', 'none'],
          revocation_endpoint: `${url.origin}/token`,
          code_challenge_methods_supported: ['plain', 'S256'],
          scopes_supported: ['mcp', 'claudeai'],
          service_documentation: 'https://github.com/dsp-dr/guile-changeflow'
        }), {
          headers: {
            'Content-Type': 'application/json',
            ...corsHeaders
          }
        });

      case '/.well-known/oauth-protected-resource':
        // OAuth protected resource metadata (for MCP service discovery)
        return new Response(JSON.stringify({
          resource: url.origin,
          scopes_supported: ['mcp', 'claudeai'],
          authorization_servers: [url.origin],
          bearer_methods_supported: ['header'],
          resource_documentation: 'https://github.com/dsp-dr/guile-changeflow',
          resource_signing_alg_values_supported: ['none'],
          interfaces: {
            mcp: {
              endpoint: `${url.origin}/v1/sse`,
              protocol: 'sse',
              version: '2024-11-05'
            }
          }
        }), {
          headers: {
            'Content-Type': 'application/json',
            ...corsHeaders
          }
        });

      case '/oauth/authorize':
        // Alias for /authorize to match Cloudflare's pattern
        // Redirect to /authorize with same query params
        return new Response(null, {
          status: 302,
          headers: {
            'Location': '/authorize' + url.search,
            ...corsHeaders
          }
        });

      case '/register':
        // OAuth client registration endpoint
        // For now, auto-approve all registrations
        if (request.method !== 'POST') {
          return new Response('Method not allowed', { status: 405 });
        }

        const registrationClientId = crypto.randomUUID();
        const registrationClientSecret = crypto.randomUUID();

        return new Response(JSON.stringify({
          client_id: registrationClientId,
          client_secret: registrationClientSecret,
          client_id_issued_at: Math.floor(Date.now() / 1000),
          grant_types: ['authorization_code', 'refresh_token'],
          response_types: ['code'],
          token_endpoint_auth_method: 'client_secret_basic'
        }), {
          status: 201,
          headers: {
            'Content-Type': 'application/json',
            ...corsHeaders
          }
        });

      case '/token':
        // OAuth token endpoint
        if (request.method !== 'POST') {
          return new Response('Method not allowed', { status: 405 });
        }

        try {
          // Parse form data (OAuth uses application/x-www-form-urlencoded)
          const contentType = request.headers.get('content-type') || '';
          let body;

          if (contentType.includes('application/x-www-form-urlencoded')) {
            const formData = await request.formData();
            body = Object.fromEntries(formData.entries());
          } else {
            // Fallback to JSON for backward compatibility
            body = await request.json();
          }

          const grantType = body.grant_type;
          const code = body.code;
          const codeVerifier = body.code_verifier;

          // Handle authorization_code grant
          if (grantType === 'authorization_code' && code) {
            // Try to decode as our Claude auth code first
            try {
              const codeData = JSON.parse(atob(code));

              // Validate expiration
              if (codeData.exp && Date.now() > codeData.exp) {
                throw new Error('Code expired');
              }

              // Validate PKCE if present
              if (codeData.code_challenge_method === 'S256' && codeVerifier) {
                const encoder = new TextEncoder();
                const data = encoder.encode(codeVerifier);
                const hashBuffer = await crypto.subtle.digest('SHA-256', data);
                const hashArray = new Uint8Array(hashBuffer);

                // Base64URL encode
                const computed = btoa(String.fromCharCode(...hashArray))
                  .replace(/=/g, '')
                  .replace(/\+/g, '-')
                  .replace(/\//g, '_');

                if (computed !== codeData.code_challenge) {
                  return new Response(JSON.stringify({
                    error: 'invalid_grant',
                    error_description: 'PKCE validation failed'
                  }), {
                    status: 400,
                    headers: { 'Content-Type': 'application/json', ...corsHeaders }
                  });
                }
              }

              // Generate access token with embedded data
              const tokenData = {
                client_id: codeData.client_id,
                scope: codeData.scope || 'mcp',
                exp: Date.now() + 3600000 // 1 hour
              };

              const accessToken = btoa(JSON.stringify(tokenData));

              return new Response(JSON.stringify({
                access_token: accessToken,
                token_type: 'Bearer',
                expires_in: 3600,
                scope: tokenData.scope
              }), {
                headers: {
                  'Content-Type': 'application/json',
                  ...corsHeaders
                }
              });
            } catch (decodeError) {
              // Not our encoded code - reject it
              return new Response(JSON.stringify({
                error: 'invalid_grant',
                error_description: 'Invalid or expired authorization code'
              }), {
                status: 400,
                headers: {
                  'Content-Type': 'application/json',
                  ...corsHeaders
                }
              });
            }
          }

          return new Response(JSON.stringify({
            error: 'invalid_grant',
            error_description: 'Invalid grant type or code'
          }), {
            status: 400,
            headers: {
              'Content-Type': 'application/json',
              ...corsHeaders
            }
          });
        } catch (error) {
          return new Response(JSON.stringify({
            error: 'invalid_request',
            error_description: 'Invalid request format'
          }), {
            status: 400,
            headers: {
              'Content-Type': 'application/json',
              ...corsHeaders
            }
          });
        }

      case '/favicon.ico':
        // Return inline favicon (ChangeFlow logo from generated image)
        // This is a tiny 16x16 16-color favicon (318 bytes) embedded as base64
        const favicon = 'AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAgAAAAAAAAAAAAAAAEAAAABAAAACZMyIAqlUiALtmVQC7iGYA7u4iAN3MVQDMqiIA7t2qAO7MzAD//90A////AP/u7gD///8AzJmZAAAAAAAAAAAAqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqsi2qqqqqqqogIDqqqqqqqxrIF6qqqqqqO4tWeaqqqqq4i7VEqqqqqqqqqnmqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA';

        // Convert base64 to bytes
        const binaryString = atob(favicon);
        const bytes = new Uint8Array(binaryString.length);
        for (let i = 0; i < binaryString.length; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }

        return new Response(bytes, {
          headers: {
            'Content-Type': 'image/x-icon',
            'Cache-Control': 'public, max-age=86400',
            ...corsHeaders
          }
        });

      default:
        return new Response('Not Found', { status: 404 });
    }
  }
};

// Helper function to calculate risk
function calculateRisk(params) {
  let score = RISK_FACTORS.baseScore;

  if (params.environment === 'production') score += RISK_FACTORS.production;
  if (params.description?.includes('security')) score += RISK_FACTORS.security;
  if (params.description?.includes('payment')) score += RISK_FACTORS.payment;
  if (params.changeType === 'emergency') score += RISK_FACTORS.emergency;

  if (score <= 25) return 'LOW';
  if (score <= 50) return 'MEDIUM';
  if (score <= 75) return 'HIGH';
  return 'CRITICAL';
}