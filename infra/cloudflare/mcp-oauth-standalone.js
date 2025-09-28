/**
 * MCP OAuth Standalone Implementation for Claude.ai
 * Supports 3/26 and 6/18 auth specs with Dynamic Client Registration
 */

const SERVER_VERSION = '1.5.0';

// In-memory stores (replace with KV in production)
const registeredClients = new Map();
const authorizationCodes = new Map();
const accessTokens = new Map();
const refreshTokens = new Map();

/**
 * Dynamic Client Registration (DCR) - RFC 7591
 */
async function handleClientRegistration(request) {
  const body = await request.json();

  // Validate required fields
  if (!body.client_name || !body.redirect_uris || !Array.isArray(body.redirect_uris)) {
    return new Response(JSON.stringify({
      error: 'invalid_client_metadata',
      error_description: 'Missing required fields'
    }), {
      status: 400,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  // Validate Claude's callback URLs (both .ai and .com for future-proofing)
  const validCallbacks = [
    'https://claude.ai/api/mcp/auth_callback',
    'https://claude.com/api/mcp/auth_callback'
  ];

  const isValidCallback = body.redirect_uris.every(uri =>
    validCallbacks.includes(uri)
  );

  if (!isValidCallback) {
    return new Response(JSON.stringify({
      error: 'invalid_redirect_uri',
      error_description: 'Invalid redirect URI'
    }), {
      status: 400,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  // Generate client credentials
  const clientId = crypto.randomUUID();
  const clientSecret = crypto.randomUUID(); // Optional for public clients

  const client = {
    client_id: clientId,
    client_secret: clientSecret,
    client_name: body.client_name,
    redirect_uris: body.redirect_uris,
    grant_types: body.grant_types || ['authorization_code', 'refresh_token'],
    response_types: body.response_types || ['code'],
    token_endpoint_auth_method: body.token_endpoint_auth_method || 'none',
    created_at: Date.now()
  };

  registeredClients.set(clientId, client);

  // Return client information (without secret for public clients)
  const response = {
    client_id: clientId,
    client_name: client.client_name,
    redirect_uris: client.redirect_uris,
    grant_types: client.grant_types,
    response_types: client.response_types,
    token_endpoint_auth_method: client.token_endpoint_auth_method
  };

  // Include secret only for confidential clients
  if (client.token_endpoint_auth_method !== 'none') {
    response.client_secret = clientSecret;
  }

  return new Response(JSON.stringify(response), {
    status: 201,
    headers: { 'Content-Type': 'application/json' }
  });
}

/**
 * OAuth Authorization Endpoint
 */
async function handleAuthorization(request, url) {
  const clientId = url.searchParams.get('client_id');
  const redirectUri = url.searchParams.get('redirect_uri');
  const responseType = url.searchParams.get('response_type');
  const state = url.searchParams.get('state');
  const codeChallenge = url.searchParams.get('code_challenge');
  const codeChallengeMethod = url.searchParams.get('code_challenge_method');
  const scope = url.searchParams.get('scope') || 'mcp';

  // For GET requests, show consent page
  if (request.method === 'GET') {
    // Check if client is registered (or allow unregistered for Claude)
    const client = registeredClients.get(clientId) || {
      client_name: 'Claude',
      redirect_uris: [
        'https://claude.ai/api/mcp/auth_callback',
        'https://claude.com/api/mcp/auth_callback'
      ]
    };

    // Validate redirect URI
    if (!client.redirect_uris.includes(redirectUri)) {
      return new Response('Invalid redirect URI', { status: 400 });
    }

    // Generate consent page
    const consentHTML = `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Authorize MCP Access</title>
<style>
body { font-family: system-ui; background: #0f172a; color: #e2e8f0; padding: 2rem; max-width: 500px; margin: 0 auto; }
.container { background: #1e293b; padding: 2rem; border-radius: 1rem; }
h1 { color: #3b82f6; font-size: 1.5rem; margin-bottom: 1rem; }
.client { color: #10b981; font-weight: bold; }
.permissions { background: #0f172a; padding: 1rem; border-radius: 0.5rem; margin: 1rem 0; }
.permissions li { margin: 0.5rem 0; }
.buttons { display: flex; gap: 1rem; margin-top: 2rem; }
button { padding: 0.75rem 2rem; border: none; border-radius: 0.5rem; font-size: 1rem; cursor: pointer; }
.approve { background: #3b82f6; color: white; }
.approve:hover { background: #2563eb; }
.deny { background: #475569; color: #e2e8f0; }
.deny:hover { background: #334155; }
</style>
</head>
<body>
<div class="container">
  <h1>🔄 ChangeFlow MCP Server</h1>
  <p><span class="client">${client.client_name}</span> is requesting access to:</p>

  <div class="permissions">
    <ul>
      <li>✓ ITIL Change Management Tools</li>
      <li>✓ Risk Assessment Functions</li>
      <li>✓ Freeze Period Checks</li>
      <li>✓ CAB Approval Workflows</li>
      <li>✓ Audit Trail Access</li>
    </ul>
  </div>

  <form method="POST" action="/authorize">
    <input type="hidden" name="client_id" value="${clientId}">
    <input type="hidden" name="redirect_uri" value="${redirectUri}">
    <input type="hidden" name="response_type" value="${responseType}">
    <input type="hidden" name="state" value="${state}">
    <input type="hidden" name="code_challenge" value="${codeChallenge || ''}">
    <input type="hidden" name="code_challenge_method" value="${codeChallengeMethod || ''}">
    <input type="hidden" name="scope" value="${scope}">

    <div class="buttons">
      <button type="submit" name="action" value="approve" class="approve">Authorize</button>
      <button type="submit" name="action" value="deny" class="deny">Cancel</button>
    </div>
  </form>
</div>
</body>
</html>`;

    return new Response(consentHTML, {
      headers: { 'Content-Type': 'text/html' }
    });
  }

  // Handle POST (consent response)
  if (request.method === 'POST') {
    const formData = await request.formData();
    const action = formData.get('action');
    const redirectUri = formData.get('redirect_uri');
    const state = formData.get('state');

    if (action === 'deny') {
      // User denied access
      return Response.redirect(
        `${redirectUri}?error=access_denied&state=${state}`,
        302
      );
    }

    // User approved - generate authorization code
    const code = crypto.randomUUID();

    // Store code with PKCE challenge and metadata
    authorizationCodes.set(code, {
      client_id: formData.get('client_id'),
      redirect_uri: redirectUri,
      code_challenge: formData.get('code_challenge'),
      code_challenge_method: formData.get('code_challenge_method'),
      scope: formData.get('scope'),
      expires_at: Date.now() + 600000, // 10 minutes
      created_at: Date.now()
    });

    // Redirect back to Claude with authorization code
    return Response.redirect(
      `${redirectUri}?code=${code}&state=${state}`,
      302
    );
  }

  return new Response('Method not allowed', { status: 405 });
}

/**
 * OAuth Token Endpoint
 */
async function handleToken(request) {
  if (request.method !== 'POST') {
    return new Response('Method not allowed', { status: 405 });
  }

  const contentType = request.headers.get('content-type');
  let params;

  if (contentType?.includes('application/json')) {
    params = await request.json();
  } else if (contentType?.includes('application/x-www-form-urlencoded')) {
    const formData = await request.formData();
    params = Object.fromEntries(formData);
  } else {
    return new Response(JSON.stringify({
      error: 'invalid_request',
      error_description: 'Unsupported content type'
    }), {
      status: 400,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  const grantType = params.grant_type;

  // Handle authorization_code grant
  if (grantType === 'authorization_code') {
    const code = params.code;
    const codeVerifier = params.code_verifier;
    const clientId = params.client_id;

    // Retrieve and validate authorization code
    const authCode = authorizationCodes.get(code);

    if (!authCode) {
      return new Response(JSON.stringify({
        error: 'invalid_grant',
        error_description: 'Invalid authorization code'
      }), {
        status: 400,
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Check expiration
    if (Date.now() > authCode.expires_at) {
      authorizationCodes.delete(code);
      return new Response(JSON.stringify({
        error: 'invalid_grant',
        error_description: 'Authorization code expired'
      }), {
        status: 400,
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Validate client
    if (authCode.client_id !== clientId) {
      return new Response(JSON.stringify({
        error: 'invalid_client',
        error_description: 'Client mismatch'
      }), {
        status: 401,
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Validate PKCE (required for 3/26 and 6/18 specs)
    if (authCode.code_challenge) {
      if (!codeVerifier) {
        return new Response(JSON.stringify({
          error: 'invalid_grant',
          error_description: 'PKCE code verifier required'
        }), {
          status: 400,
          headers: { 'Content-Type': 'application/json' }
        });
      }

      // Compute challenge from verifier
      let computedChallenge;
      if (authCode.code_challenge_method === 'S256') {
        const encoder = new TextEncoder();
        const data = encoder.encode(codeVerifier);
        const hashBuffer = await crypto.subtle.digest('SHA-256', data);
        const hashArray = new Uint8Array(hashBuffer);

        // Base64URL encode
        computedChallenge = btoa(String.fromCharCode(...hashArray))
          .replace(/=/g, '')
          .replace(/\+/g, '-')
          .replace(/\//g, '_');
      } else {
        // Plain method
        computedChallenge = codeVerifier;
      }

      if (computedChallenge !== authCode.code_challenge) {
        return new Response(JSON.stringify({
          error: 'invalid_grant',
          error_description: 'PKCE validation failed'
        }), {
          status: 400,
          headers: { 'Content-Type': 'application/json' }
        });
      }
    }

    // Delete used authorization code
    authorizationCodes.delete(code);

    // Generate tokens
    const accessToken = crypto.randomUUID();
    const refreshToken = crypto.randomUUID();

    // Store tokens
    accessTokens.set(accessToken, {
      client_id: clientId,
      scope: authCode.scope,
      expires_at: Date.now() + 3600000, // 1 hour
      created_at: Date.now()
    });

    refreshTokens.set(refreshToken, {
      client_id: clientId,
      scope: authCode.scope,
      created_at: Date.now()
    });

    return new Response(JSON.stringify({
      access_token: accessToken,
      token_type: 'Bearer',
      expires_in: 3600,
      refresh_token: refreshToken,
      scope: authCode.scope
    }), {
      headers: { 'Content-Type': 'application/json' }
    });
  }

  // Handle refresh_token grant
  if (grantType === 'refresh_token') {
    const refreshToken = params.refresh_token;
    const clientId = params.client_id;

    const tokenData = refreshTokens.get(refreshToken);

    if (!tokenData || tokenData.client_id !== clientId) {
      // Signal invalid client for DCR re-registration
      return new Response(JSON.stringify({
        error: 'invalid_client',
        error_description: 'Client authentication failed'
      }), {
        status: 401,
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Generate new access token
    const newAccessToken = crypto.randomUUID();

    accessTokens.set(newAccessToken, {
      client_id: clientId,
      scope: tokenData.scope,
      expires_at: Date.now() + 3600000,
      created_at: Date.now()
    });

    return new Response(JSON.stringify({
      access_token: newAccessToken,
      token_type: 'Bearer',
      expires_in: 3600,
      scope: tokenData.scope
    }), {
      headers: { 'Content-Type': 'application/json' }
    });
  }

  return new Response(JSON.stringify({
    error: 'unsupported_grant_type',
    error_description: 'Grant type not supported'
  }), {
    status: 400,
    headers: { 'Content-Type': 'application/json' }
  });
}

/**
 * Validate Bearer Token
 */
function validateBearerToken(request) {
  const authHeader = request.headers.get('authorization');

  if (!authHeader?.startsWith('Bearer ')) {
    return null;
  }

  const token = authHeader.substring(7);
  const tokenData = accessTokens.get(token);

  if (!tokenData) {
    return null;
  }

  // Check expiration
  if (Date.now() > tokenData.expires_at) {
    accessTokens.delete(token);
    return null;
  }

  return tokenData;
}

/**
 * OAuth Discovery Endpoints
 */
function getDiscoveryDocument(origin) {
  return {
    issuer: origin,
    authorization_endpoint: `${origin}/authorize`,
    token_endpoint: `${origin}/token`,
    registration_endpoint: `${origin}/register`,
    response_types_supported: ['code'],
    response_modes_supported: ['query'],
    grant_types_supported: ['authorization_code', 'refresh_token'],
    code_challenge_methods_supported: ['plain', 'S256'],
    token_endpoint_auth_methods_supported: ['none', 'client_secret_post', 'client_secret_basic'],
    service_documentation: 'https://github.com/dsp-dr/guile-changeflow',
    ui_locales_supported: ['en']
  };
}

/**
 * Main request handler
 */
export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    const path = url.pathname;

    // CORS headers
    const corsHeaders = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization'
    };

    // Handle OPTIONS
    if (request.method === 'OPTIONS') {
      return new Response(null, { status: 204, headers: corsHeaders });
    }

    try {
      switch (path) {
        // OAuth Discovery
        case '/.well-known/oauth-authorization-server':
          return new Response(JSON.stringify(getDiscoveryDocument(url.origin)), {
            headers: {
              'Content-Type': 'application/json',
              ...corsHeaders
            }
          });

        // Dynamic Client Registration
        case '/register':
          return await handleClientRegistration(request);

        // OAuth Authorization
        case '/authorize':
          return await handleAuthorization(request, url);

        // OAuth Token
        case '/token':
          return await handleToken(request);

        // MCP Protocol Endpoint
        case '/v1/sse':
          const tokenData = validateBearerToken(request);

          if (!tokenData) {
            return new Response(JSON.stringify({
              error: 'invalid_token',
              error_description: 'Authentication required'
            }), {
              status: 401,
              headers: {
                'WWW-Authenticate': 'Bearer realm="MCP", error="invalid_token"',
                'Content-Type': 'application/json',
                ...corsHeaders
              }
            });
          }

          // Return MCP SSE stream for authenticated requests
          return new Response(`data: {"type":"ready","version":"${SERVER_VERSION}"}\n\n`, {
            headers: {
              'Content-Type': 'text/event-stream',
              'Cache-Control': 'no-cache',
              'Connection': 'keep-alive',
              ...corsHeaders
            }
          });

        default:
          return new Response('Not found', { status: 404 });
      }
    } catch (error) {
      console.error('Error:', error);
      return new Response(JSON.stringify({
        error: 'server_error',
        error_description: error.message
      }), {
        status: 500,
        headers: { 'Content-Type': 'application/json' }
      });
    }
  }
};