/**
 * OAuth Wrapper for Remote MCP Server
 * Based on: https://github.com/cloudflare/ai/tree/main/demos/remote-mcp-github-oauth
 *
 * This handles the OAuth flow while the MCP server can be a simple hello world
 */

export class OAuthProvider {
  constructor(config) {
    this.apiRoute = config.apiRoute || '/sse';
    this.apiHandler = config.apiHandler;
    this.authorizeEndpoint = config.authorizeEndpoint || '/authorize';
    this.tokenEndpoint = config.tokenEndpoint || '/token';
    this.callbackEndpoint = config.callbackEndpoint || '/callback';
  }

  async fetch(request, env, ctx) {
    const url = new URL(request.url);

    // Handle different routes
    switch (url.pathname) {
      case '/':
        return this.handleHome(request, env);

      case this.authorizeEndpoint:
        return this.handleAuthorize(request, env);

      case this.callbackEndpoint:
        return this.handleCallback(request, env);

      case this.tokenEndpoint:
        return this.handleToken(request, env);

      case this.apiRoute:
        return this.handleMCPConnection(request, env);

      default:
        return new Response('Not Found', { status: 404 });
    }
  }

  async handleHome(request, env) {
    return new Response(`
      <html>
        <head><title>MCP OAuth Server</title></head>
        <body>
          <h1>Remote MCP Server with OAuth</h1>
          <p>Endpoints:</p>
          <ul>
            <li>${this.authorizeEndpoint} - OAuth authorization</li>
            <li>${this.callbackEndpoint} - OAuth callback</li>
            <li>${this.tokenEndpoint} - Token exchange</li>
            <li>${this.apiRoute} - MCP SSE connection</li>
          </ul>
        </body>
      </html>
    `, {
      headers: { 'Content-Type': 'text/html' }
    });
  }

  async handleAuthorize(request, env) {
    const url = new URL(request.url);
    const clientId = url.searchParams.get('client_id');
    const redirectUri = url.searchParams.get('redirect_uri');
    const state = url.searchParams.get('state');

    // For now, skip actual GitHub OAuth and auto-approve
    // In production, redirect to GitHub OAuth
    if (env.GITHUB_CLIENT_ID && env.GITHUB_CLIENT_SECRET) {
      const githubAuthUrl = new URL('https://github.com/login/oauth/authorize');
      githubAuthUrl.searchParams.set('client_id', env.GITHUB_CLIENT_ID);
      githubAuthUrl.searchParams.set('redirect_uri', `${url.origin}/callback`);
      githubAuthUrl.searchParams.set('state', state || '');
      githubAuthUrl.searchParams.set('scope', 'read:user');

      return Response.redirect(githubAuthUrl.toString(), 302);
    }

    // Mock authorization for testing
    const code = 'mock_auth_code_' + Date.now();
    const callbackUrl = new URL(redirectUri);
    callbackUrl.searchParams.set('code', code);
    callbackUrl.searchParams.set('state', state || '');

    return Response.redirect(callbackUrl.toString(), 302);
  }

  async handleCallback(request, env) {
    const url = new URL(request.url);
    const code = url.searchParams.get('code');
    const state = url.searchParams.get('state');

    if (!code) {
      return new Response('Missing authorization code', { status: 400 });
    }

    // Exchange code for token with GitHub
    if (env.GITHUB_CLIENT_ID && env.GITHUB_CLIENT_SECRET) {
      const tokenResponse = await fetch('https://github.com/login/oauth/access_token', {
        method: 'POST',
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          client_id: env.GITHUB_CLIENT_ID,
          client_secret: env.GITHUB_CLIENT_SECRET,
          code: code,
        }),
      });

      const tokenData = await tokenResponse.json();

      if (tokenData.access_token) {
        // Store token in KV or return to client
        // For now, return success page
        return new Response(`
          <html>
            <body>
              <h1>Authorization Successful!</h1>
              <p>You can now use the MCP server.</p>
              <script>
                // Pass token to parent window if in iframe/popup
                if (window.opener) {
                  window.opener.postMessage({
                    type: 'oauth_success',
                    token: '${tokenData.access_token}'
                  }, '*');
                  window.close();
                }
              </script>
            </body>
          </html>
        `, {
          headers: { 'Content-Type': 'text/html' }
        });
      }
    }

    // Mock success for testing
    return new Response('Authorization successful!', { status: 200 });
  }

  async handleToken(request, env) {
    if (request.method !== 'POST') {
      return new Response('Method not allowed', { status: 405 });
    }

    const body = await request.json();
    const { code, client_id, client_secret, grant_type } = body;

    if (grant_type !== 'authorization_code') {
      return new Response(JSON.stringify({
        error: 'unsupported_grant_type'
      }), {
        status: 400,
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Mock token response
    const token = 'mock_access_token_' + Date.now();

    return new Response(JSON.stringify({
      access_token: token,
      token_type: 'Bearer',
      scope: 'read:user',
    }), {
      headers: { 'Content-Type': 'application/json' }
    });
  }

  async handleMCPConnection(request, env) {
    // Check for authorization
    const authHeader = request.headers.get('Authorization');

    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      // Redirect to authorization
      const url = new URL(request.url);
      const authorizeUrl = new URL(this.authorizeEndpoint, url.origin);
      authorizeUrl.searchParams.set('client_id', 'mcp_client');
      authorizeUrl.searchParams.set('redirect_uri', url.toString());

      return Response.redirect(authorizeUrl.toString(), 302);
    }

    // Token is valid, pass to MCP handler
    if (this.apiHandler) {
      return this.apiHandler(request, env, ctx);
    }

    // Fallback: simple SSE response
    return new Response('data: {"type":"hello","content":"MCP Server Ready"}\\n\\n', {
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive',
      }
    });
  }
}

export default OAuthProvider;