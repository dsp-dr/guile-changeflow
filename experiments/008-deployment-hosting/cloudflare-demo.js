// Cloudflare Workers implementation for ChangeFlow MCP Server
// Demonstrates global edge deployment with Durable Objects

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);

    // Add security headers to all responses
    const addSecurityHeaders = (response) => {
      response.headers.set('X-Content-Type-Options', 'nosniff');
      response.headers.set('X-Frame-Options', 'DENY');
      response.headers.set('Strict-Transport-Security', 'max-age=31536000');
      response.headers.set('Access-Control-Allow-Origin', 'https://claude.ai');
      response.headers.set('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
      response.headers.set('Access-Control-Allow-Headers', 'Authorization, Content-Type');
      return response;
    };

    // Handle CORS preflight requests
    if (request.method === 'OPTIONS') {
      return addSecurityHeaders(new Response(null, { status: 204 }));
    }

    // Rate limiting
    const rateLimiter = new RateLimiter(env);
    const clientIP = request.headers.get('CF-Connecting-IP') || 'unknown';
    const rateLimit = await rateLimiter.checkLimit(clientIP);

    if (!rateLimit.allowed) {
      return addSecurityHeaders(new Response('Rate limit exceeded', {
        status: 429,
        headers: { 'Retry-After': rateLimit.retryAfter.toString() }
      }));
    }

    // Route requests
    try {
      let response;

      switch (url.pathname) {
        case '/':
          response = handleRoot();
          break;
        case '/health':
          response = await handleHealthCheck(env);
          break;
        case '/mcp/initialize':
          response = await handleMCPInitialize(request, env);
          break;
        case '/mcp/sse':
          response = await handleServerSentEvents(request, env);
          break;
        case '/oauth/authorize':
          response = await handleOAuthAuthorize(request, env);
          break;
        case '/oauth/callback':
          response = await handleOAuthCallback(request, env);
          break;
        case '/api/changes':
          response = await handleChangesAPI(request, env);
          break;
        default:
          response = new Response('Not Found', { status: 404 });
      }

      return addSecurityHeaders(response);

    } catch (error) {
      console.error('Request failed:', error);
      return addSecurityHeaders(new Response('Internal Server Error', { status: 500 }));
    }
  }
};

// Rate limiting implementation
class RateLimiter {
  constructor(env) {
    this.kv = env.RATE_LIMIT_KV;
  }

  async checkLimit(clientId, limit = 100, window = 3600) {
    const key = `rate_limit:${clientId}`;
    const now = Date.now();

    const existing = await this.kv.get(key, { type: 'json' });
    const current = existing || { count: 0, resetTime: now + window * 1000 };

    if (now > current.resetTime) {
      current.count = 0;
      current.resetTime = now + window * 1000;
    }

    if (current.count >= limit) {
      return {
        allowed: false,
        retryAfter: Math.ceil((current.resetTime - now) / 1000)
      };
    }

    current.count++;
    await this.kv.put(key, JSON.stringify(current), { expirationTtl: window });

    return { allowed: true, remaining: limit - current.count };
  }
}

// Root endpoint - MCP server info
function handleRoot() {
  const serverInfo = {
    name: 'Guile ChangeFlow MCP Server',
    version: '0.1.0',
    protocol: 'MCP/1.0',
    capabilities: {
      tools: true,
      notifications: true,
      logging: true
    },
    endpoints: {
      initialize: '/mcp/initialize',
      sse: '/mcp/sse',
      oauth: '/oauth/authorize'
    }
  };

  return new Response(JSON.stringify(serverInfo, null, 2), {
    headers: { 'Content-Type': 'application/json' }
  });
}

// Health check endpoint
async function handleHealthCheck(env) {
  const checks = {
    timestamp: new Date().toISOString(),
    status: 'healthy',
    checks: {
      kv_storage: await testKVStorage(env),
      durable_objects: await testDurableObjects(env),
      external_apis: await testExternalAPIs()
    }
  };

  const allHealthy = Object.values(checks.checks).every(check => check.status === 'ok');

  return new Response(JSON.stringify(checks, null, 2), {
    status: allHealthy ? 200 : 503,
    headers: { 'Content-Type': 'application/json' }
  });
}

async function testKVStorage(env) {
  try {
    await env.MCP_CACHE.put('health_check', 'ok', { expirationTtl: 60 });
    const result = await env.MCP_CACHE.get('health_check');
    return { status: result === 'ok' ? 'ok' : 'error', message: 'KV storage functional' };
  } catch (error) {
    return { status: 'error', message: error.message };
  }
}

async function testDurableObjects(env) {
  try {
    const id = env.MCP_CONNECTIONS.idFromName('health_check');
    const stub = env.MCP_CONNECTIONS.get(id);
    await stub.healthCheck();
    return { status: 'ok', message: 'Durable Objects functional' };
  } catch (error) {
    return { status: 'error', message: error.message };
  }
}

async function testExternalAPIs() {
  // In production, test connectivity to GitHub, OAuth provider, etc.
  return { status: 'ok', message: 'External APIs accessible' };
}

// MCP Protocol initialization
async function handleMCPInitialize(request, env) {
  if (request.method !== 'POST') {
    return new Response('Method not allowed', { status: 405 });
  }

  const body = await request.json();

  // Validate MCP initialization request
  if (!body.version || !body.capabilities) {
    return new Response('Invalid MCP initialization', { status: 400 });
  }

  const serverCapabilities = {
    tools: [
      {
        name: 'create_change_request',
        description: 'Create a new change request with automatic risk assessment',
        inputSchema: {
          type: 'object',
          properties: {
            title: { type: 'string' },
            description: { type: 'string' },
            systems: { type: 'array', items: { type: 'string' } },
            urgency: { type: 'string', enum: ['normal', 'high', 'emergency'] }
          },
          required: ['title', 'description']
        }
      },
      {
        name: 'assess_change_risk',
        description: 'Analyze risk factors for a proposed change',
        inputSchema: {
          type: 'object',
          properties: {
            changeId: { type: 'string' }
          },
          required: ['changeId']
        }
      },
      {
        name: 'check_freeze_periods',
        description: 'Verify if deployments are allowed in current timeframe',
        inputSchema: {
          type: 'object',
          properties: {
            startTime: { type: 'string', format: 'date-time' },
            endTime: { type: 'string', format: 'date-time' }
          }
        }
      }
    ],
    notifications: true,
    logging: true
  };

  return new Response(JSON.stringify({
    version: '1.0.0',
    capabilities: serverCapabilities,
    serverInfo: {
      name: 'Guile ChangeFlow',
      version: '0.1.0'
    }
  }), {
    headers: { 'Content-Type': 'application/json' }
  });
}

// Server-Sent Events for MCP streaming
async function handleServerSentEvents(request, env) {
  // Verify authorization
  const authHeader = request.headers.get('Authorization');
  if (!authHeader || !await validateAuth(authHeader, env)) {
    return new Response('Unauthorized', { status: 401 });
  }

  // Create Durable Object for connection state
  const connectionId = crypto.randomUUID();
  const id = env.MCP_CONNECTIONS.idFromName(connectionId);
  const stub = env.MCP_CONNECTIONS.get(id);

  // Establish SSE connection
  const { readable, writable } = new TransformStream();
  const writer = writable.getWriter();

  // Initialize connection in Durable Object
  stub.initializeConnection(connectionId, request);

  // Set up SSE headers
  return new Response(readable, {
    headers: {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive',
    }
  });
}

// OAuth authorization endpoint
async function handleOAuthAuthorize(request, env) {
  const url = new URL(request.url);
  const clientId = url.searchParams.get('client_id');
  const redirectUri = url.searchParams.get('redirect_uri');
  const state = url.searchParams.get('state');
  const codeChallenge = url.searchParams.get('code_challenge');

  // Validate OAuth parameters
  if (!clientId || !redirectUri || !state || !codeChallenge) {
    return new Response('Invalid OAuth request', { status: 400 });
  }

  // Store OAuth session
  await env.OAUTH_SESSIONS.put(state, JSON.stringify({
    clientId,
    redirectUri,
    codeChallenge,
    timestamp: Date.now()
  }), { expirationTtl: 600 }); // 10 minutes

  // Redirect to OAuth provider
  const authUrl = `https://accounts.google.com/oauth2/v2/auth?` +
    `client_id=${env.OAUTH_CLIENT_ID}&` +
    `redirect_uri=${encodeURIComponent(env.OAUTH_REDIRECT_URI)}&` +
    `scope=openid%20email%20profile&` +
    `response_type=code&` +
    `state=${state}`;

  return Response.redirect(authUrl, 302);
}

// OAuth callback handler
async function handleOAuthCallback(request, env) {
  const url = new URL(request.url);
  const code = url.searchParams.get('code');
  const state = url.searchParams.get('state');

  if (!code || !state) {
    return new Response('Invalid OAuth callback', { status: 400 });
  }

  // Retrieve OAuth session
  const sessionData = await env.OAUTH_SESSIONS.get(state);
  if (!sessionData) {
    return new Response('Invalid or expired OAuth state', { status: 400 });
  }

  const session = JSON.parse(sessionData);

  // Exchange code for token
  const tokenResponse = await fetch('https://oauth2.googleapis.com/token', {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      code,
      client_id: env.OAUTH_CLIENT_ID,
      client_secret: env.OAUTH_CLIENT_SECRET,
      redirect_uri: env.OAUTH_REDIRECT_URI
    })
  });

  const tokens = await tokenResponse.json();

  if (!tokens.access_token) {
    return new Response('OAuth token exchange failed', { status: 400 });
  }

  // Generate session token for MCP
  const sessionToken = await generateSessionToken(tokens, env);

  // Clean up OAuth session
  await env.OAUTH_SESSIONS.delete(state);

  // Redirect back to client with session token
  const redirectUrl = `${session.redirectUri}?token=${sessionToken}&state=${state}`;
  return Response.redirect(redirectUrl, 302);
}

// Changes API endpoint
async function handleChangesAPI(request, env) {
  if (request.method === 'GET') {
    return await listChanges(env);
  } else if (request.method === 'POST') {
    return await createChange(request, env);
  } else {
    return new Response('Method not allowed', { status: 405 });
  }
}

async function listChanges(env) {
  // In production, fetch from database
  const changes = [
    {
      id: 'CHG-001',
      title: 'Update API documentation',
      status: 'approved',
      riskScore: 15,
      createdAt: '2025-09-13T10:00:00Z'
    },
    {
      id: 'CHG-002',
      title: 'Deploy payment gateway update',
      status: 'pending',
      riskScore: 78,
      createdAt: '2025-09-13T11:30:00Z'
    }
  ];

  return new Response(JSON.stringify(changes), {
    headers: { 'Content-Type': 'application/json' }
  });
}

async function createChange(request, env) {
  const body = await request.json();

  // Validate request
  if (!body.title || !body.description) {
    return new Response('Title and description required', { status: 400 });
  }

  // Calculate risk score (simplified)
  const riskScore = calculateRiskScore(body);

  const change = {
    id: `CHG-${Date.now().toString().slice(-6)}`,
    title: body.title,
    description: body.description,
    systems: body.systems || [],
    riskScore,
    status: riskScore < 30 ? 'approved' : 'pending',
    createdAt: new Date().toISOString()
  };

  // In production, save to database
  await env.CHANGE_CACHE.put(change.id, JSON.stringify(change));

  return new Response(JSON.stringify(change), {
    status: 201,
    headers: { 'Content-Type': 'application/json' }
  });
}

// Simplified risk calculation
function calculateRiskScore(change) {
  let score = 10; // Base score

  // Production impact
  if (change.title.toLowerCase().includes('production') ||
      change.title.toLowerCase().includes('prod')) {
    score += 40;
  }

  // Security related
  if (change.description.toLowerCase().includes('security') ||
      change.description.toLowerCase().includes('auth')) {
    score += 30;
  }

  // Payment systems
  if (change.systems && change.systems.some(s => s.includes('payment'))) {
    score += 25;
  }

  // Multiple systems
  if (change.systems && change.systems.length > 1) {
    score += change.systems.length * 5;
  }

  return Math.min(100, score);
}

// Helper functions
async function validateAuth(authHeader, env) {
  // Simplified auth validation
  const token = authHeader.replace('Bearer ', '');
  const sessionData = await env.AUTH_SESSIONS.get(token);
  return !!sessionData;
}

async function generateSessionToken(tokens, env) {
  const sessionToken = crypto.randomUUID();
  await env.AUTH_SESSIONS.put(sessionToken, JSON.stringify(tokens), {
    expirationTtl: 3600 // 1 hour
  });
  return sessionToken;
}

// Durable Object for MCP connections
export class MCPConnection {
  constructor(state, env) {
    this.state = state;
    this.env = env;
    this.connections = new Map();
  }

  async initializeConnection(connectionId, request) {
    this.connections.set(connectionId, {
      id: connectionId,
      createdAt: Date.now(),
      lastActivity: Date.now()
    });

    return new Response('Connection initialized');
  }

  async healthCheck() {
    return new Response('OK');
  }

  async handleToolInvoke(toolName, params) {
    switch (toolName) {
      case 'create_change_request':
        return await this.createChangeRequest(params);
      case 'assess_change_risk':
        return await this.assessChangeRisk(params);
      case 'check_freeze_periods':
        return await this.checkFreezePeriods(params);
      default:
        throw new Error(`Unknown tool: ${toolName}`);
    }
  }

  async createChangeRequest(params) {
    const riskScore = calculateRiskScore(params);

    const change = {
      id: `CHG-${Date.now().toString().slice(-6)}`,
      title: params.title,
      description: params.description,
      systems: params.systems || [],
      urgency: params.urgency || 'normal',
      riskScore,
      status: riskScore < 30 ? 'approved' : 'pending',
      createdAt: new Date().toISOString()
    };

    // Store in KV
    await this.env.CHANGE_CACHE.put(change.id, JSON.stringify(change));

    return {
      success: true,
      change,
      message: riskScore < 30 ?
        'Change automatically approved (low risk)' :
        'Change created, approval required'
    };
  }

  async assessChangeRisk(params) {
    const changeData = await this.env.CHANGE_CACHE.get(params.changeId);
    if (!changeData) {
      throw new Error('Change not found');
    }

    const change = JSON.parse(changeData);

    return {
      changeId: change.id,
      riskScore: change.riskScore,
      category: change.riskScore < 30 ? 'low' :
                change.riskScore < 70 ? 'medium' : 'high',
      factors: this.getRiskFactors(change)
    };
  }

  async checkFreezePeriods(params) {
    // Simplified freeze period check
    const now = new Date();
    const dayOfWeek = now.getDay();
    const hour = now.getHours();

    // Example: No deployments on weekends or outside business hours
    const isFrozen = dayOfWeek === 0 || dayOfWeek === 6 || hour < 9 || hour > 17;

    return {
      frozen: isFrozen,
      reason: isFrozen ? 'Outside business hours or weekend' : null,
      nextWindow: isFrozen ? this.getNextBusinessHour() : null
    };
  }

  getRiskFactors(change) {
    const factors = [];

    if (change.title.toLowerCase().includes('production')) {
      factors.push({ name: 'Production Impact', weight: 40 });
    }

    if (change.description.toLowerCase().includes('security')) {
      factors.push({ name: 'Security Related', weight: 30 });
    }

    if (change.systems.some(s => s.includes('payment'))) {
      factors.push({ name: 'Payment System', weight: 25 });
    }

    if (change.systems.length > 1) {
      factors.push({ name: 'Multiple Systems', weight: change.systems.length * 5 });
    }

    return factors;
  }

  getNextBusinessHour() {
    const now = new Date();
    const next = new Date(now);

    // Find next Monday 9 AM if it's weekend
    if (now.getDay() === 0) { // Sunday
      next.setDate(next.getDate() + 1);
    } else if (now.getDay() === 6) { // Saturday
      next.setDate(next.getDate() + 2);
    } else if (now.getHours() >= 17) { // After hours
      next.setDate(next.getDate() + 1);
    }

    next.setHours(9, 0, 0, 0);
    return next.toISOString();
  }
}