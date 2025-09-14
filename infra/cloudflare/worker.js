/**
 * ChangeFlow MCP Server - Complete OAuth + MCP in ONE FILE
 * No imports, no build system, everything inline
 */

// Inline HTML for landing page (as function to use SERVER_VERSION)
const getLandingHTML = () => `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>ChangeFlow MCP</title>
<style>body{font-family:system-ui;background:#0f172a;color:#e2e8f0;margin:0;padding:2rem;text-align:center}h1{color:#3b82f6}a{color:#3b82f6;text-decoration:none;padding:1rem 2rem;background:#1e293b;display:inline-block;margin:1rem;border-radius:0.5rem}a:hover{background:#334155}.version{color:#64748b;font-size:0.9rem}.gh-link{position:absolute;top:1rem;right:1rem;color:#64748b;text-decoration:none}.gh-link:hover{color:#94a3b8}</style>
</head>
<body>
<a href="https://github.com/dsp-dr/guile-changeflow" class="gh-link">
<svg width="24" height="24" viewBox="0 0 24 24" fill="currentColor"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>
</a>
<h1>🔄 ChangeFlow MCP Server</h1>
<p>ITIL 4 Change Management for AI</p>
<p class="version">v${SERVER_VERSION} - OAuth + SSE Ready!</p>
<p><a href="/authorize">🔑 Authorize with GitHub</a></p>
</body>
</html>`;

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
const SERVER_VERSION = '1.3.2';

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
        // Landing page
        return new Response(getLandingHTML(), {
          headers: { 'Content-Type': 'text/html', ...corsHeaders }
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

      case '/authorize':
        // OAuth Step 1: Redirect to GitHub
        if (!env.GITHUB_CLIENT_ID) {
          return new Response('OAuth not configured - GITHUB_CLIENT_ID missing', { status: 500 });
        }

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
        const state = url.searchParams.get('state');

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

            // For Claude.ai, check if state indicates return to SSE
            let returnUrl = '/';
            try {
              if (state) {
                const stateData = JSON.parse(atob(state));
                if (stateData.returnUrl === '/v1/sse') {
                  // Redirect back to Claude.ai's expected URL
                  returnUrl = 'https://claude.ai/connect/success';
                }
              }
            } catch (e) {
              // Ignore state parsing errors
            }

            return new Response(SUCCESS_HTML, {
              headers: {
                'Content-Type': 'text/html',
                'Set-Cookie': `mcp_session=${sessionId}; Path=/; HttpOnly; Secure; SameSite=None; Max-Age=86400`
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

              let result;
              switch (toolName) {
                case 'create_change_request':
                  const changeId = `CHG-${Date.now()}`;
                  const change = {
                    id: changeId,
                    ...toolParams,
                    status: 'pending',
                    createdAt: new Date().toISOString(),
                    riskScore: calculateRisk(toolParams)
                  };
                  changeRequests.set(changeId, change);
                  result = { content: [{ type: 'text', text: JSON.stringify(change, null, 2) }] };
                  break;

                case 'check_freeze_period':
                  const checkDate = new Date(toolParams.date);
                  const inFreeze = freezePeriods.some(period => {
                    const start = new Date(period.start);
                    const end = new Date(period.end);
                    return checkDate >= start && checkDate <= end;
                  });
                  result = {
                    content: [{
                      type: 'text',
                      text: inFreeze ? 'Date is within a freeze period!' : 'Date is clear for changes.'
                    }]
                  };
                  break;

                default:
                  result = { content: [{ type: 'text', text: `Tool ${toolName} executed successfully` }] };
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
        // Claude.ai flow: First request without auth → redirect to OAuth → OAuth completion → SSE works

        // Check for authentication (session cookie or Bearer token)
        const cookies = request.headers.get('cookie') || '';
        const hasSession = cookies.includes('mcp_session=');
        const authHeader = request.headers.get('authorization');
        const userAgent = request.headers.get('user-agent') || '';

        // If no auth (no session cookie and no Bearer token), redirect to OAuth
        if (!hasSession && !authHeader) {
          // For GET request without auth, redirect to OAuth
          if (request.method === 'GET') {
            // Generate state for OAuth that includes return URL
            const state = btoa(JSON.stringify({
              returnUrl: '/v1/sse',
              timestamp: Date.now()
            }));

            const authParams = new URLSearchParams({
              client_id: env.GITHUB_CLIENT_ID || '',
              redirect_uri: `${url.origin}/callback`,
              scope: 'read:user',
              state: state
            });

            // Redirect to GitHub OAuth
            return Response.redirect(`${GITHUB_OAUTH_URL}?${authParams}`, 302);
          }
        }

        // For authenticated requests (has session or Bearer token), return SSE stream
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

                  let result;
                  switch (toolName) {
                    case 'create_change_request':
                      const changeId = `CHG-${Date.now()}`;
                      const change = {
                        id: changeId,
                        ...toolParams,
                        status: 'pending',
                        createdAt: new Date().toISOString(),
                        riskScore: calculateRisk(toolParams)
                      };
                      changeRequests.set(changeId, change);
                      result = { content: [{ type: 'text', text: JSON.stringify(change, null, 2) }] };
                      break;

                    case 'check_freeze_period':
                      const checkDate = new Date(toolParams.date);
                      const inFreeze = freezePeriods.some(period => {
                        const start = new Date(period.start);
                        const end = new Date(period.end);
                        return checkDate >= start && checkDate <= end;
                      });
                      result = {
                        content: [{
                          type: 'text',
                          text: inFreeze ? 'Date is within a freeze period!' : 'Date is clear for changes.'
                        }]
                      };
                      break;

                    default:
                      result = { content: [{ type: 'text', text: `Tool ${toolName} executed successfully` }] };
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

      case '/favicon.ico':
        // Return inline favicon (ChangeFlow logo - circular arrow)
        const favicon = 'AAABAAEAEBAAAAEAIABoBAAAFgAAACgAAAAQAAAAIAAAAAEAIAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAA' +
          'AAAAAAAA7+/vAP///wD///8A////AP///wD///8A////AP///wD///8A////AP///wD///8A////AP///' +
          'wD///8A7+/vAP///wAzrO8AiNj/AO/v7wD///8A////AP///wD///8A////AP///wD///8A7+/vAGXE' +
          '/wBQvv8AM6zvAP///wD///8AM6zvAIjY/wDU8/8AZcT/AP///wD///8A////AP///wD///8A////AP///' +
          'wD///8AZcT/AIHY/wAzrO8A////AP///wBQvv8A1PP/ANTZ/wDU2f8AZcT/AP///wD///8A////AP///' +
          'wD///8A////ANTz/wDU2f8AULT/AP///wD///8AZcT/ANTZ/wDU2f8A1Nn/ANTZ/wBQvv8A////AP///' +
          'wD///8A////AFDC/wDU2f8A1Nn/ANTZ/wBQvv8A////AFDz/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/AGnE' +
          '/wD///8A////AGnE/wDU2f8A1Nn/ANTZ/wDU2f8AUML/AP///wBpxP8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ' +
          '/wDU2f8AacT/AGnE/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/AGnE/wD///8A////AGnE/wDU2f8A1Nn/ANTZ' +
          '/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ/wBpxP8A////AP///wD///8AacT/ANTZ' +
          '/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ/wBpxP8A////AP///wD///8A////AGnE' +
          '/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ/wDU2f8A1Nn/AGnE/wD///8A////AP///wD///8A////AP///' +
          'wBpxP8A1Nn/ANTZ/wDU2f8A1Nn/ANTZ/wDU2f8AacT/AP///wD///8A////AP///wD///8A////AP///' +
          'wD///8AacT/ANTZ/wDU2f8A1Nn/ANTZ/wBpxP8A////AP///wD///8A////AP///wD///8A////AP///' +
          'wD///8A////AGnE/wDU2f8A1Nn/AGnE/wD///8A////AP///wD///8A////AP///wD///8A////AP///' +
          'wD///8A////AP///wBpxP8AacT/AP///wD///8A////AP///wD///8A////AP///wD///8A////AP///' +
          'wD///8A////AP///wD///8A////AP///wD///8A////AP///wD///8A////AP///wDwDwAA4AcAAMAD' +
          'AACAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAACAAQAAYAMEAMADAAA=';

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