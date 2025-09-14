/**
 * Guile ChangeFlow MCP Server - Production Worker
 * Implements Model Context Protocol with ITIL 4 Change Management
 * With Workers Logs and sampling for monitoring
 */

// Risk calculation factors
const RISK_FACTORS = {
  production: 40,
  security: 30,
  payment: 20,
  emergency: 25,
  systemImpact: 10,
  baseScore: 10
};

// Change request storage (in-memory for demo, use KV for production)
const changeRequests = new Map();

// Sampling configuration for Workers Logs
const LOG_SAMPLING_RATE = 0.1; // Log 10% of requests

/**
 * Calculate risk score based on change details
 */
function calculateRiskScore(title, description, systems = [], urgency = 'normal') {
  let score = RISK_FACTORS.baseScore;

  const text = `${title} ${description}`.toLowerCase();

  if (text.includes('production') || text.includes('prod')) {
    score += RISK_FACTORS.production;
  }

  if (text.includes('security') || text.includes('auth') || text.includes('authentication')) {
    score += RISK_FACTORS.security;
  }

  if (text.includes('payment') || text.includes('financial') || text.includes('billing')) {
    score += RISK_FACTORS.payment;
  }

  if (urgency === 'emergency') {
    score += RISK_FACTORS.emergency;
  }

  score += systems.length * RISK_FACTORS.systemImpact;

  return Math.min(score, 100);
}

/**
 * Get risk category from score
 */
function getRiskCategory(score) {
  if (score < 30) return 'low';
  if (score < 70) return 'medium';
  return 'high';
}

/**
 * Structured logging with sampling
 */
function logRequest(request, response, startTime, env) {
  const shouldLog = Math.random() < LOG_SAMPLING_RATE;

  if (!shouldLog) return;

  const duration = Date.now() - startTime;
  const url = new URL(request.url);

  const logEntry = {
    timestamp: new Date().toISOString(),
    method: request.method,
    path: url.pathname,
    status: response.status,
    duration_ms: duration,
    cf_ray: request.headers.get('cf-ray'),
    country: request.headers.get('cf-ipcountry'),
    sampling_rate: LOG_SAMPLING_RATE,
    user_agent: request.headers.get('user-agent'),
    type: 'request'
  };

  console.log(JSON.stringify(logEntry));
}

/**
 * Log tool invocations (always log these as they're important)
 */
function logToolInvocation(toolName, params, result, duration) {
  const logEntry = {
    timestamp: new Date().toISOString(),
    type: 'tool_invocation',
    tool: toolName,
    params: params,
    success: !result.error,
    duration_ms: duration,
    risk_score: result.risk_score,
    change_id: result.id
  };

  console.log(JSON.stringify(logEntry));
}

export default {
  async fetch(request, env, ctx) {
    const startTime = Date.now();
    const url = new URL(request.url);

    // CORS headers for Claude.ai
    const headers = {
      'Access-Control-Allow-Origin': 'https://claude.ai',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization, anthropic-version',
      'Content-Type': 'application/json'
    };

    // Handle preflight requests
    if (request.method === 'OPTIONS') {
      const response = new Response(null, { status: 204, headers });
      logRequest(request, response, startTime, env);
      return response;
    }

    try {
      let response;

      switch (url.pathname) {
        case '/':
        case '/health':
          response = new Response(JSON.stringify({
            status: 'healthy',
            service: 'Guile ChangeFlow MCP Server',
            version: '1.0.0',
            timestamp: new Date().toISOString(),
            environment: 'production',
            capabilities: ['mcp', 'change_management', 'risk_assessment']
          }), { headers });
          break;

        case '/.well-known/mcp':
        case '/mcp':
          response = new Response(JSON.stringify({
            mcp_version: '1.0.0',
            server_name: 'guile-changeflow',
            server_version: '1.0.0',
            description: 'ITIL 4-compliant change management system with automatic risk assessment',
            capabilities: {
              tools: true,
              prompts: false,
              resources: false,
              notifications: true
            }
          }), { headers });
          break;

        case '/tools':
        case '/mcp/tools':
          response = new Response(JSON.stringify([
            {
              name: 'create_change_request',
              description: 'Create a new change request with automatic risk assessment',
              inputSchema: {
                type: 'object',
                properties: {
                  title: {
                    type: 'string',
                    description: 'Brief title of the change'
                  },
                  description: {
                    type: 'string',
                    description: 'Detailed description of the change'
                  },
                  systems: {
                    type: 'array',
                    items: { type: 'string' },
                    description: 'List of affected systems'
                  },
                  urgency: {
                    type: 'string',
                    enum: ['low', 'normal', 'high', 'emergency'],
                    description: 'Urgency level of the change'
                  }
                },
                required: ['title', 'description']
              }
            },
            {
              name: 'get_change_request',
              description: 'Retrieve a specific change request by ID',
              inputSchema: {
                type: 'object',
                properties: {
                  id: {
                    type: 'string',
                    description: 'Change request ID'
                  }
                },
                required: ['id']
              }
            },
            {
              name: 'list_change_requests',
              description: 'List all change requests with optional filtering',
              inputSchema: {
                type: 'object',
                properties: {
                  status: {
                    type: 'string',
                    enum: ['submitted', 'assessing', 'approved', 'rejected', 'completed'],
                    description: 'Filter by status'
                  },
                  risk_category: {
                    type: 'string',
                    enum: ['low', 'medium', 'high'],
                    description: 'Filter by risk category'
                  }
                }
              }
            },
            {
              name: 'assess_risk',
              description: 'Perform risk assessment on a proposed change',
              inputSchema: {
                type: 'object',
                properties: {
                  title: { type: 'string' },
                  description: { type: 'string' },
                  systems: {
                    type: 'array',
                    items: { type: 'string' }
                  },
                  urgency: {
                    type: 'string',
                    enum: ['low', 'normal', 'high', 'emergency']
                  }
                },
                required: ['title', 'description']
              }
            }
          ]), { headers });
          break;

        case '/tools/create_change_request/invoke':
        case '/mcp/tools/invoke':
          const body = await request.json();
          const toolStartTime = Date.now();

          if (!body.tool && !body.name) {
            response = new Response(JSON.stringify({
              error: 'Missing tool name'
            }), { status: 400, headers });
            break;
          }

          const toolName = body.tool || body.name;
          const params = body.params || body.arguments || {};

          let result;

          switch (toolName) {
            case 'create_change_request':
              const riskScore = calculateRiskScore(
                params.title,
                params.description,
                params.systems,
                params.urgency
              );

              const changeId = `CHG-${new Date().getFullYear()}-${String(changeRequests.size + 1).padStart(3, '0')}`;

              const changeRequest = {
                id: changeId,
                title: params.title,
                description: params.description,
                systems: params.systems || [],
                urgency: params.urgency || 'normal',
                risk_score: riskScore,
                risk_category: getRiskCategory(riskScore),
                status: 'submitted',
                created_at: new Date().toISOString(),
                created_by: 'claude-ai',
                updated_at: new Date().toISOString()
              };

              changeRequests.set(changeId, changeRequest);

              // Auto-approve low risk changes
              if (riskScore < 30) {
                changeRequest.status = 'approved';
                changeRequest.approved_at = new Date().toISOString();
                changeRequest.approval_note = 'Auto-approved due to low risk';
              }

              result = changeRequest;
              break;

            case 'get_change_request':
              const change = changeRequests.get(params.id);
              if (!change) {
                result = { error: 'Change request not found' };
              } else {
                result = change;
              }
              break;

            case 'list_change_requests':
              let changes = Array.from(changeRequests.values());

              if (params.status) {
                changes = changes.filter(c => c.status === params.status);
              }

              if (params.risk_category) {
                changes = changes.filter(c => c.risk_category === params.risk_category);
              }

              result = {
                count: changes.length,
                changes: changes.sort((a, b) =>
                  new Date(b.created_at) - new Date(a.created_at)
                )
              };
              break;

            case 'assess_risk':
              const score = calculateRiskScore(
                params.title,
                params.description,
                params.systems,
                params.urgency
              );

              result = {
                risk_score: score,
                risk_category: getRiskCategory(score),
                factors: {
                  production_impact: params.title?.toLowerCase().includes('production'),
                  security_impact: params.title?.toLowerCase().includes('security'),
                  payment_impact: params.title?.toLowerCase().includes('payment'),
                  urgency: params.urgency || 'normal',
                  affected_systems: params.systems?.length || 0
                },
                recommendation: score < 30 ? 'Auto-approve' :
                               score < 70 ? 'Single approval required' :
                               'CAB review required'
              };
              break;

            default:
              result = {
                error: `Unknown tool: ${toolName}`,
                available_tools: [
                  'create_change_request',
                  'get_change_request',
                  'list_change_requests',
                  'assess_risk'
                ]
              };
          }

          const toolDuration = Date.now() - toolStartTime;
          logToolInvocation(toolName, params, result, toolDuration);

          response = new Response(JSON.stringify(result), {
            status: result.error ? 400 : 200,
            headers
          });
          break;

        case '/api/changes':
          if (request.method === 'GET') {
            const changes = Array.from(changeRequests.values());
            response = new Response(JSON.stringify(changes), { headers });
          } else if (request.method === 'POST') {
            const data = await request.json();
            const riskScore = calculateRiskScore(
              data.title,
              data.description,
              data.systems,
              data.urgency
            );

            const changeId = `CHG-${new Date().getFullYear()}-${String(changeRequests.size + 1).padStart(3, '0')}`;

            const changeRequest = {
              id: changeId,
              title: data.title,
              description: data.description,
              systems: data.systems || [],
              urgency: data.urgency || 'normal',
              risk_score: riskScore,
              risk_category: getRiskCategory(riskScore),
              status: 'submitted',
              created_at: new Date().toISOString(),
              created_by: 'api',
              updated_at: new Date().toISOString()
            };

            changeRequests.set(changeId, changeRequest);
            response = new Response(JSON.stringify(changeRequest), {
              status: 201,
              headers
            });
          } else {
            response = new Response(JSON.stringify({
              error: 'Method not allowed'
            }), { status: 405, headers });
          }
          break;

        case '/webhooks/github':
          if (request.method === 'POST') {
            const webhook = await request.json();

            if (webhook.action === 'opened' && webhook.pull_request) {
              const pr = webhook.pull_request;
              const riskScore = calculateRiskScore(
                pr.title,
                pr.body || '',
                ['github'],
                'normal'
              );

              const changeId = `CHG-GH-${pr.number}`;

              const changeRequest = {
                id: changeId,
                title: `GitHub PR #${pr.number}: ${pr.title}`,
                description: pr.body || 'No description provided',
                systems: ['github', pr.base.repo.name],
                urgency: pr.labels.some(l => l.name === 'urgent') ? 'high' : 'normal',
                risk_score: riskScore,
                risk_category: getRiskCategory(riskScore),
                status: 'submitted',
                created_at: new Date().toISOString(),
                created_by: pr.user.login,
                updated_at: new Date().toISOString(),
                external_reference: {
                  type: 'github_pr',
                  url: pr.html_url,
                  number: pr.number
                }
              };

              changeRequests.set(changeId, changeRequest);

              console.log(JSON.stringify({
                timestamp: new Date().toISOString(),
                type: 'webhook_processed',
                source: 'github',
                change_id: changeId,
                pr_number: pr.number
              }));

              response = new Response(JSON.stringify(changeRequest), {
                status: 201,
                headers
              });
            } else {
              response = new Response(JSON.stringify({
                message: 'Webhook received but not processed'
              }), { headers });
            }
          } else {
            response = new Response(JSON.stringify({
              error: 'Method not allowed'
            }), { status: 405, headers });
          }
          break;

        default:
          response = new Response(JSON.stringify({
            error: 'Not Found',
            message: `Path ${url.pathname} not found`,
            available_endpoints: [
              '/health',
              '/mcp',
              '/tools',
              '/mcp/tools/invoke',
              '/api/changes',
              '/webhooks/github'
            ]
          }), { status: 404, headers });
      }

      // Log the request/response
      logRequest(request, response, startTime, env);

      return response;

    } catch (error) {
      console.error(JSON.stringify({
        timestamp: new Date().toISOString(),
        type: 'error',
        message: error.message,
        stack: error.stack,
        path: url.pathname
      }));

      const errorResponse = new Response(JSON.stringify({
        error: 'Internal Server Error',
        message: error.message
      }), { status: 500, headers });

      logRequest(request, errorResponse, startTime, env);

      return errorResponse;
    }
  }
};