/**
 * MCP Protocol Implementation for Cloudflare Worker
 * POC for 7 AM Demo
 * 
 * TODO for collaborator:
 * 1. Replace skeleton responses with real MCP handlers
 * 2. Implement all 8 ITIL tools
 * 3. Add proper error handling
 * 4. Test with scripts/test-endpoints.sh
 */

// MCP Protocol Version
const PROTOCOL_VERSION = '2024-11-05';

// Available ITIL Tools
const TOOLS = [
  {
    name: 'create_change_request',
    description: 'Create a new ITIL change request',
    inputSchema: {
      type: 'object',
      properties: {
        title: { type: 'string' },
        description: { type: 'string' },
        risk_level: { type: 'string', enum: ['low', 'medium', 'high', 'critical'] },
        environment: { type: 'string', enum: ['dev', 'test', 'staging', 'production'] }
      },
      required: ['title', 'description', 'risk_level', 'environment']
    }
  },
  {
    name: 'assess_change_risk',
    description: 'Assess risk score for a change',
    inputSchema: {
      type: 'object',
      properties: {
        change_type: { type: 'string' },
        environment: { type: 'string' },
        components_affected: { type: 'number' },
        has_rollback: { type: 'boolean' },
        tested_in_staging: { type: 'boolean' }
      },
      required: ['change_type', 'environment']
    }
  },
  // TODO: Add remaining 6 tools
];

/**
 * Main request handler
 */
export default {
  async fetch(request, env, ctx) {
    // CORS headers
    const headers = {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    };

    // Handle OPTIONS
    if (request.method === 'OPTIONS') {
      return new Response(null, { headers });
    }

    // Parse request
    let body;
    try {
      body = await request.json();
    } catch (e) {
      // Not JSON, return health check
      return new Response(JSON.stringify({
        status: 'healthy',
        message: 'MCP Server Ready',
        version: PROTOCOL_VERSION,
        timestamp: new Date().toISOString()
      }), { headers });
    }

    // Route to handler based on method
    const { method, params, id } = body;
    
    let result;
    switch (method) {
      case 'initialize':
        result = handleInitialize(params);
        break;
      case 'tools/list':
        result = handleToolsList();
        break;
      case 'tools/call':
        result = await handleToolCall(params);
        break;
      default:
        result = {
          error: {
            code: -32601,
            message: `Method not found: ${method}`
          }
        };
    }

    // Return JSON-RPC response
    return new Response(JSON.stringify({
      jsonrpc: '2.0',
      result,
      id
    }), { headers });
  },
};

/**
 * Handle initialize request
 */
function handleInitialize(params) {
  return {
    protocolVersion: PROTOCOL_VERSION,
    capabilities: {
      tools: {},
      notifications: {}
    },
    serverInfo: {
      name: 'guile-changeflow-mcp',
      version: '1.0.0'
    }
  };
}

/**
 * Handle tools/list request
 */
function handleToolsList() {
  return { tools: TOOLS };
}

/**
 * Handle tools/call request
 */
async function handleToolCall(params) {
  const { name, arguments: args } = params;
  
  // TODO: Implement actual tool logic
  switch (name) {
    case 'create_change_request':
      return {
        change_id: `CHG-${Date.now()}`,
        status: 'pending',
        risk_score: calculateRiskScore(args),
        created_at: new Date().toISOString()
      };
    
    case 'assess_change_risk':
      return {
        risk_score: calculateRiskScore(args),
        risk_level: getRiskLevel(calculateRiskScore(args)),
        factors: getRiskFactors(args)
      };
    
    default:
      return {
        error: `Tool not implemented: ${name}`
      };
  }
}

/**
 * Calculate risk score
 */
function calculateRiskScore(params) {
  let score = 0;
  
  // Environment risk
  if (params.environment === 'production') score += 40;
  else if (params.environment === 'staging') score += 20;
  else if (params.environment === 'test') score += 10;
  
  // Testing risk
  if (!params.tested_in_staging) score += 20;
  if (!params.has_rollback) score += 15;
  
  // Components risk
  if (params.components_affected > 10) score += 20;
  else if (params.components_affected > 5) score += 10;
  
  // Time risk (weekend/holiday)
  const now = new Date();
  if (now.getDay() === 0 || now.getDay() === 6) score += 10;
  
  return Math.min(score, 100);
}

/**
 * Get risk level from score
 */
function getRiskLevel(score) {
  if (score >= 80) return 'critical';
  if (score >= 60) return 'high';
  if (score >= 40) return 'medium';
  return 'low';
}

/**
 * Get risk factors
 */
function getRiskFactors(params) {
  const factors = [];
  
  if (params.environment === 'production') {
    factors.push('Production environment (+40 risk)');
  }
  if (!params.tested_in_staging) {
    factors.push('Not tested in staging (+20 risk)');
  }
  if (!params.has_rollback) {
    factors.push('No rollback plan (+15 risk)');
  }
  
  return factors;
}

// TODO: Implement remaining tools
// TODO: Add persistence (KV or D1)
// TODO: Add authentication
// TODO: Add rate limiting
// TODO: Add comprehensive error handling