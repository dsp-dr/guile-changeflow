// Minimal skeleton MCP server for changeflow.us
// This will be replaced by agents over the weekend

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);

    // CORS for Claude
    const headers = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization',
      'Content-Type': 'application/json'
    };

    // Handle preflight
    if (request.method === 'OPTIONS') {
      return new Response(null, { status: 204, headers });
    }

    // Routes
    switch (url.pathname) {
      case '/':
      case '/health':
        return new Response(JSON.stringify({
          status: 'healthy',
          message: 'Guile ChangeFlow MCP Server (Pre-Agent Version)',
          version: '0.0.1-skeleton',
          timestamp: new Date().toISOString(),
          note: 'Agents will implement the real system soon'
        }), { headers });

      case '/mcp':
        return new Response(JSON.stringify({
          name: 'Guile ChangeFlow',
          version: '0.0.1',
          description: 'ITIL 4 Change Management (whatever that means)',
          capabilities: {
            tools: true,
            notifications: false
          },
          tools: [
            {
              name: 'create_change_request',
              description: 'Creates a change (agents will implement)',
              inputSchema: {
                type: 'object',
                properties: {
                  title: { type: 'string' },
                  description: { type: 'string' }
                },
                required: ['title']
              }
            },
            {
              name: 'assess_risk',
              description: 'Returns a random risk score for now',
              inputSchema: {
                type: 'object',
                properties: {
                  title: { type: 'string' }
                }
              }
            }
          ],
          implementation_status: 'waiting_for_agents'
        }), { headers });

      case '/mcp/tools/invoke':
        const body = await request.json().catch(() => ({}));

        if (body.tool === 'assess_risk') {
          return new Response(JSON.stringify({
            risk_score: Math.floor(Math.random() * 100),
            message: 'Random score until agents implement real algorithm',
            timestamp: new Date().toISOString()
          }), { headers });
        }

        if (body.tool === 'create_change_request') {
          return new Response(JSON.stringify({
            id: `CHG-${Date.now().toString().slice(-6)}`,
            title: body.params?.title || 'Untitled',
            status: 'not_implemented',
            message: 'Agents will build the real system',
            timestamp: new Date().toISOString()
          }), { headers });
        }

        return new Response(JSON.stringify({
          error: 'Tool not implemented yet',
          available_tools: ['assess_risk', 'create_change_request'],
          message: 'Agents starting work soon'
        }), { status: 501, headers });

      default:
        return new Response(JSON.stringify({
          error: 'Not Found',
          message: 'This endpoint will be implemented by agents',
          available_endpoints: ['/', '/health', '/mcp', '/mcp/tools/invoke']
        }), { status: 404, headers });
    }
  }
};