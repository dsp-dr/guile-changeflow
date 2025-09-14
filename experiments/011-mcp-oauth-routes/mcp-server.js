/**
 * Simple MCP Server Placeholder
 *
 * This is a minimal MCP server that agents (coordinator + builder) will replace
 * with the full implementation. We just need something that works for testing OAuth.
 */

export class SimpleMCPServer {
  constructor() {
    this.tools = [
      {
        name: 'hello_world',
        description: 'A simple hello world tool',
        inputSchema: {
          type: 'object',
          properties: {
            name: { type: 'string' }
          }
        }
      }
    ];
  }

  async handleRequest(request, env) {
    const url = new URL(request.url);

    // Simple router for MCP endpoints
    if (url.pathname === '/mcp' || url.pathname === '/.well-known/mcp') {
      return this.getCapabilities();
    }

    if (url.pathname === '/mcp/tools' || url.pathname === '/tools') {
      return this.listTools();
    }

    if (url.pathname === '/mcp/tools/invoke' || url.pathname === '/tools/invoke') {
      return this.invokeTool(request);
    }

    // SSE connection for real MCP
    if (url.pathname === '/sse') {
      return this.handleSSE(request);
    }

    return new Response('Not Found', { status: 404 });
  }

  getCapabilities() {
    return new Response(JSON.stringify({
      mcp_version: '1.0.0',
      server_name: 'simple-mcp-server',
      capabilities: {
        tools: true,
        prompts: false,
        resources: false
      }
    }), {
      headers: { 'Content-Type': 'application/json' }
    });
  }

  listTools() {
    return new Response(JSON.stringify(this.tools), {
      headers: { 'Content-Type': 'application/json' }
    });
  }

  async invokeTool(request) {
    const body = await request.json();
    const { tool, params } = body;

    if (tool === 'hello_world') {
      return new Response(JSON.stringify({
        result: `Hello, ${params.name || 'World'}!`
      }), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    return new Response(JSON.stringify({
      error: 'Unknown tool'
    }), {
      status: 400,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  handleSSE(request) {
    // Simple SSE stream for MCP connection
    const encoder = new TextEncoder();
    const stream = new ReadableStream({
      start(controller) {
        // Send initial connection message
        controller.enqueue(encoder.encode('data: {"type":"connection","status":"ready"}\\n\\n'));

        // Keep connection alive
        const interval = setInterval(() => {
          controller.enqueue(encoder.encode('data: {"type":"ping"}\\n\\n'));
        }, 30000);

        // Clean up on close
        request.signal.addEventListener('abort', () => {
          clearInterval(interval);
          controller.close();
        });
      }
    });

    return new Response(stream, {
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive',
      }
    });
  }
}

// Export a simple router function
export function MCPRouter(request, env) {
  const server = new SimpleMCPServer();
  return server.handleRequest(request, env);
}

export default SimpleMCPServer;