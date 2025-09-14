/**
 * Cloudflare Worker Entry Point
 *
 * Combines OAuth wrapper with MCP server
 */

import OAuthProvider from './oauth-wrapper.js';
import { MCPRouter } from './mcp-server.js';

// Create OAuth provider with MCP server as the API handler
const provider = new OAuthProvider({
  apiRoute: '/sse',
  apiHandler: MCPRouter,
  authorizeEndpoint: '/authorize',
  tokenEndpoint: '/token',
  callbackEndpoint: '/callback'
});

// Export for Cloudflare Workers
export default {
  async fetch(request, env, ctx) {
    return provider.fetch(request, env, ctx);
  }
};