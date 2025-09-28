/**
 * Landing page worker for changeflow.us and www.changeflow.us
 * Serves favicon and redirects to main MCP server
 */

export default {
  async fetch(request, env) {
    const url = new URL(request.url);

    // CORS headers
    const corsHeaders = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    };

    // Handle OPTIONS
    if (request.method === 'OPTIONS') {
      return new Response(null, { headers: corsHeaders });
    }

    switch (url.pathname) {
      case '/favicon.ico':
        // Same favicon as MCP server - tiny 16x16 16-color (318 bytes)
        const favicon = 'AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAgAAAAAAAAAAAAAAAEAAAABAAAACZMyIAqlUiALtmVQC7iGYA7u4iAN3MVQDMqiIA7t2qAO7MzAD//90A////AP/u7gD///8AzJmZAAAAAAAAAAAAqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqsi2qqqqqqqogIDqqqqqqqxrIF6qqqqqqO4tWeaqqqqq4i7VEqqqqqqqqqnmqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA';

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

      case '/robots.txt':
        return new Response(`User-agent: *
Allow: /favicon.ico
Disallow: /

# MCP Server at https://mcp.changeflow.us
`, {
          headers: {
            'Content-Type': 'text/plain',
            ...corsHeaders
          }
        });

      case '/':
        // Simple landing page that redirects to MCP server
        return new Response(`<!DOCTYPE html>
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
<meta http-equiv="refresh" content="5;url=https://mcp.changeflow.us">
<style>
body {
  font-family: system-ui, -apple-system, sans-serif;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  margin: 0;
  padding: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 100vh;
}
.container {
  text-align: center;
  padding: 2rem;
  background: rgba(255, 255, 255, 0.1);
  border-radius: 1rem;
  backdrop-filter: blur(10px);
  max-width: 500px;
}
h1 {
  margin: 0 0 1rem 0;
  font-size: 2.5rem;
}
.logo {
  font-size: 4rem;
  margin-bottom: 1rem;
}
p {
  margin: 1rem 0;
  font-size: 1.1rem;
  line-height: 1.6;
}
a {
  color: #60a5fa;
  text-decoration: none;
  font-weight: bold;
}
a:hover {
  text-decoration: underline;
}
.redirect {
  margin-top: 2rem;
  padding: 1rem;
  background: rgba(59, 130, 246, 0.2);
  border-radius: 0.5rem;
}
</style>
</head>
<body>
<div class="container">
  <div class="logo">🔄</div>
  <h1>ChangeFlow</h1>
  <p>ITIL 4 Change Management MCP Server</p>
  <p>Integrate enterprise change management capabilities with Claude.ai through the Model Context Protocol.</p>

  <div class="redirect">
    <p>Redirecting to MCP server in 5 seconds...</p>
    <p>Go directly to <a href="https://mcp.changeflow.us">mcp.changeflow.us</a></p>
  </div>

  <p style="margin-top: 2rem; font-size: 0.9rem; opacity: 0.8;">
    Connect with Claude.ai using:<br>
    <code style="background: rgba(0,0,0,0.3); padding: 0.25rem 0.5rem; border-radius: 0.25rem;">https://mcp.changeflow.us</code>
  </p>
</div>
</body>
</html>`, {
          headers: {
            'Content-Type': 'text/html; charset=UTF-8',
            ...corsHeaders
          }
        });

      default:
        // For any other path, redirect to MCP server
        return Response.redirect(`https://mcp.changeflow.us${url.pathname}${url.search}`, 301);
    }
  }
};