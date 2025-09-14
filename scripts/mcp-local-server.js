#!/usr/bin/env node

/**
 * Local MCP Server for Claude Code Integration
 * Runs the CloudFlare Worker MCP implementation locally for testing
 */

const http = require('http');
const fs = require('fs');
const path = require('path');

// Configuration
const PORT = process.env.MCP_PORT || 8080;
const WORKER_PATH = path.join(__dirname, '../infra/cloudflare/worker.js');

// Mock CloudFlare Worker globals
global.Response = class LocalResponse {
  constructor(body, options = {}) {
    this.body = body;
    this.status = options.status || 200;
    this.headers = options.headers || {};
    this._json = null;
  }

  async json() {
    if (!this._json) {
      this._json = JSON.parse(this.body);
    }
    return this._json;
  }

  async text() {
    return this.body;
  }
};

class MCPLocalServer {
  constructor() {
    this.worker = null;
    this.server = null;
    this.stats = {
      requests: 0,
      errors: 0,
      startTime: Date.now()
    };
  }

  async loadWorker() {
    try {
      console.log('📦 Loading MCP worker...');
      const workerCode = fs.readFileSync(WORKER_PATH, 'utf8');
      const moduleExports = {};
      eval(workerCode.replace('export default', 'moduleExports.default ='));
      this.worker = moduleExports.default;
      console.log('✅ Worker loaded successfully');
    } catch (error) {
      console.error('❌ Failed to load worker:', error.message);
      throw error;
    }
  }

  async handleRequest(req, res) {
    this.stats.requests++;
    const startTime = Date.now();

    try {
      // Enable CORS for all requests
      res.setHeader('Access-Control-Allow-Origin', '*');
      res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
      res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

      // Handle preflight requests
      if (req.method === 'OPTIONS') {
        res.statusCode = 200;
        res.end();
        return;
      }

      // Log request
      const timestamp = new Date().toISOString();
      console.log(`📨 ${timestamp} ${req.method} ${req.url}`);

      // Collect request body
      let body = '';
      req.on('data', chunk => body += chunk);

      req.on('end', async () => {
        try {
          // Create mock request object for worker
          const mockRequest = {
            method: req.method,
            url: req.url,
            headers: req.headers,
            json: async () => {
              try {
                return body ? JSON.parse(body) : {};
              } catch (e) {
                throw new Error('Invalid JSON');
              }
            }
          };

          // Call worker
          const workerResponse = await this.worker.fetch(mockRequest, {}, {});
          const responseData = await workerResponse.json();

          // Send response
          const duration = Date.now() - startTime;
          console.log(`✅ Response sent (${duration}ms)`);

          res.setHeader('Content-Type', 'application/json');
          res.statusCode = workerResponse.status || 200;
          res.end(JSON.stringify(responseData, null, 2));

        } catch (error) {
          this.stats.errors++;
          const duration = Date.now() - startTime;
          console.error(`❌ Request error (${duration}ms):`, error.message);

          res.statusCode = 500;
          res.setHeader('Content-Type', 'application/json');
          res.end(JSON.stringify({
            error: {
              message: error.message,
              code: -32603
            }
          }, null, 2));
        }
      });

    } catch (error) {
      this.stats.errors++;
      console.error('💥 Fatal request error:', error);
      res.statusCode = 500;
      res.end();
    }
  }

  async start() {
    await this.loadWorker();

    this.server = http.createServer((req, res) => this.handleRequest(req, res));

    return new Promise((resolve, reject) => {
      this.server.listen(PORT, (error) => {
        if (error) {
          reject(error);
        } else {
          console.log(`🚀 MCP Server running on http://localhost:${PORT}`);
          console.log(`📋 Ready for Claude Code integration`);
          console.log(`🔧 Test endpoints:`);
          console.log(`   Health: curl http://localhost:${PORT}/`);
          console.log(`   Tools:  curl -X POST http://localhost:${PORT}/ -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'`);
          console.log('');
          resolve();
        }
      });
    });
  }

  stop() {
    if (this.server) {
      this.server.close();
      const uptime = Math.round((Date.now() - this.stats.startTime) / 1000);
      console.log(`🛑 Server stopped after ${uptime}s`);
      console.log(`📊 Stats: ${this.stats.requests} requests, ${this.stats.errors} errors`);
    }
  }

  printStatus() {
    const uptime = Math.round((Date.now() - this.stats.startTime) / 1000);
    const rps = this.stats.requests / Math.max(uptime, 1);
    console.log(`📊 Status: ${uptime}s uptime, ${this.stats.requests} requests (${rps.toFixed(2)}/s), ${this.stats.errors} errors`);
  }
}

// Handle graceful shutdown
const server = new MCPLocalServer();

process.on('SIGINT', () => {
  console.log('\n🛑 Received SIGINT, shutting down gracefully...');
  server.stop();
  process.exit(0);
});

process.on('SIGTERM', () => {
  console.log('\n🛑 Received SIGTERM, shutting down gracefully...');
  server.stop();
  process.exit(0);
});

// Status reporting
setInterval(() => {
  if (server.stats.requests > 0) {
    server.printStatus();
  }
}, 30000); // Every 30 seconds

// Start server
if (require.main === module) {
  server.start()
    .then(() => {
      console.log('✅ MCP Server ready for Claude Code');
      console.log('💡 Add this to your Claude Code MCP configuration:');
      console.log(`   {
     "name": "guile-changeflow",
     "command": "node",
     "args": ["${__filename}"],
     "env": {}
   }`);
    })
    .catch(error => {
      console.error('💥 Failed to start server:', error);
      process.exit(1);
    });
}

module.exports = MCPLocalServer;