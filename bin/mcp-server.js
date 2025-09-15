#!/usr/bin/env node

/**
 * Guile ChangeFlow MCP Server
 * Can be run via npx or installed globally
 *
 * Usage:
 *   npx @guile-changeflow/mcp-server
 *   npx @guile-changeflow/mcp-server --local
 *   npx @guile-changeflow/mcp-server --remote https://mcp.changeflow.us
 */

const express = require('express');
const WebSocket = require('ws');
const cors = require('cors');
const helmet = require('helmet');
const winston = require('winston');
const path = require('path');
const fs = require('fs');

// Parse command line arguments
const args = process.argv.slice(2);
const isLocal = args.includes('--local');
const remoteUrl = args.find(arg => arg.startsWith('--remote'))?.split('=')[1] ||
                  process.env.MCP_REMOTE_URL ||
                  'https://mcp.changeflow.us';

// Configuration
const config = {
  mode: isLocal ? 'local' : 'remote',
  port: process.env.PORT || 8427,
  remoteUrl: remoteUrl,
  version: '1.4.4'
};

// Logger setup
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  transports: [
    new winston.transports.Console({
      format: winston.format.combine(
        winston.format.colorize(),
        winston.format.simple()
      )
    })
  ]
});

// Express app
const app = express();
app.use(helmet());
app.use(cors());
app.use(express.json());

// MCP Tools Registry
const tools = [
  {
    name: 'create_change_request',
    description: 'Create an ITIL change request',
    inputSchema: {
      type: 'object',
      properties: {
        title: { type: 'string' },
        component: { type: 'string', enum: ['frontend', 'backend-api', 'backend-db', 'iac-app'] },
        priority: { type: 'string', enum: ['low', 'medium', 'high', 'critical'] },
        change_type: { type: 'string', enum: ['standard', 'normal', 'emergency'] }
      },
      required: ['title', 'component', 'priority']
    }
  },
  {
    name: 'assess_risk',
    description: 'Perform risk assessment for a change',
    inputSchema: {
      type: 'object',
      properties: {
        change_id: { type: 'string' },
        impact_analysis: { type: 'boolean' }
      },
      required: ['change_id']
    }
  },
  {
    name: 'deploy_to_staging',
    description: 'Deploy change to staging environment',
    inputSchema: {
      type: 'object',
      properties: {
        change_id: { type: 'string' },
        version: { type: 'string' }
      },
      required: ['change_id', 'version']
    }
  },
  {
    name: 'deploy_to_production',
    description: 'Deploy change to production environment',
    inputSchema: {
      type: 'object',
      properties: {
        change_id: { type: 'string' },
        version: { type: 'string' },
        maintenance_window: { type: 'string' }
      },
      required: ['change_id', 'version']
    }
  },
  {
    name: 'rollback',
    description: 'Rollback a deployment',
    inputSchema: {
      type: 'object',
      properties: {
        environment: { type: 'string', enum: ['staging', 'production'] },
        target_version: { type: 'string' }
      },
      required: ['environment']
    }
  },
  {
    name: 'check_health',
    description: 'Check system health',
    inputSchema: {
      type: 'object',
      properties: {
        environment: { type: 'string', enum: ['staging', 'production'] }
      }
    }
  },
  {
    name: 'simulate_chaos',
    description: 'Run chaos engineering scenario',
    inputSchema: {
      type: 'object',
      properties: {
        scenario: { type: 'string', enum: ['friday_disaster', 'cascade_failure', 'peak_load'] },
        duration_hours: { type: 'number', default: 24 }
      },
      required: ['scenario']
    }
  },
  {
    name: 'get_metrics',
    description: 'Get deployment metrics',
    inputSchema: {
      type: 'object',
      properties: {
        environment: { type: 'string' },
        time_range: { type: 'string', default: '1h' }
      }
    }
  }
];

// MCP Protocol Handlers
const handleInitialize = (params) => {
  return {
    protocolVersion: '1.0.0',
    capabilities: {
      tools: { listChanged: true },
      resources: { subscribe: false },
      prompts: { listChanged: false },
      logging: {}
    },
    serverInfo: {
      name: 'Guile ChangeFlow MCP Server',
      version: config.version
    }
  };
};

const handleToolsList = () => {
  return { tools };
};

const handleToolCall = async (params) => {
  const { name, arguments: args } = params;

  logger.info(`Tool call: ${name}`, { args });

  // In remote mode, forward to actual server
  if (config.mode === 'remote') {
    try {
      const response = await fetch(`${config.remoteUrl}/mcp`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          jsonrpc: '2.0',
          method: 'tools/call',
          params: { name, arguments: args },
          id: Date.now()
        })
      });

      const result = await response.json();
      return result.result || result;
    } catch (error) {
      logger.error('Remote call failed:', error);
      throw error;
    }
  }

  // Local mode - simulate responses
  switch (name) {
    case 'create_change_request':
      return {
        change_id: `CHG-${Date.now()}`,
        status: 'created',
        approval_required: args.change_type !== 'standard'
      };

    case 'assess_risk':
      return {
        risk_level: 'medium',
        failure_probability: 0.12,
        impact_score: 3,
        recommendations: ['Run staging tests', 'Prepare rollback plan']
      };

    case 'check_health':
      return {
        status: 'healthy',
        version: config.version,
        uptime: process.uptime(),
        environment: args.environment || 'local'
      };

    case 'simulate_chaos':
      return {
        scenario: args.scenario,
        results: {
          total_changes: 100,
          successful: 96,
          failed: 4,
          rollbacks: 2
        }
      };

    case 'get_metrics':
      return {
        environment: args.environment || 'local',
        metrics: {
          deployment_frequency: 3.2,
          lead_time: '2.5 hours',
          mttr: '22 minutes',
          change_failure_rate: 0.042
        }
      };

    default:
      throw new Error(`Unknown tool: ${name}`);
  }
};

// HTTP Endpoints
app.post('/mcp', async (req, res) => {
  const { method, params, id } = req.body;

  try {
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
        throw new Error(`Unknown method: ${method}`);
    }

    res.json({
      jsonrpc: '2.0',
      result,
      id
    });
  } catch (error) {
    res.json({
      jsonrpc: '2.0',
      error: {
        code: -32603,
        message: error.message
      },
      id
    });
  }
});

app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    version: config.version,
    mode: config.mode,
    uptime: process.uptime()
  });
});

app.get('/version', (req, res) => {
  res.json({ version: config.version });
});

// WebSocket support for stdio transport
const server = app.listen(config.port, () => {
  console.log(`
╔══════════════════════════════════════════════════╗
║     Guile ChangeFlow MCP Server v${config.version}        ║
╠══════════════════════════════════════════════════╣
║  Mode: ${config.mode.padEnd(42)}║
║  Port: ${String(config.port).padEnd(42)}║
${config.mode === 'remote' ? `║  Remote: ${config.remoteUrl.padEnd(40)}║\n` : ''}╠══════════════════════════════════════════════════╣
║  HTTP:   http://localhost:${config.port}/mcp          ║
║  Health: http://localhost:${config.port}/health       ║
║  WS:     ws://localhost:${config.port}                ║
╚══════════════════════════════════════════════════╝

Ready to handle MCP requests!

Try:
  curl -X POST http://localhost:${config.port}/mcp \\
    -H "Content-Type: application/json" \\
    -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":1}'
`);

  logger.info('MCP Server started', { port: config.port, mode: config.mode });
});

// WebSocket server
const wss = new WebSocket.Server({ server });

wss.on('connection', (ws) => {
  logger.info('WebSocket client connected');

  ws.on('message', async (message) => {
    try {
      const request = JSON.parse(message);
      const { method, params, id } = request;

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
          throw new Error(`Unknown method: ${method}`);
      }

      ws.send(JSON.stringify({
        jsonrpc: '2.0',
        result,
        id
      }));
    } catch (error) {
      ws.send(JSON.stringify({
        jsonrpc: '2.0',
        error: {
          code: -32603,
          message: error.message
        },
        id: request.id
      }));
    }
  });

  ws.on('close', () => {
    logger.info('WebSocket client disconnected');
  });
});

// Graceful shutdown
process.on('SIGTERM', () => {
  logger.info('SIGTERM received, shutting down gracefully');
  server.close(() => {
    logger.info('Server closed');
    process.exit(0);
  });
});

// Handle stdio mode for MCP
if (process.argv.includes('--stdio')) {
  // Switch to stdio mode for direct MCP communication
  process.stdin.setEncoding('utf8');

  process.stdin.on('data', async (data) => {
    try {
      const request = JSON.parse(data);
      const { method, params, id } = request;

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
      }

      process.stdout.write(JSON.stringify({
        jsonrpc: '2.0',
        result,
        id
      }) + '\n');
    } catch (error) {
      process.stderr.write(`Error: ${error.message}\n`);
    }
  });
}