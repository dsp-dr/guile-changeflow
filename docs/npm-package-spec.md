# NPM Package Specification: @changeflow/mcp-server

## Package Overview

Publish the working JavaScript MCP server implementation as a standalone npm package that developers can run locally for ITIL change management integration with Claude Code and other AI tools.

## Package Structure

```
@changeflow/mcp-server/
├── package.json
├── README.md
├── LICENSE (MIT)
├── bin/
│   └── changeflow-mcp         # CLI executable
├── src/
│   ├── index.js               # Main entry point
│   ├── server.js              # MCP server implementation
│   ├── tools/                 # ITIL tools
│   │   ├── create-change.js
│   │   ├── assess-risk.js
│   │   ├── check-freeze.js
│   │   ├── get-cab.js
│   │   ├── schedule-change.js
│   │   ├── emergency-change.js
│   │   ├── get-metrics.js
│   │   └── audit-report.js
│   ├── resources/             # MCP resources
│   │   ├── approval-matrix.js
│   │   ├── risk-factors.js
│   │   └── documentation.js
│   ├── prompts/              # MCP prompts
│   │   └── index.js
│   └── utils/
│       ├── risk-calculator.js
│       ├── validation.js
│       └── mock-data.js
├── config/
│   └── default.json          # Default configuration
├── examples/
│   ├── claude-code-setup.md
│   ├── basic-usage.js
│   └── mcp-config.json
└── test/
    └── integration.test.js

```

## Implementation Plan

### Phase 1: Extract and Modularize (Day 1)

**Extract from current monolith:**
```javascript
// Current: infra/cloudflare/worker.js (787 lines)
// Split into modular structure above

// src/index.js
export { MCPServer } from './server.js';
export * as tools from './tools/index.js';
export * as resources from './resources/index.js';

// bin/changeflow-mcp (CLI entry point)
#!/usr/bin/env node
import { MCPServer } from '../src/index.js';

const server = new MCPServer({
  port: process.env.MCP_PORT || 8427,
  host: process.env.MCP_HOST || 'localhost'
});

server.start();
```

### Phase 2: Package Configuration (Day 1)

**package.json:**
```json
{
  "name": "@changeflow/mcp-server",
  "version": "1.0.0",
  "description": "ITIL Change Management MCP Server for AI-assisted infrastructure changes",
  "keywords": ["mcp", "itil", "change-management", "claude-code", "ai-tools"],
  "main": "src/index.js",
  "type": "module",
  "bin": {
    "changeflow-mcp": "./bin/changeflow-mcp"
  },
  "scripts": {
    "start": "node bin/changeflow-mcp",
    "test": "node --test test/*.test.js",
    "dev": "node --watch bin/changeflow-mcp"
  },
  "engines": {
    "node": ">=18.0.0"
  },
  "dependencies": {
    "express": "^4.18.0",
    "cors": "^2.8.5",
    "dotenv": "^16.0.0"
  },
  "devDependencies": {
    "@types/node": "^20.0.0"
  },
  "repository": {
    "type": "git",
    "url": "https://github.com/dsp-dr/guile-changeflow.git"
  },
  "author": "DefRecord Team",
  "license": "MIT"
}
```

### Phase 3: CLI and Configuration (Day 2)

**CLI Features:**
```bash
# Install globally
npm install -g @changeflow/mcp-server

# Run with defaults (port 8427)
changeflow-mcp

# Run with custom port
changeflow-mcp --port 3000

# Run with config file
changeflow-mcp --config ./my-config.json

# Generate MCP config for Claude Code
changeflow-mcp init

# Run in development mode with hot reload
changeflow-mcp dev

# Health check
changeflow-mcp health
```

**Configuration Options:**
```javascript
// config/default.json
{
  "server": {
    "port": 8427,
    "host": "localhost",
    "cors": {
      "enabled": true,
      "origins": ["*"]
    }
  },
  "itil": {
    "riskThresholds": {
      "low": 30,
      "medium": 50,
      "high": 70
    },
    "approvalMatrix": {
      "standard": ["auto"],
      "normal": ["peer", "lead"],
      "emergency": ["lead", "cab", "cto"]
    },
    "freezePeriods": [
      {
        "name": "Holiday Freeze",
        "start": "2025-12-20",
        "end": "2026-01-05"
      }
    ]
  },
  "features": {
    "mockData": true,
    "persistence": false,
    "authentication": false
  }
}
```

### Phase 4: Documentation and Examples (Day 2)

**README.md:**
```markdown
# @changeflow/mcp-server

ITIL Change Management MCP Server for AI-assisted infrastructure changes. Provides 8 ITIL-compliant tools for managing changes through Claude Code and other MCP-compatible AI assistants.

## Quick Start

\`\`\`bash
# Install
npm install -g @changeflow/mcp-server

# Run server
changeflow-mcp

# Server running at http://localhost:8427
\`\`\`

## Claude Code Setup

1. Install the package globally
2. Add to your Claude Code MCP config:

\`\`\`json
{
  "mcpServers": {
    "changeflow": {
      "command": "npx",
      "args": ["@changeflow/mcp-server"],
      "env": {}
    }
  }
}
\`\`\`

## Available Tools

- `create_change_request` - Create new ITIL change request
- `assess_change_risk` - Calculate risk score (0-100)
- `check_freeze_periods` - Validate deployment windows
- `get_cab_members` - Get Change Advisory Board members
- `schedule_change` - Schedule deployment time
- `create_emergency_change` - Fast-track emergency changes
- `get_change_metrics` - Dashboard metrics and KPIs
- `generate_audit_report` - Compliance audit trails

## Configuration

Create `changeflow.config.json`:

\`\`\`json
{
  "server": { "port": 8427 },
  "itil": { "riskThresholds": { "low": 30, "medium": 50, "high": 70 } }
}
\`\`\`

Run with: `changeflow-mcp --config ./changeflow.config.json`
```

### Phase 5: Testing and Validation (Day 3)

**Integration Tests:**
```javascript
// test/integration.test.js
import { test } from 'node:test';
import assert from 'node:assert';
import { MCPServer } from '../src/index.js';

test('MCP Server starts and responds to initialize', async () => {
  const server = new MCPServer({ port: 0 }); // random port
  await server.start();

  const response = await fetch(`http://localhost:${server.port}/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      jsonrpc: '2.0',
      method: 'initialize',
      params: { protocolVersion: '0.1.0' },
      id: 1
    })
  });

  const result = await response.json();
  assert.equal(result.result.protocolVersion, '0.1.0');

  await server.stop();
});

test('All 8 ITIL tools are available', async () => {
  const server = new MCPServer({ port: 0 });
  await server.start();

  const response = await fetch(`http://localhost:${server.port}/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      jsonrpc: '2.0',
      method: 'tools/list',
      params: {},
      id: 2
    })
  });

  const result = await response.json();
  assert.equal(result.result.tools.length, 8);

  await server.stop();
});
```

### Phase 6: Publishing Strategy

**NPM Publishing Steps:**

1. **Scoped Package Setup:**
```bash
# Create npm org (if needed)
npm org create changeflow

# Login to npm
npm login

# Publish public scoped package
npm publish --access public
```

2. **Version Management:**
```bash
# Semantic versioning
1.0.0 - Initial release (8 ITIL tools)
1.1.0 - Add persistence support
1.2.0 - Add authentication
2.0.0 - Breaking API changes
```

3. **GitHub Actions for Auto-Publishing:**
```yaml
# .github/workflows/npm-publish.yml
name: Publish to NPM
on:
  release:
    types: [published]

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          registry-url: 'https://registry.npmjs.org'
      - run: npm ci
      - run: npm test
      - run: npm publish
        env:
          NODE_AUTH_TOKEN: ${{secrets.NPM_TOKEN}}
```

## Usage Examples

### Basic Programmatic Usage:
```javascript
import { MCPServer } from '@changeflow/mcp-server';

const server = new MCPServer({
  port: 8427,
  config: {
    itil: {
      riskThresholds: { low: 25, medium: 50, high: 75 }
    }
  }
});

await server.start();
console.log('MCP Server running on port 8427');

// Custom tool handler
server.registerTool('custom_tool', async (params) => {
  return { status: 'success', data: params };
});
```

### Claude Code Integration:
```json
// ~/.claude/mcp_config.json
{
  "mcpServers": {
    "changeflow": {
      "command": "npx",
      "args": ["@changeflow/mcp-server", "--port", "8427"],
      "env": {
        "CHANGEFLOW_ENV": "development"
      }
    }
  }
}
```

## Benefits of NPM Package

1. **Easy Installation**: Single `npm install` command
2. **Version Control**: Semantic versioning for stability
3. **Dependency Management**: Automatic dependency resolution
4. **Global CLI**: System-wide `changeflow-mcp` command
5. **Programmatic Usage**: Import as library in other projects
6. **Testing**: Standardized test suite
7. **Documentation**: Centralized on npmjs.com
8. **Updates**: Easy updates via `npm update`

## Migration from PRs

Since you have 5 large PRs with significant additions:
- PR #14: Extract core models into `src/models/`
- PR #15: Already an MCP server - use as base
- PR #16: Risk engine goes into `src/utils/risk-calculator.js`
- PR #17: Web interface could be separate package `@changeflow/web-ui`
- PR #18: Integration tests go into `test/`

## Next Steps

1. **Extract Working Code**: Pull the functional JS from `infra/cloudflare/worker.js`
2. **Modularize**: Split into logical modules per structure above
3. **Add CLI**: Create `bin/changeflow-mcp` executable
4. **Test Locally**: Ensure it works with `npm link`
5. **Publish Beta**: Release as `@changeflow/mcp-server@1.0.0-beta.1`
6. **Gather Feedback**: Test with real Claude Code users
7. **Stable Release**: Publish 1.0.0 after validation

## Why Not Scheme Version?

As you noted, the Scheme version is "beautiful but broken" and impractical for npm distribution because:
- Requires Guile 3.0+ runtime (not standard on most systems)
- Complex deployment for end users
- Missing imports and compilation issues
- No standard npm packaging for Scheme

The JavaScript version:
- Works today with 8 functional ITIL tools
- Standard Node.js (available everywhere)
- Easy npm distribution
- Already tested in production at api.changeflow.us
- Can run locally or deployed

## Summary

**Package**: `@changeflow/mcp-server`
**Version**: 1.0.0
**Size**: ~50KB
**Dependencies**: Minimal (express, cors, dotenv)
**Runtime**: Node.js 18+
**License**: MIT
**Timeline**: 3 days to package and publish
**Value**: First npm package for ITIL-compliant MCP server