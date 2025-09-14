#!/usr/bin/env node

/**
 * Local testing script for MCP Server
 * Run with: node test-local.js
 */

const worker = require('./worker.js').default;

// Mock environment
const env = {
  ENVIRONMENT: 'development',
  LOG_LEVEL: 'debug'
};

// Mock context
const ctx = {
  waitUntil: (promise) => promise
};

// Test cases
const tests = [
  {
    name: 'Health Check',
    request: new Request('http://localhost/health')
  },
  {
    name: 'MCP Discovery',
    request: new Request('http://localhost/mcp')
  },
  {
    name: 'List Tools',
    request: new Request('http://localhost/tools')
  },
  {
    name: 'Create Low Risk Change',
    request: new Request('http://localhost/mcp/tools/invoke', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        tool: 'create_change_request',
        params: {
          title: 'Update documentation',
          description: 'Minor documentation updates',
          systems: ['docs'],
          urgency: 'low'
        }
      })
    })
  },
  {
    name: 'Create High Risk Change',
    request: new Request('http://localhost/mcp/tools/invoke', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        tool: 'create_change_request',
        params: {
          title: 'Production security patch',
          description: 'Critical security update for production authentication system',
          systems: ['production', 'auth', 'api'],
          urgency: 'emergency'
        }
      })
    })
  },
  {
    name: 'Risk Assessment',
    request: new Request('http://localhost/mcp/tools/invoke', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        tool: 'assess_risk',
        params: {
          title: 'Database migration',
          description: 'Migrate production database to new schema',
          systems: ['database', 'api'],
          urgency: 'high'
        }
      })
    })
  },
  {
    name: 'List Changes',
    request: new Request('http://localhost/mcp/tools/invoke', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        tool: 'list_change_requests',
        params: {}
      })
    })
  }
];

// Run tests
async function runTests() {
  console.log('🧪 Running Local Tests\n');
  console.log('=' .repeat(50));

  for (const test of tests) {
    console.log(`\n📝 Test: ${test.name}`);
    console.log('-'.repeat(30));

    try {
      const response = await worker.fetch(test.request, env, ctx);
      const data = await response.json();

      console.log(`✅ Status: ${response.status}`);
      console.log('📦 Response:');
      console.log(JSON.stringify(data, null, 2));

      // Check for expected fields
      if (test.name === 'Create Low Risk Change' || test.name === 'Create High Risk Change') {
        if (data.risk_score !== undefined) {
          console.log(`🎯 Risk Score: ${data.risk_score} (${data.risk_category})`);
        }
      }
    } catch (error) {
      console.log(`❌ Error: ${error.message}`);
    }
  }

  console.log('\n' + '='.repeat(50));
  console.log('✅ All tests completed!\n');
}

// Execute tests
runTests().catch(console.error);