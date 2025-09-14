#!/usr/bin/env node

/**
 * Comprehensive MCP Implementation Test Suite
 * Tests all MCP protocol features: tools, resources, prompts, error handling
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

// Test configuration
const TEST_CONFIG = {
  localWorkerPath: path.join(__dirname, '../infra/cloudflare/worker.js'),
  remoteEndpoint: process.env.CLOUDFLARE_WORKER_URL || 'https://api.changeflow.us',
  protocolVersion: '2024-11-05',
  testTimeout: 5000
};

// Mock Cloudflare Worker globals for local testing
global.Response = class MockResponse {
  constructor(body, options = {}) {
    this.body = body;
    this.status = options.status || 200;
    this.headers = options.headers || {};
  }
  async json() { return JSON.parse(this.body); }
  async text() { return this.body; }
};

class MCPTestSuite {
  constructor() {
    this.results = { passed: 0, failed: 0, skipped: 0, tests: [] };
    this.worker = null;
  }

  async loadWorker() {
    try {
      const workerCode = fs.readFileSync(TEST_CONFIG.localWorkerPath, 'utf8');
      const moduleExports = {};
      eval(workerCode.replace('export default', 'moduleExports.default ='));
      this.worker = moduleExports.default;
      console.log('✅ Worker loaded successfully');
    } catch (error) {
      console.error('❌ Failed to load worker:', error.message);
      throw error;
    }
  }

  async runTest(name, testFn) {
    const startTime = Date.now();
    try {
      await testFn();
      const duration = Date.now() - startTime;
      console.log(`✅ ${name} (${duration}ms)`);
      this.results.passed++;
      this.results.tests.push({ name, status: 'PASSED', duration });
    } catch (error) {
      const duration = Date.now() - startTime;
      console.error(`❌ ${name} (${duration}ms): ${error.message}`);
      this.results.failed++;
      this.results.tests.push({ name, status: 'FAILED', duration, error: error.message });
    }
  }

  async makeRequest(method, params = {}, id = 1) {
    const request = {
      method: 'POST',
      json: async () => ({
        jsonrpc: '2.0',
        method,
        params,
        id
      })
    };
    return await this.worker.fetch(request, {}, {});
  }

  async testHealthCheck() {
    const response = await this.worker.fetch({ method: 'GET' }, {}, {});
    const result = await response.json();

    if (!result.status || result.status !== 'healthy') {
      throw new Error('Health check failed');
    }
    if (!result.version || !result.timestamp) {
      throw new Error('Missing health check fields');
    }
  }

  async testInitialize() {
    const response = await this.makeRequest('initialize', {
      protocolVersion: TEST_CONFIG.protocolVersion,
      capabilities: {}
    });
    const result = await response.json();

    if (result.result.protocolVersion !== TEST_CONFIG.protocolVersion) {
      throw new Error('Protocol version mismatch');
    }
    if (!result.result.capabilities.tools || !result.result.capabilities.resources || !result.result.capabilities.prompts) {
      throw new Error('Missing capabilities');
    }
    if (!result.result.serverInfo.name || !result.result.serverInfo.version) {
      throw new Error('Missing server info');
    }
  }

  async testToolsList() {
    const response = await this.makeRequest('tools/list');
    const result = await response.json();

    if (!Array.isArray(result.result.tools)) {
      throw new Error('Tools should be an array');
    }
    if (result.result.tools.length !== 8) {
      throw new Error(`Expected 8 tools, got ${result.result.tools.length}`);
    }

    const expectedTools = [
      'create_change_request', 'assess_change_risk', 'check_freeze_period',
      'get_cab_members', 'schedule_change', 'create_emergency_change',
      'get_change_metrics', 'generate_audit_report'
    ];

    for (const toolName of expectedTools) {
      const tool = result.result.tools.find(t => t.name === toolName);
      if (!tool) {
        throw new Error(`Missing tool: ${toolName}`);
      }
      if (!tool.description || !tool.inputSchema) {
        throw new Error(`Tool ${toolName} missing description or schema`);
      }
    }
  }

  async testToolExecution() {
    // Test each tool with valid inputs
    const toolTests = [
      {
        name: 'create_change_request',
        args: {
          title: 'Test Change',
          description: 'Test change description',
          risk_level: 'low',
          environment: 'test'
        },
        expectedFields: ['change_id', 'status', 'risk_score']
      },
      {
        name: 'assess_change_risk',
        args: {
          change_type: 'infrastructure',
          environment: 'production',
          components_affected: 3,
          has_rollback: true,
          tested_in_staging: false
        },
        expectedFields: ['risk_score', 'risk_level', 'factors']
      },
      {
        name: 'check_freeze_period',
        args: {
          planned_start: '2025-09-15T08:00:00Z',
          planned_end: '2025-09-15T09:00:00Z',
          environment: 'production'
        },
        expectedFields: ['is_freeze_period']
      },
      {
        name: 'create_emergency_change',
        args: {
          title: 'Emergency Security Patch',
          description: 'Critical vulnerability fix',
          justification: 'Active exploitation detected',
          impact: 'Service restart required',
          environment: 'production',
          requester: 'security@company.com'
        },
        expectedFields: ['change_id', 'type', 'status']
      }
    ];

    for (const test of toolTests) {
      const response = await this.makeRequest('tools/call', {
        name: test.name,
        arguments: test.args
      });
      const result = await response.json();

      if (result.result.error) {
        throw new Error(`Tool ${test.name} returned error: ${result.result.error}`);
      }

      for (const field of test.expectedFields) {
        if (!(field in result.result)) {
          throw new Error(`Tool ${test.name} missing field: ${field}`);
        }
      }
    }
  }

  async testResourcesList() {
    const response = await this.makeRequest('resources/list');
    const result = await response.json();

    if (!Array.isArray(result.result.resources)) {
      throw new Error('Resources should be an array');
    }
    if (result.result.resources.length !== 5) {
      throw new Error(`Expected 5 resources, got ${result.result.resources.length}`);
    }

    const expectedResources = [
      'changeflow://config/change-types',
      'changeflow://config/approval-matrix',
      'changeflow://config/freeze-calendar',
      'changeflow://templates/emergency-change',
      'changeflow://docs/itil-compliance'
    ];

    for (const resourceUri of expectedResources) {
      const resource = result.result.resources.find(r => r.uri === resourceUri);
      if (!resource) {
        throw new Error(`Missing resource: ${resourceUri}`);
      }
      if (!resource.name || !resource.description || !resource.mimeType) {
        throw new Error(`Resource ${resourceUri} missing required fields`);
      }
    }
  }

  async testResourceRead() {
    const testUris = [
      'changeflow://config/change-types',
      'changeflow://config/approval-matrix',
      'changeflow://docs/itil-compliance'
    ];

    for (const uri of testUris) {
      const response = await this.makeRequest('resources/read', { uri });
      const result = await response.json();

      if (result.result.error) {
        throw new Error(`Resource read failed for ${uri}: ${result.result.error}`);
      }
      if (!result.result.contents || !Array.isArray(result.result.contents)) {
        throw new Error(`Resource ${uri} should return contents array`);
      }
      if (result.result.contents.length === 0) {
        throw new Error(`Resource ${uri} returned empty contents`);
      }

      const content = result.result.contents[0];
      if (!content.uri || !content.mimeType || !content.text) {
        throw new Error(`Resource ${uri} content missing required fields`);
      }
    }
  }

  async testPromptsList() {
    const response = await this.makeRequest('prompts/list');
    const result = await response.json();

    if (!Array.isArray(result.result.prompts)) {
      throw new Error('Prompts should be an array');
    }
    if (result.result.prompts.length !== 3) {
      throw new Error(`Expected 3 prompts, got ${result.result.prompts.length}`);
    }

    const expectedPrompts = [
      'analyze-change-risk',
      'generate-rollback-plan',
      'create-change-summary'
    ];

    for (const promptName of expectedPrompts) {
      const prompt = result.result.prompts.find(p => p.name === promptName);
      if (!prompt) {
        throw new Error(`Missing prompt: ${promptName}`);
      }
      if (!prompt.description || !Array.isArray(prompt.arguments)) {
        throw new Error(`Prompt ${promptName} missing description or arguments`);
      }
    }
  }

  async testPromptGet() {
    const testPrompts = [
      {
        name: 'analyze-change-risk',
        args: {
          change_description: 'Database schema update',
          environment: 'production'
        }
      },
      {
        name: 'generate-rollback-plan',
        args: {
          change_details: 'Microservice deployment',
          system_context: 'Kubernetes cluster'
        }
      }
    ];

    for (const test of testPrompts) {
      const response = await this.makeRequest('prompts/get', {
        name: test.name,
        arguments: test.args
      });
      const result = await response.json();

      if (result.result.error) {
        throw new Error(`Prompt get failed for ${test.name}: ${result.result.error}`);
      }
      if (!result.result.description || !Array.isArray(result.result.messages)) {
        throw new Error(`Prompt ${test.name} missing description or messages`);
      }
      if (result.result.messages.length === 0) {
        throw new Error(`Prompt ${test.name} returned no messages`);
      }

      const message = result.result.messages[0];
      if (!message.role || !message.content || !message.content.text) {
        throw new Error(`Prompt ${test.name} message missing required fields`);
      }

      // Verify arguments were interpolated
      for (const [key, value] of Object.entries(test.args)) {
        if (!message.content.text.includes(value)) {
          throw new Error(`Prompt ${test.name} didn't interpolate argument ${key}`);
        }
      }
    }
  }

  async testErrorHandling() {
    // Test invalid method
    const invalidMethodResponse = await this.makeRequest('invalid/method');
    const invalidMethodResult = await invalidMethodResponse.json();
    if (!invalidMethodResult.result.error || invalidMethodResult.result.error.code !== -32601) {
      throw new Error('Should return method not found error');
    }

    // Test invalid tool
    const invalidToolResponse = await this.makeRequest('tools/call', {
      name: 'invalid_tool',
      arguments: {}
    });
    const invalidToolResult = await invalidToolResponse.json();
    if (!invalidToolResult.result.error) {
      throw new Error('Should return error for invalid tool');
    }

    // Test invalid resource
    const invalidResourceResponse = await this.makeRequest('resources/read', {
      uri: 'changeflow://invalid/resource'
    });
    const invalidResourceResult = await invalidResourceResponse.json();
    if (!invalidResourceResult.result.error) {
      throw new Error('Should return error for invalid resource');
    }

    // Test invalid prompt
    const invalidPromptResponse = await this.makeRequest('prompts/get', {
      name: 'invalid-prompt'
    });
    const invalidPromptResult = await invalidPromptResponse.json();
    if (!invalidPromptResult.result.error) {
      throw new Error('Should return error for invalid prompt');
    }
  }

  async testPerformance() {
    const iterations = 10;
    const tests = [
      () => this.makeRequest('tools/list'),
      () => this.makeRequest('resources/list'),
      () => this.makeRequest('prompts/list'),
      () => this.makeRequest('tools/call', {
        name: 'assess_change_risk',
        arguments: {
          change_type: 'config',
          environment: 'test'
        }
      })
    ];

    for (const test of tests) {
      const times = [];
      for (let i = 0; i < iterations; i++) {
        const start = Date.now();
        await test();
        times.push(Date.now() - start);
      }
      const avgTime = times.reduce((a, b) => a + b, 0) / times.length;
      const maxTime = Math.max(...times);

      if (avgTime > 100) {
        console.warn(`⚠️  Average response time ${avgTime}ms exceeds 100ms target`);
      }
      if (maxTime > 500) {
        throw new Error(`Max response time ${maxTime}ms exceeds 500ms limit`);
      }
    }
  }

  async runAllTests() {
    console.log('🧪 Starting Comprehensive MCP Test Suite');
    console.log('==========================================');

    await this.loadWorker();

    // Core protocol tests
    await this.runTest('Health Check', () => this.testHealthCheck());
    await this.runTest('MCP Initialize', () => this.testInitialize());

    // Tools tests
    await this.runTest('Tools List', () => this.testToolsList());
    await this.runTest('Tool Execution', () => this.testToolExecution());

    // Resources tests
    await this.runTest('Resources List', () => this.testResourcesList());
    await this.runTest('Resource Read', () => this.testResourceRead());

    // Prompts tests
    await this.runTest('Prompts List', () => this.testPromptsList());
    await this.runTest('Prompt Get', () => this.testPromptGet());

    // Error handling tests
    await this.runTest('Error Handling', () => this.testErrorHandling());

    // Performance tests
    await this.runTest('Performance', () => this.testPerformance());

    this.printSummary();
    return this.results.failed === 0;
  }

  printSummary() {
    console.log('\n📊 Test Results Summary');
    console.log('=======================');
    console.log(`✅ Passed: ${this.results.passed}`);
    console.log(`❌ Failed: ${this.results.failed}`);
    console.log(`⏭️  Skipped: ${this.results.skipped}`);
    console.log(`📈 Total: ${this.results.passed + this.results.failed + this.results.skipped}`);

    if (this.results.failed > 0) {
      console.log('\n❌ Failed Tests:');
      this.results.tests
        .filter(t => t.status === 'FAILED')
        .forEach(t => console.log(`   - ${t.name}: ${t.error}`));
    }

    const avgDuration = this.results.tests.reduce((sum, t) => sum + t.duration, 0) / this.results.tests.length;
    console.log(`⏱️  Average test duration: ${Math.round(avgDuration)}ms`);

    console.log(`\n${this.results.failed === 0 ? '🎉' : '💥'} Test suite ${this.results.failed === 0 ? 'PASSED' : 'FAILED'}`);
  }
}

// Run tests if called directly
if (require.main === module) {
  const suite = new MCPTestSuite();
  suite.runAllTests()
    .then(success => process.exit(success ? 0 : 1))
    .catch(error => {
      console.error('💥 Test suite crashed:', error);
      process.exit(1);
    });
}

module.exports = MCPTestSuite;