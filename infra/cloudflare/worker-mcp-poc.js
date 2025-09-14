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

// Available MCP Resources
const RESOURCES = [
  {
    uri: 'changeflow://config/change-types',
    name: 'Change Type Definitions',
    description: 'Standard change types and their risk profiles',
    mimeType: 'application/json'
  },
  {
    uri: 'changeflow://config/approval-matrix',
    name: 'Approval Matrix',
    description: 'CAB approval requirements by risk level',
    mimeType: 'application/json'
  },
  {
    uri: 'changeflow://config/freeze-calendar',
    name: 'Freeze Period Calendar',
    description: 'Scheduled maintenance windows and freeze periods',
    mimeType: 'application/json'
  },
  {
    uri: 'changeflow://templates/emergency-change',
    name: 'Emergency Change Template',
    description: 'Template for emergency change requests',
    mimeType: 'application/json'
  },
  {
    uri: 'changeflow://docs/itil-compliance',
    name: 'ITIL Compliance Guide',
    description: 'ITIL 4 compliance requirements and procedures',
    mimeType: 'text/markdown'
  }
];

// Available MCP Prompts
const PROMPTS = [
  {
    name: 'analyze-change-risk',
    description: 'Analyze the risk factors of a proposed change',
    arguments: [
      {
        name: 'change_description',
        description: 'Description of the proposed change',
        required: true
      },
      {
        name: 'environment',
        description: 'Target environment (dev/test/staging/production)',
        required: true
      }
    ]
  },
  {
    name: 'generate-rollback-plan',
    description: 'Generate a rollback plan for a change',
    arguments: [
      {
        name: 'change_details',
        description: 'Detailed change information',
        required: true
      },
      {
        name: 'system_context',
        description: 'System and infrastructure context',
        required: false
      }
    ]
  },
  {
    name: 'create-change-summary',
    description: 'Create executive summary for change approval',
    arguments: [
      {
        name: 'change_request',
        description: 'Full change request details',
        required: true
      },
      {
        name: 'audience',
        description: 'Target audience (technical/executive/operational)',
        required: true
      }
    ]
  }
];

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
  {
    name: 'check_freeze_period',
    description: 'Check if a change window falls within a freeze period',
    inputSchema: {
      type: 'object',
      properties: {
        planned_start: { type: 'string', format: 'date-time' },
        planned_end: { type: 'string', format: 'date-time' },
        environment: { type: 'string', enum: ['dev', 'test', 'staging', 'production'] }
      },
      required: ['planned_start', 'planned_end', 'environment']
    }
  },
  {
    name: 'get_cab_members',
    description: 'Get Change Advisory Board members for approval workflow',
    inputSchema: {
      type: 'object',
      properties: {
        risk_level: { type: 'string', enum: ['low', 'medium', 'high', 'critical'] },
        environment: { type: 'string', enum: ['dev', 'test', 'staging', 'production'] }
      },
      required: ['risk_level']
    }
  },
  {
    name: 'schedule_change',
    description: 'Schedule a change request for execution',
    inputSchema: {
      type: 'object',
      properties: {
        change_id: { type: 'string' },
        scheduled_start: { type: 'string', format: 'date-time' },
        scheduled_end: { type: 'string', format: 'date-time' },
        notification_contacts: { type: 'array', items: { type: 'string' } }
      },
      required: ['change_id', 'scheduled_start', 'scheduled_end']
    }
  },
  {
    name: 'create_emergency_change',
    description: 'Create an emergency change request with expedited approval',
    inputSchema: {
      type: 'object',
      properties: {
        title: { type: 'string' },
        description: { type: 'string' },
        justification: { type: 'string' },
        impact: { type: 'string' },
        environment: { type: 'string', enum: ['dev', 'test', 'staging', 'production'] },
        requester: { type: 'string' }
      },
      required: ['title', 'description', 'justification', 'impact', 'environment', 'requester']
    }
  },
  {
    name: 'get_change_metrics',
    description: 'Get change management metrics and statistics',
    inputSchema: {
      type: 'object',
      properties: {
        time_period: { type: 'string', enum: ['week', 'month', 'quarter'] },
        environment: { type: 'string', enum: ['all', 'dev', 'test', 'staging', 'production'] },
        metric_type: { type: 'string', enum: ['success_rate', 'rollback_rate', 'avg_duration', 'all'] }
      },
      required: ['time_period']
    }
  },
  {
    name: 'generate_audit_report',
    description: 'Generate compliance audit report for change management',
    inputSchema: {
      type: 'object',
      properties: {
        report_type: { type: 'string', enum: ['compliance', 'risk_analysis', 'performance'] },
        date_range: {
          type: 'object',
          properties: {
            start: { type: 'string', format: 'date' },
            end: { type: 'string', format: 'date' }
          },
          required: ['start', 'end']
        },
        include_details: { type: 'boolean' }
      },
      required: ['report_type', 'date_range']
    }
  }
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
      case 'resources/list':
        result = handleResourcesList();
        break;
      case 'resources/read':
        result = await handleResourceRead(params);
        break;
      case 'prompts/list':
        result = handlePromptsList();
        break;
      case 'prompts/get':
        result = await handlePromptGet(params);
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
      resources: {},
      prompts: {},
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

    case 'check_freeze_period':
      return checkFreezePeriod(args);

    case 'get_cab_members':
      return getCabMembers(args);

    case 'schedule_change':
      return scheduleChange(args);

    case 'create_emergency_change':
      return createEmergencyChange(args);

    case 'get_change_metrics':
      return getChangeMetrics(args);

    case 'generate_audit_report':
      return generateAuditReport(args);

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

/**
 * Check if change window conflicts with freeze periods
 */
function checkFreezePeriod(args) {
  const { planned_start, planned_end, environment } = args;

  // Mock freeze periods - in production would check against calendar/config
  const freezePeriods = [
    { start: '2025-12-20', end: '2025-01-05', reason: 'Holiday freeze' },
    { start: '2025-11-28', end: '2025-11-29', reason: 'Thanksgiving freeze' }
  ];

  const start = new Date(planned_start);
  const end = new Date(planned_end);

  for (const freeze of freezePeriods) {
    const freezeStart = new Date(freeze.start);
    const freezeEnd = new Date(freeze.end);

    if ((start >= freezeStart && start <= freezeEnd) ||
        (end >= freezeStart && end <= freezeEnd)) {
      return {
        is_freeze_period: true,
        freeze_reason: freeze.reason,
        freeze_start: freeze.start,
        freeze_end: freeze.end,
        recommendation: 'Schedule change outside freeze window'
      };
    }
  }

  return {
    is_freeze_period: false,
    message: 'No conflicts with freeze periods',
    next_freeze: freezePeriods[0]
  };
}

/**
 * Get CAB members for approval workflow
 */
function getCabMembers(args) {
  const { risk_level, environment } = args;

  const cabMembers = {
    low: ['tech-lead@company.com'],
    medium: ['tech-lead@company.com', 'ops-manager@company.com'],
    high: ['tech-lead@company.com', 'ops-manager@company.com', 'cto@company.com'],
    critical: ['tech-lead@company.com', 'ops-manager@company.com', 'cto@company.com', 'ceo@company.com']
  };

  const members = cabMembers[risk_level] || cabMembers.medium;

  return {
    approvers: members,
    approval_threshold: risk_level === 'critical' ? members.length : Math.ceil(members.length / 2),
    sla_hours: risk_level === 'critical' ? 2 : risk_level === 'high' ? 8 : 24,
    escalation_contact: 'escalation@company.com'
  };
}

/**
 * Schedule a change for execution
 */
function scheduleChange(args) {
  const { change_id, scheduled_start, scheduled_end, notification_contacts } = args;

  return {
    schedule_id: `SCHED-${Date.now()}`,
    change_id,
    status: 'scheduled',
    scheduled_start,
    scheduled_end,
    notifications: {
      contacts: notification_contacts || [],
      reminders: [
        { type: 'email', timing: '24h_before' },
        { type: 'email', timing: '1h_before' },
        { type: 'sms', timing: '15m_before' }
      ]
    },
    automation_hooks: {
      pre_change: 'backup_services',
      post_change: 'validate_deployment',
      rollback: 'restore_from_backup'
    }
  };
}

/**
 * Create emergency change with expedited approval
 */
function createEmergencyChange(args) {
  const { title, description, justification, impact, environment, requester } = args;

  const emergencyChangeId = `EMG-${Date.now()}`;

  return {
    change_id: emergencyChangeId,
    type: 'emergency',
    status: 'pending_emergency_approval',
    title,
    description,
    justification,
    impact,
    environment,
    requester,
    risk_level: 'high', // Emergency changes are automatically high risk
    approval_required: environment === 'production',
    emergency_contacts: [
      'oncall@company.com',
      'incident-commander@company.com'
    ],
    sla_approval_minutes: 30,
    auto_rollback_enabled: true,
    created_at: new Date().toISOString()
  };
}

/**
 * Get change management metrics
 */
function getChangeMetrics(args) {
  const { time_period, environment, metric_type } = args;

  // Mock metrics - in production would query from database
  const mockMetrics = {
    success_rate: {
      week: 94.2,
      month: 92.8,
      quarter: 91.5
    },
    rollback_rate: {
      week: 5.8,
      month: 7.2,
      quarter: 8.5
    },
    avg_duration: {
      week: 45,
      month: 52,
      quarter: 48
    }
  };

  const result = {
    time_period,
    environment: environment || 'all',
    period_start: new Date(Date.now() - (time_period === 'week' ? 7 : time_period === 'month' ? 30 : 90) * 24 * 60 * 60 * 1000).toISOString(),
    period_end: new Date().toISOString()
  };

  if (metric_type === 'all' || !metric_type) {
    result.metrics = {
      success_rate: mockMetrics.success_rate[time_period],
      rollback_rate: mockMetrics.rollback_rate[time_period],
      avg_duration_minutes: mockMetrics.avg_duration[time_period],
      total_changes: time_period === 'week' ? 17 : time_period === 'month' ? 84 : 267
    };
  } else {
    result.metrics = {
      [metric_type]: mockMetrics[metric_type][time_period]
    };
  }

  return result;
}

/**
 * Generate audit report for compliance
 */
function generateAuditReport(args) {
  const { report_type, date_range, include_details } = args;

  const reportId = `RPT-${Date.now()}`;

  return {
    report_id: reportId,
    report_type,
    generated_at: new Date().toISOString(),
    date_range,
    summary: {
      total_changes: 156,
      successful_changes: 144,
      failed_changes: 8,
      rolled_back: 4,
      compliance_score: 94.2
    },
    findings: [
      'All high-risk changes had proper CAB approval',
      '2 changes lacked adequate testing documentation',
      'Emergency change procedures followed correctly',
      'Freeze period violations: 0'
    ],
    recommendations: [
      'Improve testing documentation standards',
      'Consider automated testing gates',
      'Review change categorization process'
    ],
    details_included: include_details || false,
    compliance_status: 'COMPLIANT',
    next_audit_due: new Date(Date.now() + 90 * 24 * 60 * 60 * 1000).toISOString()
  };
}

/**
 * Handle resources/list request
 */
function handleResourcesList() {
  return { resources: RESOURCES };
}

/**
 * Handle resources/read request
 */
async function handleResourceRead(params) {
  const { uri } = params;

  // Mock resource data - in production would fetch from actual sources
  const resourceData = {
    'changeflow://config/change-types': {
      standard: { risk_base: 10, approval_required: false },
      normal: { risk_base: 30, approval_required: true },
      emergency: { risk_base: 80, approval_required: true, expedited: true }
    },
    'changeflow://config/approval-matrix': {
      low: ['tech-lead@company.com'],
      medium: ['tech-lead@company.com', 'ops-manager@company.com'],
      high: ['tech-lead@company.com', 'ops-manager@company.com', 'cto@company.com'],
      critical: ['tech-lead@company.com', 'ops-manager@company.com', 'cto@company.com', 'ceo@company.com']
    },
    'changeflow://config/freeze-calendar': {
      freeze_periods: [
        { start: '2025-12-20', end: '2025-01-05', reason: 'Holiday freeze' },
        { start: '2025-11-28', end: '2025-11-29', reason: 'Thanksgiving freeze' }
      ]
    },
    'changeflow://templates/emergency-change': {
      template: {
        title: 'Emergency Change: [Brief Description]',
        justification: 'Business Impact: [Impact Description]\\nRisk if not implemented: [Risk Description]',
        rollback_plan: 'Step 1: [Rollback step]\\nStep 2: [Verification step]',
        testing_notes: 'Pre-change validation: [Validation steps]'
      }
    },
    'changeflow://docs/itil-compliance': `# ITIL 4 Compliance Guide

## Change Enablement Practices

### Risk Assessment Requirements
- All production changes require risk assessment
- Risk factors: environment, complexity, dependencies, timing
- Scores: 0-39 (low), 40-59 (medium), 60-79 (high), 80-100 (critical)

### Approval Workflows
- Low risk: Tech lead approval
- Medium risk: Tech lead + Ops manager
- High risk: Tech lead + Ops manager + CTO
- Critical risk: Full CAB approval including CEO

### Emergency Changes
- Justification required within 30 minutes
- Post-implementation review mandatory
- Auto-rollback enabled by default`
  };

  const content = resourceData[uri];
  if (!content) {
    return { error: `Resource not found: ${uri}` };
  }

  return {
    contents: [{
      uri,
      mimeType: uri.includes('docs/') ? 'text/markdown' : 'application/json',
      text: typeof content === 'string' ? content : JSON.stringify(content, null, 2)
    }]
  };
}

/**
 * Handle prompts/list request
 */
function handlePromptsList() {
  return { prompts: PROMPTS };
}

/**
 * Handle prompts/get request
 */
async function handlePromptGet(params) {
  const { name, arguments: args } = params;

  const promptTemplates = {
    'analyze-change-risk': `Please analyze the risk factors for this proposed change:

**Change Description:** ${args?.change_description || '[Change description needed]'}
**Target Environment:** ${args?.environment || '[Environment needed]'}

Please provide:
1. Risk assessment score (0-100)
2. Key risk factors identified
3. Mitigation recommendations
4. Approval workflow recommendation

Consider factors such as:
- System criticality and dependencies
- Change complexity and scope
- Historical success rates
- Timing and operational windows
- Rollback complexity`,

    'generate-rollback-plan': `Create a detailed rollback plan for this change:

**Change Details:** ${args?.change_details || '[Change details needed]'}
**System Context:** ${args?.system_context || '[System context if available]'}

Please provide a step-by-step rollback plan including:
1. Pre-rollback validation checks
2. Rollback execution steps (in order)
3. Post-rollback verification procedures
4. Communication and notification steps
5. Estimated rollback time
6. Risk assessment of rollback procedure

Format as a checklist that operations teams can follow under pressure.`,

    'create-change-summary': `Create an executive summary for this change request:

**Change Request:** ${args?.change_request || '[Change request details needed]'}
**Target Audience:** ${args?.audience || '[Audience type needed]'}

Please create a concise summary including:
1. Business justification and value
2. Risk assessment and mitigation
3. Implementation timeline
4. Resource requirements
5. Success criteria and rollback plan
6. Approval recommendations

Tailor the language and technical depth for the specified audience.`
  };

  const template = promptTemplates[name];
  if (!template) {
    return { error: `Prompt not found: ${name}` };
  }

  return {
    description: PROMPTS.find(p => p.name === name)?.description || 'Prompt description',
    messages: [{
      role: 'user',
      content: {
        type: 'text',
        text: template
      }
    }]
  };
}

// TODO: Add persistence (KV or D1)
// TODO: Add authentication
// TODO: Add rate limiting