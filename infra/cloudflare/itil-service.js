/**
 * ITIL 4 Change Management Service
 * Implements core ITIL 4 practices for change control
 */

// ITIL 4 Change Types
const ChangeTypes = {
  STANDARD: 'standard',
  NORMAL: 'normal',
  EMERGENCY: 'emergency'
};

// ITIL 4 Change States
const ChangeStates = {
  NEW: 'new',
  ASSESS: 'assess',
  AUTHORIZE: 'authorize',
  SCHEDULED: 'scheduled',
  IMPLEMENT: 'implement',
  REVIEW: 'review',
  CLOSED: 'closed',
  CANCELLED: 'cancelled'
};

// Risk Matrix (ITIL 4)
const RiskLevels = {
  LOW: 1,
  MEDIUM: 2,
  HIGH: 3,
  CRITICAL: 4
};

// Freeze Periods (common in ITIL 4)
const FREEZE_PERIODS = [
  { name: 'Black Friday', start: '2025-11-25', end: '2025-12-02' },
  { name: 'Year End', start: '2025-12-20', end: '2026-01-05' },
  { name: 'Quarterly Close', pattern: 'lastWeekOfQuarter' }
];

// Deployment Patterns (from gist concepts)
const DeploymentPatterns = {
  BIG_BANG: 'big-bang',
  BLUE_GREEN: 'blue-green',
  ROLLING: 'rolling',
  CANARY: 'canary',
  DARK_LAUNCH: 'dark-launch',
  FEATURE_FLAGS: 'feature-flags'
};

class ITILService {
  constructor(kv) {
    this.kv = kv; // Cloudflare KV namespace
  }

  /**
   * Create a new change request (RFC)
   */
  async createChangeRequest(data) {
    const changeId = `CHG-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

    const change = {
      id: changeId,
      title: data.title,
      description: data.description,
      environment: data.environment,
      changeType: data.changeType || ChangeTypes.NORMAL,
      deploymentPattern: data.deploymentPattern || DeploymentPatterns.ROLLING,
      state: ChangeStates.NEW,
      priority: data.priority || 3,
      risk: null,
      impact: data.impact || 'medium',
      urgency: data.urgency || 'medium',
      plannedStartDate: data.plannedStartDate,
      plannedEndDate: data.plannedEndDate,
      implementer: data.implementer || 'system',
      requester: data.requester || 'api',
      affectedServices: data.affectedServices || [],
      rollbackPlan: data.rollbackPlan || 'Revert to previous version',
      testPlan: data.testPlan || 'Standard test suite',
      communicationPlan: data.communicationPlan || 'Notify stakeholders via email',
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
      approvals: [],
      comments: [],
      auditLog: [
        {
          timestamp: new Date().toISOString(),
          user: 'system',
          action: 'CREATED',
          details: 'Change request created'
        }
      ]
    };

    // Store in KV
    if (this.kv) {
      await this.kv.put(changeId, JSON.stringify(change), {
        metadata: { type: 'change', state: change.state }
      });

      // Update index
      await this.updateChangeIndex(changeId, change.state);
    }

    return change;
  }

  /**
   * Assess risk for a change request
   */
  async assessRisk(changeId) {
    const change = await this.getChange(changeId);
    if (!change) throw new Error('Change not found');

    // Risk calculation based on ITIL 4 practices
    const factors = {
      environment: {
        production: 3,
        staging: 2,
        development: 1
      },
      changeType: {
        emergency: 4,
        normal: 2,
        standard: 1
      },
      deploymentPattern: {
        'big-bang': 4,
        'rolling': 2,
        'blue-green': 2,
        'canary': 1,
        'dark-launch': 1,
        'feature-flags': 1
      },
      affectedServices: change.affectedServices?.length || 1
    };

    let riskScore = 0;
    riskScore += factors.environment[change.environment] || 2;
    riskScore += factors.changeType[change.changeType] || 2;
    riskScore += factors.deploymentPattern[change.deploymentPattern] || 2;
    riskScore += Math.min(factors.affectedServices, 3);

    let riskLevel;
    if (riskScore <= 4) riskLevel = 'LOW';
    else if (riskScore <= 7) riskLevel = 'MEDIUM';
    else if (riskScore <= 10) riskLevel = 'HIGH';
    else riskLevel = 'CRITICAL';

    const riskAssessment = {
      score: riskScore,
      level: riskLevel,
      factors: {
        environment: change.environment,
        changeType: change.changeType,
        deploymentPattern: change.deploymentPattern,
        affectedServices: change.affectedServices
      },
      mitigations: this.getRiskMitigations(riskLevel),
      assessedAt: new Date().toISOString()
    };

    // Update change with risk assessment
    change.risk = riskAssessment;
    change.state = ChangeStates.ASSESS;
    change.updatedAt = new Date().toISOString();
    change.auditLog.push({
      timestamp: new Date().toISOString(),
      user: 'system',
      action: 'RISK_ASSESSED',
      details: `Risk assessed as ${riskLevel} (score: ${riskScore})`
    });

    if (this.kv) {
      await this.kv.put(changeId, JSON.stringify(change));
      await this.updateChangeIndex(changeId, change.state);
    }

    return riskAssessment;
  }

  /**
   * Check if a date falls within a freeze period
   */
  checkFreezePeriod(date) {
    const checkDate = new Date(date);

    for (const freeze of FREEZE_PERIODS) {
      if (freeze.pattern === 'lastWeekOfQuarter') {
        // Check if date is in last week of any quarter
        const month = checkDate.getMonth();
        const day = checkDate.getDate();

        // March, June, September, December
        if ([2, 5, 8, 11].includes(month)) {
          const lastDay = new Date(checkDate.getFullYear(), month + 1, 0).getDate();
          if (day > lastDay - 7) {
            return {
              inFreeze: true,
              period: freeze.name,
              reason: 'Quarterly close freeze period'
            };
          }
        }
      } else if (freeze.start && freeze.end) {
        const start = new Date(freeze.start);
        const end = new Date(freeze.end);

        if (checkDate >= start && checkDate <= end) {
          return {
            inFreeze: true,
            period: freeze.name,
            start: freeze.start,
            end: freeze.end,
            reason: `${freeze.name} freeze period`
          };
        }
      }
    }

    return {
      inFreeze: false,
      nextFreeze: this.getNextFreezePeriod(checkDate)
    };
  }

  /**
   * Request CAB (Change Advisory Board) approval
   */
  async requestCABApproval(changeId) {
    const change = await this.getChange(changeId);
    if (!change) throw new Error('Change not found');

    // Determine CAB requirements based on risk and type
    const requiresCAB =
      change.risk?.level === 'HIGH' ||
      change.risk?.level === 'CRITICAL' ||
      change.changeType === ChangeTypes.EMERGENCY ||
      change.environment === 'production';

    const cabRequest = {
      changeId,
      requestedAt: new Date().toISOString(),
      requiresCAB,
      automaticApproval: !requiresCAB && change.risk?.level === 'LOW',
      approvers: this.determineApprovers(change),
      scheduledFor: this.getNextCABMeeting(),
      status: 'pending'
    };

    change.cabRequest = cabRequest;
    change.state = ChangeStates.AUTHORIZE;
    change.updatedAt = new Date().toISOString();
    change.auditLog.push({
      timestamp: new Date().toISOString(),
      user: 'system',
      action: 'CAB_REQUESTED',
      details: `CAB approval requested. Requires CAB: ${requiresCAB}`
    });

    if (this.kv) {
      await this.kv.put(changeId, JSON.stringify(change));
      await this.updateChangeIndex(changeId, change.state);
    }

    // Auto-approve low-risk standard changes
    if (cabRequest.automaticApproval) {
      return this.approveChange(changeId, 'auto-approval', 'Low-risk standard change');
    }

    return cabRequest;
  }

  /**
   * Approve a change
   */
  async approveChange(changeId, approver, comments) {
    const change = await this.getChange(changeId);
    if (!change) throw new Error('Change not found');

    const approval = {
      approver,
      approvedAt: new Date().toISOString(),
      comments
    };

    change.approvals = change.approvals || [];
    change.approvals.push(approval);

    // Check if all required approvals are met
    const requiredApprovers = change.cabRequest?.approvers || [];
    const hasAllApprovals = requiredApprovers.every(required =>
      change.approvals.some(a => a.approver === required)
    );

    if (hasAllApprovals || approver === 'auto-approval') {
      change.state = ChangeStates.SCHEDULED;
      change.cabRequest.status = 'approved';
    }

    change.updatedAt = new Date().toISOString();
    change.auditLog.push({
      timestamp: new Date().toISOString(),
      user: approver,
      action: 'APPROVED',
      details: comments
    });

    if (this.kv) {
      await this.kv.put(changeId, JSON.stringify(change));
      await this.updateChangeIndex(changeId, change.state);
    }

    return change;
  }

  /**
   * Get list of active changes
   */
  async getActiveChanges() {
    if (!this.kv) return [];

    const index = await this.kv.get('change-index', { type: 'json' });
    if (!index) return [];

    const activeStates = [
      ChangeStates.NEW,
      ChangeStates.ASSESS,
      ChangeStates.AUTHORIZE,
      ChangeStates.SCHEDULED,
      ChangeStates.IMPLEMENT
    ];

    const activeChanges = [];
    for (const state of activeStates) {
      const changeIds = index[state] || [];
      for (const id of changeIds) {
        const change = await this.getChange(id);
        if (change) activeChanges.push(change);
      }
    }

    return activeChanges;
  }

  /**
   * Generate audit report
   */
  async generateAuditReport(startDate, endDate) {
    const changes = await this.getChangesInPeriod(startDate, endDate);

    const report = {
      period: { start: startDate, end: endDate },
      totalChanges: changes.length,
      byState: {},
      byType: {},
      byRisk: {},
      byEnvironment: {},
      emergencyChanges: [],
      failedChanges: [],
      metrics: {
        averageLeadTime: 0,
        successRate: 0,
        emergencyRate: 0,
        cabApprovalRate: 0
      }
    };

    // Analyze changes
    for (const change of changes) {
      // By state
      report.byState[change.state] = (report.byState[change.state] || 0) + 1;

      // By type
      report.byType[change.changeType] = (report.byType[change.changeType] || 0) + 1;

      // By risk
      if (change.risk) {
        report.byRisk[change.risk.level] = (report.byRisk[change.risk.level] || 0) + 1;
      }

      // By environment
      report.byEnvironment[change.environment] =
        (report.byEnvironment[change.environment] || 0) + 1;

      // Emergency changes
      if (change.changeType === ChangeTypes.EMERGENCY) {
        report.emergencyChanges.push({
          id: change.id,
          title: change.title,
          date: change.createdAt
        });
      }
    }

    // Calculate metrics
    if (changes.length > 0) {
      const successful = changes.filter(c => c.state === ChangeStates.CLOSED).length;
      report.metrics.successRate = (successful / changes.length) * 100;

      const emergency = changes.filter(c => c.changeType === ChangeTypes.EMERGENCY).length;
      report.metrics.emergencyRate = (emergency / changes.length) * 100;

      const cabApproved = changes.filter(c => c.cabRequest?.status === 'approved').length;
      report.metrics.cabApprovalRate = (cabApproved / changes.length) * 100;
    }

    return report;
  }

  // Helper methods

  async getChange(changeId) {
    if (!this.kv) return null;
    const data = await this.kv.get(changeId);
    return data ? JSON.parse(data) : null;
  }

  async updateChangeIndex(changeId, state) {
    if (!this.kv) return;

    let index = await this.kv.get('change-index', { type: 'json' }) || {};

    // Remove from all states
    for (const s in index) {
      index[s] = (index[s] || []).filter(id => id !== changeId);
    }

    // Add to new state
    index[state] = index[state] || [];
    index[state].push(changeId);

    await this.kv.put('change-index', JSON.stringify(index));
  }

  getRiskMitigations(riskLevel) {
    const mitigations = {
      LOW: ['Standard testing', 'Regular deployment window'],
      MEDIUM: ['Enhanced testing', 'Rollback plan verified', 'Stakeholder notification'],
      HIGH: ['Full regression testing', 'CAB approval required', 'Staged rollout', 'Dedicated support team'],
      CRITICAL: ['Executive approval', 'War room setup', 'All hands on deck', 'Customer communication plan']
    };
    return mitigations[riskLevel] || [];
  }

  determineApprovers(change) {
    const approvers = [];

    if (change.environment === 'production') {
      approvers.push('production-owner');
    }

    if (change.risk?.level === 'HIGH' || change.risk?.level === 'CRITICAL') {
      approvers.push('risk-manager');
    }

    if (change.changeType === ChangeTypes.EMERGENCY) {
      approvers.push('emergency-approver');
    }

    if (change.affectedServices?.length > 3) {
      approvers.push('service-owner');
    }

    return approvers.length > 0 ? approvers : ['standard-approver'];
  }

  getNextCABMeeting() {
    // CAB meetings typically weekly, e.g., every Tuesday
    const now = new Date();
    const dayOfWeek = now.getDay();
    const daysUntilTuesday = (2 - dayOfWeek + 7) % 7 || 7;
    const nextMeeting = new Date(now);
    nextMeeting.setDate(now.getDate() + daysUntilTuesday);
    nextMeeting.setHours(14, 0, 0, 0); // 2 PM
    return nextMeeting.toISOString();
  }

  getNextFreezePeriod(fromDate) {
    const upcoming = [];

    for (const freeze of FREEZE_PERIODS) {
      if (freeze.start) {
        const start = new Date(freeze.start);
        if (start > fromDate) {
          upcoming.push({
            name: freeze.name,
            start: freeze.start,
            daysUntil: Math.ceil((start - fromDate) / (1000 * 60 * 60 * 24))
          });
        }
      }
    }

    upcoming.sort((a, b) => a.daysUntil - b.daysUntil);
    return upcoming[0] || null;
  }

  async getChangesInPeriod(startDate, endDate) {
    if (!this.kv) return [];

    const index = await this.kv.get('change-index', { type: 'json' });
    if (!index) return [];

    const allChanges = [];
    for (const state in index) {
      for (const id of index[state]) {
        const change = await this.getChange(id);
        if (change) {
          const created = new Date(change.createdAt);
          if (created >= new Date(startDate) && created <= new Date(endDate)) {
            allChanges.push(change);
          }
        }
      }
    }

    return allChanges;
  }
}

module.exports = { ITILService, ChangeTypes, ChangeStates, RiskLevels, DeploymentPatterns };