# Experiment 010: Cloudflare Workers Logs Integration

## Objective
Integrate Cloudflare Workers Logs for production observability of the Guile ChangeFlow MCP server.

## Reference
https://developers.cloudflare.com/workers/observability/logs/workers-logs/

## Key Features to Implement

### 1. Enable Workers Logs
- Configure via wrangler.toml or dashboard
- Set up log retention policies
- Configure sampling rates for cost control

### 2. Structured Logging
```javascript
// Log ITIL change events
console.log(JSON.stringify({
  timestamp: new Date().toISOString(),
  event: 'change_request',
  change_id: 'CHG-2025-0001',
  risk_level: 'high',
  cab_approval: true,
  metrics: {
    response_time_ms: 45,
    validation_passed: true
  }
}));
```

### 3. Head-Based Sampling
```javascript
// Implement sampling for cost control
const SAMPLE_RATE = 0.1; // 10% sampling
if (Math.random() < SAMPLE_RATE) {
  console.log('Sampled request', requestDetails);
}
```

### 4. Integration Points
- Change request lifecycle events
- CAB approval workflows
- Risk assessment calculations
- Freeze period violations
- Audit trail generation
- Performance metrics

### 5. Log Aggregation
- Send to Logpush destinations
- Configure real-time streaming
- Set up alerting thresholds

## Implementation Tasks

1. **Enable Workers Logs in wrangler.toml**
```toml
[observability]
logs = true
```

2. **Add structured logging to worker.js**
- Request/response logging
- MCP protocol events
- ITIL workflow states
- Error tracking
- Performance metrics

3. **Configure sampling strategy**
- Head-based sampling for cost control
- Always log critical events (emergencies, failures)
- Sample routine operations at 10%

4. **Create monitoring dashboard**
- Query Workers Logs via API
- Visualize change metrics
- Track SLA compliance
- Monitor error rates

## Expected Outcomes
- Real-time visibility into production MCP server
- Cost-effective logging with sampling
- Compliance audit trail
- Performance optimization insights
- Incident response acceleration

## Demo Scenarios for 7 AM

1. **Live Change Tracking**
   - Show real-time change requests in logs
   - Demonstrate approval workflows
   - Display risk calculations

2. **Performance Analysis**
   - Response time percentiles
   - Throughput metrics
   - Error rate tracking

3. **Compliance Reporting**
   - Generate audit reports from logs
   - Show SOX compliance evidence
   - Demonstrate data retention

## Status
- [ ] Enable Workers Logs
- [ ] Implement structured logging
- [ ] Configure sampling
- [ ] Test with battle scenarios
- [ ] Create monitoring queries
- [ ] Prepare demo dashboard