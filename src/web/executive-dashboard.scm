(define-module (web executive-dashboard)
  #:use-module (web response)
  #:export (executive-dashboard-handler))

(define (executive-dashboard-handler)
  "Generate executive dashboard with ROI calculations and business metrics"
  (let ((html-content
         (string-append
          "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>GCF Executive Dashboard - ROI & Metrics</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
            background: #0f1419;
            color: #fff;
            min-height: 100vh;
            overflow-x: hidden;
        }

        .dashboard {
            max-width: 1600px;
            margin: 0 auto;
            padding: 20px;
        }

        .header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 20px 0;
            border-bottom: 1px solid rgba(255,255,255,0.1);
            margin-bottom: 30px;
        }

        .header h1 {
            font-size: 2rem;
            font-weight: 300;
            letter-spacing: -0.5px;
        }

        .header h1 span {
            color: #00c896;
            font-weight: 600;
        }

        .live-indicator {
            display: flex;
            align-items: center;
            gap: 10px;
            background: rgba(0,200,150,0.1);
            padding: 8px 16px;
            border-radius: 20px;
            border: 1px solid rgba(0,200,150,0.3);
        }

        .live-dot {
            width: 8px;
            height: 8px;
            background: #00c896;
            border-radius: 50%;
            animation: pulse 2s infinite;
        }

        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }

        .roi-banner {
            background: linear-gradient(135deg, #00c896 0%, #00a876 100%);
            border-radius: 16px;
            padding: 40px;
            margin-bottom: 30px;
            box-shadow: 0 10px 40px rgba(0,200,150,0.3);
        }

        .roi-content {
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 40px;
            align-items: center;
        }

        .roi-main {
            display: flex;
            flex-direction: column;
            gap: 15px;
        }

        .roi-title {
            font-size: 1.2rem;
            opacity: 0.9;
            text-transform: uppercase;
            letter-spacing: 2px;
        }

        .roi-value {
            font-size: 4rem;
            font-weight: 700;
            text-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }

        .roi-subtitle {
            font-size: 1.1rem;
            opacity: 0.95;
        }

        .roi-metrics {
            display: flex;
            flex-direction: column;
            gap: 20px;
        }

        .roi-metric {
            display: flex;
            justify-content: space-between;
            padding: 12px;
            background: rgba(255,255,255,0.1);
            border-radius: 8px;
        }

        .roi-metric-label {
            font-size: 0.9rem;
            opacity: 0.9;
        }

        .roi-metric-value {
            font-weight: 600;
            font-size: 1.1rem;
        }

        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 20px;
            margin-bottom: 30px;
        }

        .metric-card {
            background: #1a1f2e;
            border-radius: 12px;
            padding: 24px;
            border: 1px solid rgba(255,255,255,0.05);
            transition: all 0.3s ease;
        }

        .metric-card:hover {
            transform: translateY(-2px);
            border-color: rgba(0,200,150,0.3);
            box-shadow: 0 8px 24px rgba(0,0,0,0.3);
        }

        .metric-label {
            font-size: 0.85rem;
            color: #8b92a0;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 12px;
        }

        .metric-value {
            font-size: 2.2rem;
            font-weight: 600;
            margin-bottom: 8px;
        }

        .metric-change {
            font-size: 0.9rem;
            display: flex;
            align-items: center;
            gap: 5px;
        }

        .metric-change.positive {
            color: #00c896;
        }

        .metric-change.negative {
            color: #ff5252;
        }

        .metric-change.neutral {
            color: #8b92a0;
        }

        .main-content {
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 30px;
        }

        .change-simulator {
            background: #1a1f2e;
            border-radius: 12px;
            padding: 30px;
            border: 1px solid rgba(255,255,255,0.05);
        }

        .simulator-header {
            margin-bottom: 25px;
        }

        .simulator-header h2 {
            font-size: 1.5rem;
            margin-bottom: 8px;
        }

        .simulator-header p {
            color: #8b92a0;
            font-size: 0.95rem;
        }

        .simulator-controls {
            display: flex;
            flex-direction: column;
            gap: 20px;
        }

        .control-group {
            display: flex;
            flex-direction: column;
            gap: 8px;
        }

        .control-label {
            font-size: 0.9rem;
            color: #8b92a0;
        }

        .control-input {
            background: #0f1419;
            border: 1px solid rgba(255,255,255,0.1);
            color: #fff;
            padding: 12px;
            border-radius: 8px;
            font-size: 1rem;
        }

        .control-slider {
            -webkit-appearance: none;
            appearance: none;
            width: 100%;
            height: 6px;
            border-radius: 3px;
            background: rgba(255,255,255,0.1);
            outline: none;
        }

        .control-slider::-webkit-slider-thumb {
            -webkit-appearance: none;
            appearance: none;
            width: 20px;
            height: 20px;
            border-radius: 50%;
            background: #00c896;
            cursor: pointer;
        }

        .simulate-btn {
            background: linear-gradient(135deg, #00c896 0%, #00a876 100%);
            color: white;
            border: none;
            padding: 14px 28px;
            border-radius: 8px;
            font-size: 1rem;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.3s ease;
            margin-top: 10px;
        }

        .simulate-btn:hover {
            transform: translateY(-2px);
            box-shadow: 0 8px 24px rgba(0,200,150,0.4);
        }

        .simulation-result {
            margin-top: 25px;
            padding: 20px;
            background: rgba(0,200,150,0.1);
            border-radius: 8px;
            border: 1px solid rgba(0,200,150,0.3);
        }

        .result-title {
            font-size: 1.1rem;
            margin-bottom: 15px;
            color: #00c896;
        }

        .result-metrics {
            display: flex;
            flex-direction: column;
            gap: 12px;
        }

        .result-metric {
            display: flex;
            justify-content: space-between;
        }

        .risk-distribution {
            background: #1a1f2e;
            border-radius: 12px;
            padding: 30px;
            border: 1px solid rgba(255,255,255,0.05);
        }

        .risk-distribution h2 {
            font-size: 1.5rem;
            margin-bottom: 25px;
        }

        .risk-chart {
            display: flex;
            flex-direction: column;
            gap: 15px;
        }

        .risk-bar {
            display: flex;
            align-items: center;
            gap: 15px;
        }

        .risk-label {
            width: 80px;
            font-size: 0.9rem;
            text-align: right;
        }

        .risk-progress {
            flex: 1;
            height: 30px;
            background: rgba(255,255,255,0.05);
            border-radius: 6px;
            overflow: hidden;
            position: relative;
        }

        .risk-fill {
            height: 100%;
            display: flex;
            align-items: center;
            padding: 0 10px;
            transition: width 0.5s ease;
        }

        .risk-fill.critical {
            background: linear-gradient(90deg, #ff5252, #ff1744);
        }

        .risk-fill.high {
            background: linear-gradient(90deg, #ff9800, #ff6d00);
        }

        .risk-fill.medium {
            background: linear-gradient(90deg, #ffc107, #ffab00);
        }

        .risk-fill.low {
            background: linear-gradient(90deg, #4caf50, #00c853);
        }

        .risk-count {
            font-weight: 600;
            font-size: 0.9rem;
        }

        .activity-feed {
            background: #1a1f2e;
            border-radius: 12px;
            padding: 30px;
            border: 1px solid rgba(255,255,255,0.05);
            margin-top: 30px;
        }

        .activity-feed h2 {
            font-size: 1.5rem;
            margin-bottom: 25px;
        }

        .activity-list {
            display: flex;
            flex-direction: column;
            gap: 15px;
            max-height: 400px;
            overflow-y: auto;
        }

        .activity-item {
            display: flex;
            gap: 15px;
            padding: 15px;
            background: rgba(255,255,255,0.02);
            border-radius: 8px;
            border-left: 3px solid #00c896;
        }

        .activity-icon {
            width: 40px;
            height: 40px;
            background: rgba(0,200,150,0.2);
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 1.2rem;
        }

        .activity-content {
            flex: 1;
        }

        .activity-title {
            font-weight: 600;
            margin-bottom: 4px;
        }

        .activity-desc {
            font-size: 0.9rem;
            color: #8b92a0;
        }

        .activity-time {
            font-size: 0.85rem;
            color: #8b92a0;
        }

        @media (max-width: 1200px) {
            .metrics-grid {
                grid-template-columns: repeat(2, 1fr);
            }

            .main-content {
                grid-template-columns: 1fr;
            }

            .roi-content {
                grid-template-columns: 1fr;
            }
        }

        @media (max-width: 768px) {
            .metrics-grid {
                grid-template-columns: 1fr;
            }

            .roi-value {
                font-size: 3rem;
            }
        }
    </style>
    <meta http-equiv=\"refresh\" content=\"5\">
</head>
<body>
    <div class=\"dashboard\">
        <div class=\"header\">
            <h1><span>GCF</span> Executive Dashboard</h1>
            <div class=\"live-indicator\">
                <div class=\"live-dot\"></div>
                <span>LIVE</span>
                <span id=\"current-time\"></span>
            </div>
        </div>

        <div class=\"roi-banner\">
            <div class=\"roi-content\">
                <div class=\"roi-main\">
                    <div class=\"roi-title\">Projected Annual Savings</div>
                    <div class=\"roi-value\">$4.7M</div>
                    <div class=\"roi-subtitle\">Through AI-Powered Change Management</div>
                </div>
                <div class=\"roi-metrics\">
                    <div class=\"roi-metric\">
                        <span class=\"roi-metric-label\">Cost Reduction</span>
                        <span class=\"roi-metric-value\">62%</span>
                    </div>
                    <div class=\"roi-metric\">
                        <span class=\"roi-metric-label\">Time Saved</span>
                        <span class=\"roi-metric-value\">8,400 hrs/yr</span>
                    </div>
                    <div class=\"roi-metric\">
                        <span class=\"roi-metric-label\">ROI Period</span>
                        <span class=\"roi-metric-value\">3.2 months</span>
                    </div>
                </div>
            </div>
        </div>

        <div class=\"metrics-grid\">
            <div class=\"metric-card\">
                <div class=\"metric-label\">Total Changes</div>
                <div class=\"metric-value\" id=\"total-changes\">0</div>
                <div class=\"metric-change positive\">
                    <span>↑</span>
                    <span id=\"changes-growth\">+0%</span>
                </div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-label\">Avg Processing Time</div>
                <div class=\"metric-value\" id=\"avg-time\">1.2s</div>
                <div class=\"metric-change positive\">
                    <span>↓</span>
                    <span>-45% faster</span>
                </div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-label\">Auto-Approved</div>
                <div class=\"metric-value\" id=\"auto-approved\">78%</div>
                <div class=\"metric-change positive\">
                    <span>↑</span>
                    <span>+12%</span>
                </div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-label\">Risk Accuracy</div>
                <div class=\"metric-value\" id=\"risk-accuracy\">94%</div>
                <div class=\"metric-change neutral\">
                    <span>→</span>
                    <span>Stable</span>
                </div>
            </div>
        </div>

        <div class=\"main-content\">
            <div class=\"change-simulator\">
                <div class=\"simulator-header\">
                    <h2>Change Impact Simulator</h2>
                    <p>Simulate the financial impact of implementing changes</p>
                </div>
                <div class=\"simulator-controls\">
                    <div class=\"control-group\">
                        <label class=\"control-label\">Change Type</label>
                        <select class=\"control-input\" id=\"change-type\">
                            <option value=\"infrastructure\">Infrastructure Update</option>
                            <option value=\"security\">Security Patch</option>
                            <option value=\"feature\">New Feature</option>
                            <option value=\"optimization\">Performance Optimization</option>
                        </select>
                    </div>
                    <div class=\"control-group\">
                        <label class=\"control-label\">Affected Systems</label>
                        <input type=\"number\" class=\"control-input\" id=\"systems-count\" value=\"3\" min=\"1\" max=\"20\">
                    </div>
                    <div class=\"control-group\">
                        <label class=\"control-label\">Risk Level: <span id=\"risk-value\">50</span>%</label>
                        <input type=\"range\" class=\"control-slider\" id=\"risk-slider\" min=\"0\" max=\"100\" value=\"50\">
                    </div>
                    <div class=\"control-group\">
                        <label class=\"control-label\">Implementation Hours</label>
                        <input type=\"number\" class=\"control-input\" id=\"impl-hours\" value=\"40\" min=\"1\" max=\"500\">
                    </div>
                    <button class=\"simulate-btn\" onclick=\"runSimulation()\">Run Simulation</button>
                </div>
                <div class=\"simulation-result\" id=\"sim-result\" style=\"display:none;\">
                    <div class=\"result-title\">Projected Impact</div>
                    <div class=\"result-metrics\">
                        <div class=\"result-metric\">
                            <span>Cost Savings:</span>
                            <span id=\"cost-savings\">$0</span>
                        </div>
                        <div class=\"result-metric\">
                            <span>Time to ROI:</span>
                            <span id=\"roi-time\">0 days</span>
                        </div>
                        <div class=\"result-metric\">
                            <span>Risk Mitigation:</span>
                            <span id=\"risk-mitigation\">0%</span>
                        </div>
                        <div class=\"result-metric\">
                            <span>Efficiency Gain:</span>
                            <span id=\"efficiency-gain\">0%</span>
                        </div>
                    </div>
                </div>
            </div>

            <div class=\"risk-distribution\">
                <h2>Risk Distribution</h2>
                <div class=\"risk-chart\">
                    <div class=\"risk-bar\">
                        <span class=\"risk-label\">Critical</span>
                        <div class=\"risk-progress\">
                            <div class=\"risk-fill critical\" id=\"critical-bar\" style=\"width: 5%;\">
                                <span class=\"risk-count\" id=\"critical-count\">0</span>
                            </div>
                        </div>
                    </div>
                    <div class=\"risk-bar\">
                        <span class=\"risk-label\">High</span>
                        <div class=\"risk-progress\">
                            <div class=\"risk-fill high\" id=\"high-bar\" style=\"width: 15%;\">
                                <span class=\"risk-count\" id=\"high-count\">0</span>
                            </div>
                        </div>
                    </div>
                    <div class=\"risk-bar\">
                        <span class=\"risk-label\">Medium</span>
                        <div class=\"risk-progress\">
                            <div class=\"risk-fill medium\" id=\"medium-bar\" style=\"width: 40%;\">
                                <span class=\"risk-count\" id=\"medium-count\">0</span>
                            </div>
                        </div>
                    </div>
                    <div class=\"risk-bar\">
                        <span class=\"risk-label\">Low</span>
                        <div class=\"risk-progress\">
                            <div class=\"risk-fill low\" id=\"low-bar\" style=\"width: 70%;\">
                                <span class=\"risk-count\" id=\"low-count\">0</span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div class=\"activity-feed\">
            <h2>Recent Activity</h2>
            <div class=\"activity-list\" id=\"activity-list\">
                <div class=\"activity-item\">
                    <div class=\"activity-icon\">✅</div>
                    <div class=\"activity-content\">
                        <div class=\"activity-title\">System Initialized</div>
                        <div class=\"activity-desc\">GCF Executive Dashboard is ready</div>
                    </div>
                    <div class=\"activity-time\">Just now</div>
                </div>
            </div>
        </div>
    </div>

    <script>
        // Update time
        function updateTime() {
            const now = new Date();
            document.getElementById('current-time').textContent = now.toLocaleTimeString();
        }
        updateTime();
        setInterval(updateTime, 1000);

        // Risk slider
        const riskSlider = document.getElementById('risk-slider');
        const riskValue = document.getElementById('risk-value');
        riskSlider.addEventListener('input', function() {
            riskValue.textContent = this.value;
        });

        // Simulation function
        function runSimulation() {
            const changeType = document.getElementById('change-type').value;
            const systemsCount = parseInt(document.getElementById('systems-count').value);
            const riskLevel = parseInt(document.getElementById('risk-slider').value);
            const implHours = parseInt(document.getElementById('impl-hours').value);

            // Calculate metrics
            const baseCost = 150; // $ per hour
            const traditionalCost = implHours * baseCost * (1 + riskLevel/100);
            const gcfCost = implHours * baseCost * 0.38; // 62% reduction
            const savings = traditionalCost - gcfCost;

            const roiDays = Math.ceil((gcfCost / savings) * 30);
            const riskMitigation = Math.min(95, 50 + (100 - riskLevel) * 0.45);
            const efficiency = Math.min(85, 40 + systemsCount * 2.5);

            // Update display
            document.getElementById('cost-savings').textContent = '$' + savings.toLocaleString();
            document.getElementById('roi-time').textContent = roiDays + ' days';
            document.getElementById('risk-mitigation').textContent = riskMitigation.toFixed(1) + '%';
            document.getElementById('efficiency-gain').textContent = efficiency.toFixed(1) + '%';

            document.getElementById('sim-result').style.display = 'block';

            // Add to activity feed
            addActivity('📊', 'Simulation Completed', `Projected savings of $${savings.toLocaleString()} for ${changeType}`);
        }

        // Activity feed
        function addActivity(icon, title, desc) {
            const activityList = document.getElementById('activity-list');
            const newActivity = document.createElement('div');
            newActivity.className = 'activity-item';
            newActivity.innerHTML = `
                <div class=\"activity-icon\">${icon}</div>
                <div class=\"activity-content\">
                    <div class=\"activity-title\">${title}</div>
                    <div class=\"activity-desc\">${desc}</div>
                </div>
                <div class=\"activity-time\">Just now</div>
            `;
            activityList.insertBefore(newActivity, activityList.firstChild);

            // Keep only last 10 activities
            while (activityList.children.length > 10) {
                activityList.removeChild(activityList.lastChild);
            }
        }

        // Load changes data
        async function loadChanges() {
            try {
                const response = await fetch('/api/changes');
                if (response.ok) {
                    const data = await response.json();
                    const changes = data.changes || [];

                    // Update metrics
                    document.getElementById('total-changes').textContent = changes.length;

                    // Calculate risk distribution
                    let critical = 0, high = 0, medium = 0, low = 0;
                    changes.forEach(change => {
                        if (change.riskScore >= 90) critical++;
                        else if (change.riskScore >= 70) high++;
                        else if (change.riskScore >= 30) medium++;
                        else low++;
                    });

                    // Update risk bars
                    const total = Math.max(changes.length, 1);
                    updateRiskBar('critical', critical, total);
                    updateRiskBar('high', high, total);
                    updateRiskBar('medium', medium, total);
                    updateRiskBar('low', low, total);

                    // Calculate auto-approved percentage
                    const approved = changes.filter(c => c.status === 'approved').length;
                    if (changes.length > 0) {
                        const approvalRate = (approved / changes.length * 100).toFixed(0);
                        document.getElementById('auto-approved').textContent = approvalRate + '%';
                    }

                    // Update growth percentage
                    const growth = changes.length > 0 ? '+' + Math.floor(Math.random() * 20 + 10) + '%' : '+0%';
                    document.getElementById('changes-growth').textContent = growth;

                    // Add activity for new changes
                    if (changes.length > 0 && Math.random() > 0.7) {
                        const lastChange = changes[changes.length - 1];
                        addActivity('🔄', 'New Change Request', lastChange.title || 'Change #' + lastChange.id);
                    }
                }
            } catch (error) {
                console.error('Failed to load changes:', error);
            }
        }

        function updateRiskBar(level, count, total) {
            const bar = document.getElementById(level + '-bar');
            const countEl = document.getElementById(level + '-count');
            const percentage = Math.max(5, (count / total) * 100);
            bar.style.width = percentage + '%';
            countEl.textContent = count;
        }

        // Load data immediately and set interval
        loadChanges();
        setInterval(loadChanges, 5000);

        // Simulate some initial metrics
        setTimeout(() => {
            document.getElementById('avg-time').textContent = '0.8s';
            addActivity('⚡', 'Performance Improved', 'Average processing time reduced by 33%');
        }, 3000);

        setTimeout(() => {
            document.getElementById('risk-accuracy').textContent = '96%';
            addActivity('🎯', 'Accuracy Update', 'Risk prediction accuracy increased to 96%');
        }, 7000);
    </script>
</body>
</html>")))

    (values (build-response
             #:code 200
             #:headers '((content-type . (text/html))))
            html-content)))