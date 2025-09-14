(define-module (web dashboard)
  #:use-module (web response)
  #:export (dashboard-handler))

(define (dashboard-handler)
  "Generate HTML dashboard with auto-refresh and risk color coding"
  (let ((html-content
         (string-append
          "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>ChangeFlow Dashboard</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: rgba(255, 255, 255, 0.95);
            border-radius: 15px;
            box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
            overflow: hidden;
        }

        .header {
            background: linear-gradient(135deg, #4CAF50 0%, #45a049 100%);
            color: white;
            padding: 30px;
            text-align: center;
        }

        .header h1 {
            font-size: 2.5rem;
            margin-bottom: 10px;
            text-shadow: 0 2px 4px rgba(0, 0, 0, 0.3);
        }

        .header p {
            font-size: 1.1rem;
            opacity: 0.9;
        }

        .stats {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 25px;
            padding: 30px;
            background: #f8f9fa;
        }

        .stat-card {
            background: white;
            padding: 25px;
            border-radius: 12px;
            box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
            text-align: center;
            transition: transform 0.3s ease, box-shadow 0.3s ease;
        }

        .stat-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
        }

        .stat-value {
            font-size: 3rem;
            font-weight: bold;
            margin-bottom: 10px;
        }

        .stat-label {
            color: #666;
            font-size: 1rem;
            text-transform: uppercase;
            letter-spacing: 1px;
        }

        .total-changes .stat-value { color: #2196F3; }
        .pending-changes .stat-value { color: #FF9800; }
        .high-risk .stat-value { color: #F44336; }
        .approved-changes .stat-value { color: #4CAF50; }

        .content {
            padding: 30px;
        }

        .table-container {
            background: white;
            border-radius: 12px;
            overflow: hidden;
            box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
        }

        table {
            width: 100%;
            border-collapse: collapse;
        }

        th {
            background: linear-gradient(135deg, #4CAF50 0%, #45a049 100%);
            color: white;
            padding: 18px 15px;
            text-align: left;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            font-size: 0.9rem;
        }

        td {
            padding: 18px 15px;
            border-bottom: 1px solid #eee;
            vertical-align: middle;
        }

        tbody tr:hover {
            background: #f8f9fa;
        }

        tbody tr:last-child td {
            border-bottom: none;
        }

        .risk-critical {
            color: #c62828;
            font-weight: bold;
            background: #ffebee;
            padding: 8px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .risk-high {
            color: #d84315;
            font-weight: bold;
            background: #fff3e0;
            padding: 8px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .risk-medium {
            color: #f57c00;
            font-weight: bold;
            background: #fff8e1;
            padding: 8px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .risk-low {
            color: #388e3c;
            font-weight: bold;
            background: #e8f5e9;
            padding: 8px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .status {
            padding: 8px 15px;
            border-radius: 20px;
            font-size: 0.85rem;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .status-submitted {
            background: #e3f2fd;
            color: #1976d2;
        }

        .status-approved {
            background: #e8f5e9;
            color: #388e3c;
        }

        .status-rejected {
            background: #ffebee;
            color: #c62828;
        }

        .status-assessing {
            background: #fff3e0;
            color: #f57c00;
        }

        .refresh-info {
            text-align: center;
            margin-top: 30px;
            padding: 20px;
            background: #e8f5e9;
            border-radius: 10px;
            color: #2e7d32;
            font-weight: 500;
        }

        .no-data {
            text-align: center;
            padding: 60px 20px;
            color: #666;
            font-size: 1.1rem;
        }

        .no-data .icon {
            font-size: 4rem;
            margin-bottom: 20px;
            opacity: 0.5;
        }

        .risk-score {
            font-weight: bold;
            font-size: 1.1rem;
        }

        .loading {
            text-align: center;
            padding: 40px;
            color: #666;
            font-style: italic;
        }

        @media (max-width: 768px) {
            .container {
                margin: 0;
                border-radius: 0;
            }

            .header h1 {
                font-size: 2rem;
            }

            .stats {
                grid-template-columns: 1fr;
                padding: 20px;
            }

            .content {
                padding: 20px;
            }

            .table-container {
                overflow-x: auto;
            }

            th, td {
                padding: 12px 8px;
                font-size: 0.9rem;
            }
        }
    </style>
    <meta http-equiv=\"refresh\" content=\"5\">
</head>
<body>
    <div class=\"container\">
        <div class=\"header\">
            <h1>🔄 ChangeFlow Dashboard</h1>
            <p>Real-time Change Management System</p>
        </div>

        <div class=\"stats\">
            <div class=\"stat-card total-changes\">
                <div class=\"stat-value\" id=\"total-changes\">-</div>
                <div class=\"stat-label\">Total Changes</div>
            </div>
            <div class=\"stat-card pending-changes\">
                <div class=\"stat-value\" id=\"pending-changes\">-</div>
                <div class=\"stat-label\">Pending Review</div>
            </div>
            <div class=\"stat-card high-risk\">
                <div class=\"stat-value\" id=\"high-risk\">-</div>
                <div class=\"stat-label\">High Risk</div>
            </div>
            <div class=\"stat-card approved-changes\">
                <div class=\"stat-value\" id=\"approved-changes\">-</div>
                <div class=\"stat-label\">Approved</div>
            </div>
        </div>

        <div class=\"content\">
            <div class=\"table-container\">
                <table>
                    <thead>
                        <tr>
                            <th>Change ID</th>
                            <th>Title</th>
                            <th>Risk Score</th>
                            <th>Risk Level</th>
                            <th>Status</th>
                            <th>Created</th>
                        </tr>
                    </thead>
                    <tbody id=\"changes-body\">
                        <tr>
                            <td colspan=\"6\" class=\"loading\">Loading changes...</td>
                        </tr>
                    </tbody>
                </table>
            </div>

            <div class=\"refresh-info\">
                <strong>📊 Auto-refreshing every 5 seconds</strong><br>
                Last update: <span id=\"last-update\">Loading...</span>
            </div>
        </div>
    </div>

    <script>
        let lastUpdateTime = null;

        function updateTimestamp() {
            const now = new Date();
            document.getElementById('last-update').textContent = now.toLocaleTimeString();
        }

        async function loadChanges() {
            try {
                console.log('Fetching changes from API...');
                const response = await fetch('/api/changes');

                if (!response.ok) {
                    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
                }

                const data = await response.json();
                console.log('Received data:', data);

                const tbody = document.getElementById('changes-body');
                const changes = data.changes || [];

                // Update statistics
                const totalChanges = changes.length;
                const pendingChanges = changes.filter(c =>
                    c.status === 'submitted' || c.status === 'assessing'
                ).length;
                const highRiskChanges = changes.filter(c => c.riskScore >= 70).length;
                const approvedChanges = changes.filter(c => c.status === 'approved').length;

                document.getElementById('total-changes').textContent = totalChanges;
                document.getElementById('pending-changes').textContent = pendingChanges;
                document.getElementById('high-risk').textContent = highRiskChanges;
                document.getElementById('approved-changes').textContent = approvedChanges;

                // Update changes table
                if (changes.length === 0) {
                    tbody.innerHTML = `
                        <tr>
                            <td colspan=\"6\" class=\"no-data\">
                                <div class=\"icon\">📝</div>
                                <strong>No changes found</strong><br>
                                Create your first change request using the MCP tools!
                            </td>
                        </tr>
                    `;
                } else {
                    tbody.innerHTML = changes.map(change => `
                        <tr>
                            <td><strong>${change.id}</strong></td>
                            <td>${change.title}</td>
                            <td class=\"risk-score\">${change.riskScore}</td>
                            <td><span class=\"risk-${change.riskCategory}\">${change.riskCategory}</span></td>
                            <td><span class=\"status status-${change.status}\">${change.status}</span></td>
                            <td>${formatDate(change.createdAt)}</td>
                        </tr>
                    `).join('');
                }

                updateTimestamp();
                console.log('Dashboard updated successfully');

            } catch (error) {
                console.error('Failed to load changes:', error);
                const tbody = document.getElementById('changes-body');
                tbody.innerHTML = `
                    <tr>
                        <td colspan=\"6\" style=\"text-align: center; color: #c62828; padding: 40px;\">
                            <strong>⚠️ Error loading changes</strong><br>
                            ${error.message}<br>
                            <small>Retrying in 5 seconds...</small>
                        </td>
                    </tr>
                `;
                updateTimestamp();
            }
        }

        function formatDate(dateString) {
            if (!dateString) return 'Just now';
            try {
                const date = new Date(dateString);
                return date.toLocaleString();
            } catch (e) {
                return dateString;
            }
        }

        // Load changes immediately
        console.log('Initializing dashboard...');
        loadChanges();

        // Set up auto-refresh every 5 seconds
        setInterval(loadChanges, 5000);
        console.log('Auto-refresh enabled (5 seconds)');
    </script>
</body>
</html>")))

    (values (build-response
             #:code 200
             #:headers '((content-type . (text/html))))
            html-content)))