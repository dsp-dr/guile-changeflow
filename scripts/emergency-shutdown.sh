#!/usr/bin/env bash

# Emergency Shutdown Script for Cloudflare Workers
# Provides quick kill switch and maintenance mode options

set -e

ACTION=${1:-help}

echo "🚨 Cloudflare Emergency Control Script"
echo "======================================"

case "$ACTION" in
  kill|shutdown)
    echo "⚠️  EMERGENCY SHUTDOWN - Disabling all workers..."

    # Disable production route
    echo "Removing production route..."
    cd infra/cloudflare
    wrangler route delete "api.changeflow.us/*" || true

    # Deploy maintenance page
    cat > emergency-worker.js << 'EOF'
export default {
  async fetch(request) {
    return new Response(JSON.stringify({
      status: "maintenance",
      message: "System temporarily offline for emergency maintenance",
      timestamp: new Date().toISOString()
    }), {
      status: 503,
      headers: {
        "Content-Type": "application/json",
        "Retry-After": "3600"
      }
    });
  }
};
EOF

    echo "Deploying maintenance worker..."
    wrangler publish emergency-worker.js --name changeflow-maintenance

    echo "✅ SHUTDOWN COMPLETE - All traffic blocked"
    echo "To restore: ./emergency-shutdown.sh restore"
    ;;

  maintenance)
    echo "🔧 Enabling maintenance mode..."

    cd infra/cloudflare

    # Deploy maintenance worker
    cat > maintenance-worker.js << 'EOF'
export default {
  async fetch(request) {
    const maintenanceEnd = new Date();
    maintenanceEnd.setHours(maintenanceEnd.getHours() + 2);

    return new Response(JSON.stringify({
      status: "maintenance",
      message: "Scheduled maintenance in progress",
      estimated_completion: maintenanceEnd.toISOString(),
      timestamp: new Date().toISOString()
    }), {
      status: 503,
      headers: {
        "Content-Type": "application/json",
        "Retry-After": "600",
        "Cache-Control": "no-cache"
      }
    });
  }
};
EOF

    echo "Deploying maintenance mode..."
    wrangler publish maintenance-worker.js --compatibility-date 2024-01-01

    echo "✅ Maintenance mode enabled"
    echo "To restore: ./emergency-shutdown.sh restore"
    ;;

  restore)
    echo "♻️  Restoring normal operations..."

    cd infra/cloudflare

    # Restore production worker
    echo "Redeploying production worker..."
    wrangler publish

    # Re-add custom domain route
    echo "Restoring routes..."
    wrangler route add "api.changeflow.us/*"

    # Verify restoration
    sleep 5
    if curl -s https://api.changeflow.us/ | grep -q "healthy"; then
      echo "✅ Service restored successfully"
    else
      echo "⚠️  Service may need additional time to propagate"
    fi
    ;;

  status)
    echo "🔍 Checking service status..."

    echo -n "Production (api.changeflow.us): "
    if curl -s https://api.changeflow.us/ | grep -q "healthy"; then
      echo "✅ ONLINE"
    else
      echo "❌ OFFLINE/MAINTENANCE"
    fi

    echo -n "Staging (jasonwalsh.workers.dev): "
    if curl -s https://guile-changeflow-staging.jasonwalsh.workers.dev/ | grep -q "healthy"; then
      echo "✅ ONLINE"
    else
      echo "❌ OFFLINE/MAINTENANCE"
    fi
    ;;

  help|*)
    echo "Usage: $0 [action]"
    echo ""
    echo "Actions:"
    echo "  kill|shutdown  - Emergency shutdown (503 all requests)"
    echo "  maintenance    - Enable maintenance mode with message"
    echo "  restore        - Restore normal operations"
    echo "  status         - Check current status"
    echo ""
    echo "Examples:"
    echo "  $0 shutdown    # Emergency kill switch"
    echo "  $0 maintenance # Scheduled maintenance"
    echo "  $0 restore     # Bring back online"
    echo "  $0 status      # Check status"
    ;;
esac