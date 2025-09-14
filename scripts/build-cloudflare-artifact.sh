#!/usr/bin/env bash

# Build Cloudflare Worker Artifact for Manual Deployment
# This creates a deployable worker.js file ready for Cloudflare dashboard upload

echo "🏗️  Building Cloudflare Worker Artifact for Manual Deployment"
echo "============================================================"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create build directory
BUILD_DIR="build/cloudflare"
mkdir -p "$BUILD_DIR"

# Copy the worker file
echo -e "${YELLOW}📦 Preparing worker.js...${NC}"
cp infra/cloudflare/worker.js "$BUILD_DIR/worker.js"

# Update version to 1.0.0 in the worker
echo -e "${YELLOW}🔧 Setting version to 1.0.0...${NC}"
sed -i.bak "s/version: '.*'/version: '1.0.0'/" "$BUILD_DIR/worker.js"
sed -i.bak "s/0.0.1-skeleton/1.0.0/" "$BUILD_DIR/worker.js"
rm "$BUILD_DIR/worker.js.bak"

# Create deployment info file
cat > "$BUILD_DIR/deployment-info.txt" << 'EOF'
CLOUDFLARE WORKER MANUAL DEPLOYMENT INSTRUCTIONS
================================================

1. Go to Cloudflare Dashboard:
   https://dash.cloudflare.com/

2. Select your account/zone

3. Go to Workers & Pages > Create Application > Create Worker

4. Name it: guile-changeflow-prod (or use existing)

5. Click "Quick Edit" or "Edit Code"

6. DELETE all existing code

7. PASTE the entire contents of worker.js

8. Click "Save and Deploy"

9. Configure custom domain (if needed):
   - Go to Settings > Triggers
   - Add custom domain: api.changeflow.us
   - Or use the provided *.workers.dev URL

10. Test the deployment:
    curl https://api.changeflow.us/
    or
    curl https://guile-changeflow-prod.YOUR-SUBDOMAIN.workers.dev/

11. Verify MCP tools are working:
    curl -X POST https://api.changeflow.us/ \
      -H "Content-Type: application/json" \
      -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

ENVIRONMENT VARIABLES (Optional):
- Add in Settings > Variables if needed
- WORKER_VERSION: 1.0.0
- MCP_PROTOCOL_VERSION: 2024-11-05
- CORS_ORIGINS: *

EOF

# Create a minified version (optional)
echo -e "${YELLOW}🗜️  Creating minified version...${NC}"
if command -v node > /dev/null; then
    # Simple minification by removing comments and excess whitespace
    node -e "
    const fs = require('fs');
    const code = fs.readFileSync('$BUILD_DIR/worker.js', 'utf8');
    const minified = code
        .replace(/\/\*[\s\S]*?\*\//g, '') // Remove block comments
        .replace(/\/\/.*$/gm, '')         // Remove line comments
        .replace(/\n\s*\n/g, '\n')        // Remove empty lines
        .replace(/^\s+/gm, ' ');          // Reduce indentation
    fs.writeFileSync('$BUILD_DIR/worker.min.js', minified);
    console.log('Minified version created');
    " 2>/dev/null || echo "Minification skipped (optional)"
else
    echo "Node.js not found - skipping minification"
fi

# Calculate file sizes
ORIGINAL_SIZE=$(wc -c < "$BUILD_DIR/worker.js")
echo -e "${GREEN}✅ Build complete!${NC}"
echo ""
echo "📁 Build artifacts in: $BUILD_DIR/"
echo "   - worker.js (${ORIGINAL_SIZE} bytes) - Main deployment file"
if [ -f "$BUILD_DIR/worker.min.js" ]; then
    MIN_SIZE=$(wc -c < "$BUILD_DIR/worker.min.js")
    echo "   - worker.min.js (${MIN_SIZE} bytes) - Minified version"
fi
echo "   - deployment-info.txt - Manual deployment instructions"
echo ""
echo -e "${GREEN}📋 Quick Deployment Steps:${NC}"
echo "1. Copy the contents of $BUILD_DIR/worker.js"
echo "2. Go to https://dash.cloudflare.com/"
echo "3. Navigate to Workers & Pages"
echo "4. Create or edit worker: guile-changeflow-prod"
echo "5. Paste the code and click 'Save and Deploy'"
echo ""
echo -e "${YELLOW}🔍 To view the worker code:${NC}"
echo "   cat $BUILD_DIR/worker.js | head -50"
echo ""
echo -e "${YELLOW}🚀 To test after deployment:${NC}"
echo "   curl https://api.changeflow.us/"
echo "   curl -X POST https://api.changeflow.us/ -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":1}' | jq ."