# Domain Configuration for ChangeFlow

## Current Setup

### MCP Server (Main Application)
- **Worker**: `guile-changeflow-prod`
- **URL**: https://mcp.changeflow.us
- **Custom Domain**: Configured in Cloudflare dashboard
- **Purpose**: OAuth-enabled MCP server for Claude.ai integration

### Landing Page (Root Domain)
- **Worker**: `changeflow-landing`
- **URLs**:
  - https://changeflow.us
  - https://www.changeflow.us
- **Purpose**: Serves favicon for Claude.ai and redirects to MCP server

## Why Two Workers?

1. **MCP Server** (`mcp.changeflow.us`): Full MCP implementation with OAuth
2. **Landing Page** (`changeflow.us`): Simple page that:
   - Serves `/favicon.ico` for Google's favicon service
   - Provides landing page with information
   - Redirects to main MCP server

## Setup Instructions

### 1. Deploy the Workers

```bash
# Deploy MCP server (already done)
cd infra/cloudflare
wrangler deploy

# Deploy landing page
wrangler deploy -c wrangler-landing.toml
```

### 2. Configure Custom Domains in Cloudflare Dashboard

1. Go to Workers & Pages in Cloudflare dashboard
2. Find `changeflow-landing` worker
3. Go to Settings → Triggers
4. Add custom domains:
   - `changeflow.us`
   - `www.changeflow.us`

### 3. DNS Configuration

Ensure these DNS records exist:
- `changeflow.us` → Cloudflare proxy
- `www.changeflow.us` → Cloudflare proxy
- `mcp.changeflow.us` → Cloudflare proxy (already configured)

## Why This Fixes Claude.ai Favicon

Claude.ai uses Google's favicon service:
```
https://www.google.com/s2/favicons?domain=changeflow.us
```

Google fetches from the root domain, not subdomains. By serving `/favicon.ico` at `changeflow.us`, Google's service will cache and serve our favicon to Claude.ai.

## Testing

After setup, test these URLs:
```bash
# Should return our favicon
curl -I https://changeflow.us/favicon.ico
curl -I https://www.changeflow.us/favicon.ico

# Should show landing page
curl https://changeflow.us/

# After Google crawls it (may take time):
curl "https://www.google.com/s2/favicons?domain=changeflow.us"
```

## Architecture

```
Claude.ai
    ↓
Google Favicon Service
    ↓
changeflow.us/favicon.ico (Landing Worker)

Claude.ai MCP Integration
    ↓
mcp.changeflow.us (Main MCP Worker)
```