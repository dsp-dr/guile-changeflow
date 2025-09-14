# Claude Code Directives for Guile ChangeFlow

## Post-Commit Deployment Verification

**IMPORTANT**: After committing to the main branch, ALWAYS verify deployment:

1. **Check GitHub Actions status**:
   ```bash
   gh run list --repo dsp-dr/guile-changeflow --limit 3
   ```

2. **Wait for deployment to complete** (usually 2-3 minutes)

3. **Verify production deployment**:
   ```bash
   curl -s https://mcp.changeflow.us/health | jq '.version'
   ```

4. **Confirm the version matches your commit**

This verification is CRITICAL because:
- Changes to main branch auto-deploy to production
- Deployment failures can break the live service
- Version verification confirms your changes are live

## Repository-Specific Guidelines

- `.envrc` contains no secrets and should be committed
- All secrets are in `.env` which must remain gitignored
- The MCP server runs on port 8427 locally
- Cloudflare Workers deployment is at https://mcp.changeflow.us/