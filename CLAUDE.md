# Claude Code Directives for Guile ChangeFlow

## MANDATORY Post-Commit Deployment Verification

**CRITICAL**: The post-commit hook AUTOMATICALLY verifies deployments when you commit to main.

The hook will:
1. Check if deployment was triggered for your commit
2. Wait for deployment to complete (max 3 minutes)
3. Verify ALL production endpoints are responding correctly
4. Compare production version with code version
5. Report any failures or mismatches

**YOU MUST**:
- ALWAYS wait for the post-commit hook to complete after committing to main
- NEVER ignore deployment failures reported by the hook
- If the hook reports issues, investigate and fix them immediately
- If endpoints fail, check the deployment logs and fix the issue

**The hook checks these endpoints**:
- `/` (200)
- `/health` (200)
- `/mcp` (200)
- `/v1/sse` (401 - requires auth)
- `/authorize` (200 or 302)
- `/oauth/authorize` (302)
- `/.well-known/oauth-authorization-server` (200)
- `/favicon.ico` (200)

**If you see deployment issues**:
```bash
# Check deployment status
gmake deploy-status

# Clear stuck queue and redeploy
gmake kill-queue
gmake deploy-manual

# Check specific run logs
gh run view <RUN_ID> --log-failed --repo dsp-dr/guile-changeflow
```

This automated verification ensures your changes are properly deployed without manual checking.

## Repository-Specific Guidelines

- `.envrc` contains no secrets and should be committed
- All secrets are in `.env` which must remain gitignored
- The MCP server runs on port 8427 locally
- Cloudflare Workers deployment is at https://mcp.changeflow.us/