# Guile ChangeFlow - ITIL 4 Change Management MCP Server

## 🎉 v1.2.0 Now Live with OAuth Support!

**MCP Endpoint**: https://mcp.changeflow.us/mcp

## Quick Start

### For Claude.ai Users

1. **Authorize**: Visit https://mcp.changeflow.us/authorize
2. **Add to Claude**: Settings → Custom Connectors → Add `https://mcp.changeflow.us/mcp`
3. **Start Using**: Access ITIL 4 change management tools directly in Claude

## Features

- ✅ **OAuth Authentication** via GitHub
- ✅ **Model Context Protocol (MCP)** compliant
- ✅ **8 ITIL Tools** for change management
- ✅ **Risk Assessment** with automatic scoring
- ✅ **Freeze Period** enforcement
- ✅ **CAB Approval** workflows
- ✅ **Audit Trail** for compliance

## Available Endpoints

- **Landing Page**: https://mcp.changeflow.us/
- **OAuth Flow**: https://mcp.changeflow.us/authorize
- **Health Check**: https://mcp.changeflow.us/health
- **MCP Protocol**: https://mcp.changeflow.us/mcp

## MCP Tools

1. `create_change_request` - Create ITIL 4 compliant change requests
2. `assess_risk` - Evaluate change risk levels
3. `check_freeze_period` - Verify deployment windows
4. `get_change_request` - Retrieve change details
5. `list_change_requests` - List all changes
6. `get_approval_status` - Check CAB approval status
7. `emergency_override` - Request emergency changes
8. `audit_trail` - View complete audit history

## Development

```bash
# Clone repository
git clone https://github.com/dsp-dr/guile-changeflow.git
cd guile-changeflow

# Run tests
gmake test

# Check production status
gmake check-prod

# Clear stuck GitHub Actions
gmake clear-queue
```

## Documentation

- [MCP Setup Guide](docs/mcp-setup-guide.md)
- [GitHub OAuth Configuration](docs/oauth-implementation-log.org)
- [Design Prompts](docs/prompts/)

## Version History

- **v1.2.0** - OAuth support, dedicated MCP domain, Remote MCP Server compatible
- **v1.1.1** - CORS support for Claude.ai
- **v1.0.0** - Initial release with basic ITIL tools

## License

MIT License - See [LICENSE](LICENSE) for details

## Support

- **Issues**: https://github.com/dsp-dr/guile-changeflow/issues
- **Discussions**: https://github.com/dsp-dr/guile-changeflow/discussions

---

*Built with ❤️ for the ITIL and AI communities*