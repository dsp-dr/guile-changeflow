# Builder Agent Directive

## Primary Mission
Fix and enhance the core MCP (Model Context Protocol) server to full compliance and production readiness.

## Key Requirements

### 1. MCP Compliance
- Read ALL MCP specifications from the official spec
- Ensure full compliance with MCP protocol
- Correctly implement and expose tools, resources, prompts per spec
- Test with MCP inspector for validation

### 2. Testing & Integration
- Must work with Claude Code
- Create comprehensive integration tests for SSE and HTTP transports
- Develop unit tests for all components
- Test with MCP inspector tool for protocol validation
- **MCP Inspector Settings**: Use `http://localhost:8247` (our dev port), Transport Type: HTTP
- Reference screenshot in gcf-mcp-inspector directory shows Inspector v0.16.7 interface

### 3. Type Safety & Quality
- Migrate to TypeScript if necessary for better type safety
- Use appropriate type checking tools
- Follow modern JavaScript/TypeScript best practices
- Ensure error handling and edge cases are covered

### 4. Development Process
- Work in isolated worktree (gcf-mcp-inspector)
- Commit work frequently with descriptive messages
- Use `git note` for detailed context that doesn't belong in commit messages:
  - Decision rationale
  - Context and background
  - Goals and objectives
  - Issues encountered
  - Debugging approaches
  - Time log of work sessions
  - Technical considerations

### 5. Documentation
- Keep implementation aligned with ITIL 4 change management
- Document API endpoints and tool capabilities
- Maintain compatibility with existing Cloudflare Workers deployment

## Current Codebase Context
- Main MCP server: `mcp-server/changeflow-mcp.js`
- Existing ITIL tools: 8 core change management functions
- Deployment target: Cloudflare Workers
- Testing: Local development on port 8247/8427

## Success Criteria
- MCP inspector shows full compliance
- Integration tests pass for SSE/HTTP
- Works seamlessly with Claude Code
- Type-safe implementation
- Comprehensive test coverage
- Clean commit history with detailed git notes

## Coordination
- Coordinator agent monitors your tmux session
- Regular check-ins every 10 minutes via monitoring script
- Stay focused on core MCP functionality (NO REMOTE/OAuth features)