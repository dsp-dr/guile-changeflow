# MCP Sequence Diagrams

## Complete Claude.ai Integration Flow

### OAuth + MCP Sequence

```mermaid
sequenceDiagram
    participant U as User
    participant C as Claude.ai
    participant G as GitHub OAuth
    participant M as ChangeFlow MCP

    Note over U,M: Phase 1: Setup & Authorization
    U->>+M: Visit https://mcp.changeflow.us/
    M->>-U: Landing page with "Authorize" button

    U->>+M: Click /authorize
    M->>+G: Redirect to GitHub OAuth
    G->>-U: GitHub login page

    U->>+G: Enter credentials
    G->>+M: Callback with auth code
    M->>+G: Exchange code for token
    G->>-M: Access token
    M->>-U: Success page with /v1/sse URL

    Note over U,M: Phase 2: Claude.ai MCP Connection
    U->>+C: Add Custom Connector: https://mcp.changeflow.us/v1/sse
    C->>+M: POST /v1/sse - MCP Initialize
    Note right of C: {"method":"initialize","protocolVersion":"2024-11-05"}
    M->>-C: SSE: Initialize Response
    Note left of M: {"result":{"capabilities":{"tools":{}},"serverInfo":{}}}

    C->>+M: POST /v1/sse - Tools List
    Note right of C: {"method":"tools/list"}
    M->>-C: SSE: 8 ITIL Tools
    Note left of M: create_change_request, assess_risk, etc.

    C->>U: Tools available in interface

    Note over U,M: Phase 3: Tool Usage
    U->>+C: "Create a change request for database upgrade"
    C->>+M: POST /v1/sse - Tool Call
    Note right of C: {"method":"tools/call","params":{"name":"create_change_request"}}
    M->>-C: SSE: Change Request Created
    C->>-U: "Created CHG-1726339234567..."
```

### Critical MCP Protocol Handshake

```mermaid
sequenceDiagram
    participant C as Claude.ai
    participant M as MCP Server

    Note over C,M: CRITICAL: Initialize must happen first

    C->>+M: POST /v1/sse
    Note right of C: {"jsonrpc":"2.0","method":"initialize","id":1}
    M->>M: Validate Bearer token
    alt No Bearer Token
        M->>C: 401 Unauthorized
    else Valid Token
        M->>-C: SSE Stream
        Note left of M: data: {"type":"connected"}
        Note left of M: data: {"jsonrpc":"2.0","result":{"protocolVersion":"2024-11-05"}}
        Note left of M: data: {"type":"done"}
    end

    Note over C,M: Only after initialize succeeds:

    C->>+M: POST /v1/sse
    Note right of C: {"jsonrpc":"2.0","method":"tools/list","id":2}
    M->>-C: SSE Stream with Tools
    Note left of M: data: {"result":{"tools":[...]}}

    C->>+M: POST /v1/sse
    Note right of C: {"jsonrpc":"2.0","method":"tools/call","id":3}
    M->>-C: SSE Stream with Result
    Note left of M: data: {"result":{"content":[...]}}
```

### Error Scenarios & Fixes

```mermaid
sequenceDiagram
    participant C as Claude.ai
    participant M as MCP Server

    Note over C,M: Common Error Scenarios

    rect rgb(255, 200, 200)
        Note over C,M: ERROR 1: Missing Initialize Handler
        C->>+M: {"method":"initialize"}
        M->>-C: {"result":{}} (Empty response)
        C->>C: Connection fails - no capabilities
        Note over C: FIX: Add initialize method handler
    end

    rect rgb(255, 200, 200)
        Note over C,M: ERROR 2: Wrong SSE Format
        C->>+M: {"method":"tools/list"}
        M->>-C: {"result":{"tools":[...]}} (Plain JSON)
        C->>C: Protocol error - expects SSE
        Note over C: FIX: Prefix with "data: " and end with "\n\n"
    end

    rect rgb(255, 200, 200)
        Note over C,M: ERROR 3: Missing Authentication
        C->>+M: POST /v1/sse (no Authorization header)
        M->>-C: 401 Unauthorized
        Note over C: FIX: OAuth flow must complete first
    end

    rect rgb(200, 255, 200)
        Note over C,M: SUCCESS: Proper Sequence
        C->>+M: {"method":"initialize"} + Bearer token
        M->>-C: SSE: {"result":{"capabilities":{}}}
        C->>+M: {"method":"tools/list"}
        M->>-C: SSE: {"result":{"tools":[8 tools]}}
        Note over C: ✅ Connection successful
    end
```

## Implementation Timeline

### v1.2.0 Issues
- ❌ OAuth working but MCP connection failed
- ❌ Missing `initialize` method handler
- ❌ Claude.ai couldn't complete handshake

### v1.3.0 Fixes
- ✅ Added `/v1/sse` SSE endpoint
- ❌ Still missing `initialize` handler

### v1.3.1 Complete
- ✅ Added MCP `initialize` handshake
- ✅ Both `/v1/sse` and `/mcp` support full protocol
- ✅ Claude.ai integration working

## Key Learnings

1. **Initialize is Required**: Claude.ai MUST receive proper `initialize` response before proceeding
2. **SSE Format Critical**: All responses over `/v1/sse` must use `data: ` prefix
3. **Authentication Order**: OAuth must complete before MCP connection attempts
4. **Protocol Version**: Must match `2024-11-05` for compatibility
5. **Capabilities Declaration**: Server must declare `tools`, `resources`, `prompts`, `logging`

---

**Status**: ✅ All sequence diagrams validated
**Version**: 1.3.1
**Last Updated**: 2025-09-14