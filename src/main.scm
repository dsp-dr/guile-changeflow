(define-module (main)
  #:use-module (mcp server)
  #:export (main))

(define (main args)
  "Main entry point for the MCP server"
  (display "Guile ChangeFlow MCP Server starting...\n")
  (display "Server will be available at http://localhost:8081\n")
  (display "Discovery endpoint: http://localhost:8081/.well-known/mcp\n")
  (display "Press Ctrl+C to stop the server.\n\n")

  (start-mcp-server))