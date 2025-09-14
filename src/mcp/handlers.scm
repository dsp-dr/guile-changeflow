(define-module (mcp handlers)
  #:use-module (json)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (mcp tools)
  #:use-module (ice-9 textual-ports)
  #:export (tools-list-handler
            tool-invoke-handler))

(define (tools-list-handler request)
  "Handle requests for tool list"
  (let ((response-data
         `((tools . #(((name . "create_change_request")
                      (description . "Create a new change request"))
                     ((name . "assess_risk")
                      (description . "Assess risk for a change request")))))))
    (values '((content-type . (application/json))
              (access-control-allow-origin . "*"))
            (scm->json-string response-data))))

(define (extract-tool-name path)
  "Extract tool name from path like /tools/create_change_request/invoke"
  (let* ((parts (string-split path #\/))
         (tool-part (find (lambda (part)
                           (and (not (string=? part ""))
                                (not (string=? part "tools"))
                                (not (string=? part "invoke"))))
                         parts)))
    tool-part))

(define (parse-request-body body)
  "Parse JSON request body"
  (if (string? body)
      (json-string->scm body)
      (json-string->scm (get-string-all body))))

(define (tool-invoke-handler request body)
  "Handle tool invocation requests"
  (let* ((path (uri-path (request-uri request)))
         (tool-name (extract-tool-name path))
         (request-data (parse-request-body body))
         (params (assoc-ref request-data 'params)))

    (let ((result
           (cond
             ((string=? tool-name "create_change_request")
              (create-change-request-tool params))
             ((string=? tool-name "assess_risk")
              (assess-risk-tool params))
             (else
              `((success . #f)
                (error . "Unknown tool")
                (tool . ,tool-name))))))

      (values '((content-type . (application/json))
                (access-control-allow-origin . "*")
                (access-control-allow-methods . "POST, OPTIONS")
                (access-control-allow-headers . "Content-Type"))
              (scm->json-string result)))))