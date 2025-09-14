(define-module (json)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (scm->json-string
            json-string->scm))

;; Minimal JSON implementation for MCP protocol
;; Handles basic JSON operations needed for the demo

(define (scm->json-string obj)
  "Convert Scheme object to JSON string"
  (cond
    ((null? obj) "null")
    ((boolean? obj) (if obj "true" "false"))
    ((number? obj) (number->string obj))
    ((string? obj) (format #f "~s" obj)) ; Basic string escaping
    ((list? obj)
     (if (and (not (null? obj))
              (symbol? (car obj)))
         ;; Association list -> JSON object
         (string-append "{"
                        (string-join
                         (map (lambda (pair)
                                (format #f "~s:~a"
                                        (symbol->string (car pair))
                                        (scm->json-string (cdr pair))))
                              obj)
                         ",")
                        "}")
         ;; Regular list -> JSON array
         (string-append "["
                        (string-join
                         (map scm->json-string obj)
                         ",")
                        "]")))
    ((pair? obj)
     ;; Simple pair -> object with single key
     (format #f "{~s:~a}"
             (if (symbol? (car obj))
                 (symbol->string (car obj))
                 (car obj))
             (scm->json-string (cdr obj))))
    (else
     (format #f "~s" (format #f "~a" obj)))))

(define (json-string->scm json-str)
  "Convert JSON string to Scheme object"
  ;; Basic JSON parser for demo purposes
  ;; This is a simplified implementation
  (cond
    ((string=? json-str "null") '())
    ((string=? json-str "true") #t)
    ((string=? json-str "false") #f)
    ((string-match "^\".*\"$" json-str)
     ;; String literal - remove quotes
     (substring json-str 1 (- (string-length json-str) 1)))
    ((string-match "^[0-9.-]+$" json-str)
     ;; Number
     (string->number json-str))
    ((string-match "^\\[.*\\]$" json-str)
     ;; Array - simplified parsing
     (let ((content (substring json-str 1 (- (string-length json-str) 1))))
       (if (string=? content "")
           '()
           ;; Very basic array parsing
           (map json-string->scm
                (map string-trim-both
                     (string-split content #\,))))))
    ((string-match "^\\{.*\\}$" json-str)
     ;; Object - simplified parsing
     (let ((content (substring json-str 1 (- (string-length json-str) 1))))
       (if (string=? content "")
           '()
           ;; Very basic object parsing for key:value pairs
           (map (lambda (pair-str)
                  (let ((parts (string-split pair-str #\:)))
                    (if (= (length parts) 2)
                        (cons (string->symbol
                               (string-trim-both
                                (string-trim-both (car parts) #\")
                                #\"))
                              (json-string->scm
                               (string-trim-both (cadr parts))))
                        (cons 'error "Invalid JSON pair"))))
                (string-split content #\,)))))
    (else
     ;; Fallback: return as string
     json-str)))