(define-module (web simple-json)
  #:use-module (srfi srfi-1)
  #:export (scm->json-string))

(define (string-join lst sep)
  "Join a list of strings with separator"
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (string-join (cdr lst) sep)))))

(define (escape-json-string str)
  "Escape special characters in JSON strings"
  (let loop ((chars (string->list str))
             (result '()))
    (if (null? chars)
        (list->string (reverse result))
        (let ((c (car chars)))
          (case c
            ((#\") (loop (cdr chars) (append '(#\" #\\) result)))
            ((#\\) (loop (cdr chars) (append '(#\\ #\\) result)))
            ((#\newline) (loop (cdr chars) (append '(#\n #\\) result)))
            ((#\return) (loop (cdr chars) (append '(#\r #\\) result)))
            ((#\tab) (loop (cdr chars) (append '(#\t #\\) result)))
            (else (loop (cdr chars) (cons c result))))))))

(define (scm->json-string obj)
  "Convert Scheme data to JSON string"
  (cond
    ((null? obj) "null")
    ((boolean? obj) (if obj "true" "false"))
    ((number? obj) (number->string obj))
    ((string? obj) (string-append "\"" (escape-json-string obj) "\""))
    ((symbol? obj) (string-append "\"" (escape-json-string (symbol->string obj)) "\""))
    ((list? obj)
     (if (and (pair? obj) (pair? (car obj)))
         ;; It's an alist (object)
         (string-append "{"
                        (string-join
                         (map (lambda (pair)
                                (string-append
                                 (scm->json-string (if (symbol? (car pair))
                                                      (symbol->string (car pair))
                                                      (car pair)))
                                 ":"
                                 (scm->json-string (cdr pair))))
                              obj)
                         ",")
                        "}")
         ;; It's a regular list (array)
         (string-append "["
                        (string-join
                         (map scm->json-string obj)
                         ",")
                        "]")))
    (else "null")))