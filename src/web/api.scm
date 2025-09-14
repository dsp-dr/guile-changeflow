(define-module (web api)
  #:use-module (web simple-json)
  #:use-module (web response)
  #:use-module (srfi srfi-19)
  #:export (api-changes-handler get-mock-changes))

;; Mock change data for demonstration
(define mock-changes
  (list
    (list
      (cons 'id "CHG-001")
      (cons 'title "Database Schema Update")
      (cons 'description "Update user table to include new authentication fields")
      (cons 'riskScore 85)
      (cons 'riskCategory "high")
      (cons 'status "submitted")
      (cons 'createdAt "2025-09-13T10:30:00Z"))
    (list
      (cons 'id "CHG-002")
      (cons 'title "Frontend API Integration")
      (cons 'description "Connect React components to new user management API")
      (cons 'riskScore 45)
      (cons 'riskCategory "medium")
      (cons 'status "approved")
      (cons 'createdAt "2025-09-13T09:15:00Z"))
    (list
      (cons 'id "CHG-003")
      (cons 'title "Configuration File Update")
      (cons 'description "Update production environment variables for new feature")
      (cons 'riskScore 25)
      (cons 'riskCategory "low")
      (cons 'status "assessing")
      (cons 'createdAt "2025-09-13T11:45:00Z"))
    (list
      (cons 'id "CHG-004")
      (cons 'title "Critical Security Patch")
      (cons 'description "Apply urgent security patches to authentication system")
      (cons 'riskScore 95)
      (cons 'riskCategory "critical")
      (cons 'status "submitted")
      (cons 'createdAt "2025-09-13T14:20:00Z"))
    (list
      (cons 'id "CHG-005")
      (cons 'title "Documentation Update")
      (cons 'description "Update API documentation with new endpoints")
      (cons 'riskScore 15)
      (cons 'riskCategory "low")
      (cons 'status "approved")
      (cons 'createdAt "2025-09-13T08:00:00Z"))))

(define (get-mock-changes)
  "Return mock changes data - will be replaced by real storage integration"
  mock-changes)

(define (categorize-risk score)
  "Categorize risk score into levels"
  (cond
    ((>= score 90) "critical")
    ((>= score 70) "high")
    ((>= score 40) "medium")
    (else "low")))

(define (api-changes-handler)
  "Return all changes as JSON"
  (let* ((changes (get-mock-changes))
         (changes-data `((changes . ,changes)
                        (total . ,(length changes))
                        (lastUpdated . ,(date->string (current-date) "~Y-~m-~dT~H:~M:~SZ"))
                        (status . "ok"))))

    (values (build-response
             #:code 200
             #:headers '((content-type . (application/json))
                         (access-control-allow-origin . "*")))
            (scm->json-string changes-data))))