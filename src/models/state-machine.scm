(define-module (models state-machine)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (models change-request)
  #:export (valid-transition?
            transition-state
            get-next-states
            get-all-states
            transition-change-state!
            state-machine-definition))

(define state-machine-definition
  '((submitted . (assessing cancelled))
    (assessing . (approved rejected needs-info))
    (approved . (implementing))
    (needs-info . (assessing cancelled))
    (implementing . (completed failed))
    (rejected . ())
    (cancelled . ())
    (completed . ())
    (failed . (implementing rejected))))

(define (valid-transition? from-state to-state)
  "Check if transition from FROM-STATE to TO-STATE is valid"
  (let ((allowed-states (assoc-ref state-machine-definition from-state)))
    (and allowed-states
         (member to-state allowed-states))))

(define (get-next-states current-state)
  "Get list of valid next states from CURRENT-STATE"
  (assoc-ref state-machine-definition current-state))

(define (get-all-states)
  "Get list of all possible states in the state machine"
  (append (map car state-machine-definition)
          (apply append (map cdr state-machine-definition))))

(define (transition-state from-state to-state)
  "Perform state transition validation and return new state if valid"
  (if (valid-transition? from-state to-state)
      to-state
      (error "Invalid state transition" from-state to-state)))

(define (transition-change-state! change new-state)
  "Transition a change request to a new state if valid"
  (let ((current-state (change-request-status change)))
    (if (valid-transition? current-state new-state)
        (begin
          (set-change-request-status! change new-state)
          (set-change-request-updated-at! change (current-time))
          new-state)
        (error "Invalid state transition for change"
               (change-request-id change)
               current-state
               new-state))))

(define (can-approve? current-state)
  "Check if current state allows approval"
  (eq? current-state 'assessing))

(define (can-reject? current-state)
  "Check if current state allows rejection"
  (eq? current-state 'assessing))

(define (is-terminal-state? state)
  "Check if state is terminal (no further transitions)"
  (null? (get-next-states state)))

(define (is-active-state? state)
  "Check if state represents an active change"
  (member state '(submitted assessing approved implementing needs-info)))