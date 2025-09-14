#!/usr/bin/env guile
!#

(add-to-load-path "src")
(use-modules (risk calculator))

;; Simple test
(display "Testing risk calculator:\n")
(display (assess-risk "Production database migration"
                     "Critical security patch"
                     '("payment-gateway" "database-master")))
(newline)