;;; project-status.el --- Batch operations for project status -*- lexical-binding: t -*-

;;; Commentary:
;; Batch mode operations for generating project status reports

;;; Code:

(require 'org)
(require 'org-agenda)

;; Load project configuration
(load-file "guile-changeflow.el")

(defun generate-project-report ()
  "Generate comprehensive project status report."
  (let ((report-file "PROJECT-STATUS.org")
        (todos '())
        (stats '((TODO . 0) (ASSIGNED . 0) (ACTIVE . 0) (BLOCKED . 0) (COMPLETE . 0) (DONE . 0))))

    ;; Setup agenda
    (guile-changeflow-setup-agenda)

    ;; Collect TODO items
    (dolist (file org-agenda-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (let ((todo-state (org-get-todo-state))
                   (heading (org-get-heading t t t t))
                   (file-name (file-name-nondirectory file)))
               (when todo-state
                 (push (list todo-state heading file-name) todos)
                 (cl-incf (alist-get (intern todo-state) stats 0)))))))))

    ;; Generate report
    (with-temp-file report-file
      (insert "#+TITLE: Project Status Report\n")
      (insert (format "#+DATE: %s\n\n" (format-time-string "[%Y-%m-%d %a]")))

      (insert "* Statistics\n\n")
      (insert "| State | Count |\n")
      (insert "|-------+-------|\n")
      (dolist (state '(TODO ASSIGNED ACTIVE BLOCKED COMPLETE DONE))
        (insert (format "| %s | %d |\n" state (alist-get state stats 0))))
      (insert "\n")

      (insert "* Active Tasks\n\n")
      (dolist (item (reverse todos))
        (when (member (car item) '("ASSIGNED" "ACTIVE" "!"))
          (insert (format "** %s %s\n   From: %s\n" (car item) (cadr item) (caddr item)))))

      (insert "\n* Blocked Items\n\n")
      (dolist (item (reverse todos))
        (when (string= (car item) "BLOCKED")
          (insert (format "** %s\n   From: %s\n" (cadr item) (caddr item)))))

      (insert "\n* Completed Items\n\n")
      (dolist (item (reverse todos))
        (when (member (car item) '("COMPLETE" "DONE"))
          (insert (format "** %s\n   From: %s\n" (cadr item) (caddr item))))))

    (message "Report generated: %s" report-file)))

(defun generate-agent-status ()
  "Generate agent status summary."
  (let ((agents '(("core-models" . "gcf-a1")
                  ("mcp-server" . "gcf-a2")
                  ("risk-engine" . "gcf-a3")
                  ("web-interface" . "gcf-a4")
                  ("integrations" . "gcf-a5")))
        (status-file "AGENT-STATUS.org"))

    (with-temp-file status-file
      (insert "#+TITLE: Agent Status Summary\n")
      (insert (format "#+DATE: %s\n\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))

      (insert "* Agent Worktrees\n\n")
      (insert "| Agent | Worktree | Branch | Session |\n")
      (insert "|-------+----------+--------+---------|\n")

      (dolist (agent agents)
        (let* ((name (car agent))
               (session (cdr agent))
               (worktree-path (format "../gcf-%s" name))
               (branch (format "feat/%s" name)))
          (insert (format "| %s | %s | %s | %s |\n"
                          name worktree-path branch session))))

      (insert "\n* Agent Instructions\n\n")
      (dolist (agent agents)
        (let* ((name (car agent))
               (inst-file (format "../gcf-%s/INSTRUCTIONS.org" name)))
          (when (file-exists-p inst-file)
            (insert (format "** %s\n" name))
            (insert (format "   [[file:%s][Instructions]]\n" inst-file))))))

    (message "Agent status generated: %s" status-file)))

;; Run if called from batch mode
(when noninteractive
  (generate-project-report)
  (generate-agent-status)
  (message "Status reports generated successfully"))

(provide 'project-status)
;;; project-status.el ends here