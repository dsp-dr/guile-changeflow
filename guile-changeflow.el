;;; guile-changeflow.el --- Project management for Guile ChangeFlow -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs configuration for managing the Guile ChangeFlow project
;; Provides org-agenda integration, project navigation, and development tools

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'project)

;; Project root
(defvar guile-changeflow-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of Guile ChangeFlow project.")

;; Org files for agenda
(defvar guile-changeflow-org-files
  '("README.org"
    "setup.org"
    "design.org"
    "itil-requirements.org"
    "mcp-protocol.org"
    "deployment.org"
    "tracking.org"
    "sprint-plan.org"
    "demo-requirements.org"
    "integration-test-plan.org"
    "agent-coordinator.org"
    "experiments/README.org"
    "experiments/000-core-tools/README.org")
  "List of org files for agenda.")

;; Set up org-agenda files
(defun guile-changeflow-setup-agenda ()
  "Set up org-agenda for Guile ChangeFlow project."
  (interactive)
  (setq org-agenda-files
        (mapcar (lambda (file)
                  (expand-file-name file guile-changeflow-root))
                guile-changeflow-org-files))
  (message "Guile ChangeFlow agenda files configured: %d files"
           (length org-agenda-files)))

;; Custom agenda views
(defun guile-changeflow-setup-custom-agenda ()
  "Set up custom agenda views for the project."
  (setq org-agenda-custom-commands
        '(("p" "Project Overview"
           ((agenda "" ((org-agenda-span 7)))
            (todo "ASSIGNED|ACTIVE"
                  ((org-agenda-overriding-header "Active Tasks")))
            (todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked Items")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Backlog")))))

          ("s" "Sprint Status"
           ((todo "ASSIGNED|ACTIVE|!"
                  ((org-agenda-files (list (expand-file-name "sprint-plan.org" guile-changeflow-root)))
                   (org-agenda-overriding-header "Sprint Tasks")))
            (todo "COMPLETE"
                  ((org-agenda-files (list (expand-file-name "sprint-plan.org" guile-changeflow-root)))
                   (org-agenda-overriding-header "Completed Sprint Tasks")))))

          ("a" "Agent Status"
           ((todo "ASSIGNED|ACTIVE"
                  ((org-agenda-files (list (expand-file-name "agent-coordinator.org" guile-changeflow-root)))
                   (org-agenda-overriding-header "Agent Tasks")))
            (todo "BLOCKED"
                  ((org-agenda-files (list (expand-file-name "agent-coordinator.org" guile-changeflow-root)))
                   (org-agenda-overriding-header "Agent Blockers")))))

          ("d" "Demo Requirements"
           ((todo "TODO|ASSIGNED"
                  ((org-agenda-files (list (expand-file-name "demo-requirements.org" guile-changeflow-root)))
                   (org-agenda-overriding-header "Demo Requirements")))))))

;; Project navigation
(defun guile-changeflow-find-file ()
  "Find file in Guile ChangeFlow project."
  (interactive)
  (let ((default-directory guile-changeflow-root))
    (call-interactively 'project-find-file)))

;; Agent monitoring
(defun guile-changeflow-monitor-agents ()
  "Open agent monitor in terminal."
  (interactive)
  (let ((default-directory guile-changeflow-root))
    (compile "./monitor-agents.sh")))

;; Worktree management
(defun guile-changeflow-list-worktrees ()
  "List all project worktrees."
  (interactive)
  (let ((default-directory guile-changeflow-root))
    (shell-command "git worktree list")))

(defun guile-changeflow-visit-agent (agent)
  "Visit AGENT worktree.
AGENT should be one of: core-models, mcp-server, risk-engine, web-interface, integrations"
  (interactive
   (list (completing-read "Agent: "
                          '("core-models" "mcp-server" "risk-engine" "web-interface" "integrations"))))
  (let ((worktree-path (expand-file-name (format "../gcf-%s" agent)
                                          guile-changeflow-root)))
    (if (file-directory-p worktree-path)
        (dired worktree-path)
      (message "Worktree not found: %s" worktree-path))))

;; GitHub integration
(defun guile-changeflow-create-issue (title body)
  "Create GitHub issue using gh CLI."
  (interactive "sIssue title: \nsIssue body: ")
  (let ((default-directory guile-changeflow-root))
    (shell-command
     (format "gh issue create --title %s --body %s"
             (shell-quote-argument title)
             (shell-quote-argument body)))))

;; Export functions for batch mode
(defun guile-changeflow-export-agenda ()
  "Export agenda to text file."
  (guile-changeflow-setup-agenda)
  (guile-changeflow-setup-custom-agenda)
  (org-batch-agenda "p" org-agenda-write "agenda.txt" 'txt))

(defun guile-changeflow-export-todo-stats ()
  "Export TODO statistics."
  (guile-changeflow-setup-agenda)
  (let ((stats '()))
    (dolist (file org-agenda-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (let ((todo-state (org-get-todo-state)))
               (when todo-state
                 (push todo-state stats))))))))
    (with-temp-file (expand-file-name "todo-stats.txt" guile-changeflow-root)
      (insert "TODO Statistics\n")
      (insert "===============\n\n")
      (dolist (state '("TODO" "ASSIGNED" "ACTIVE" "BLOCKED" "COMPLETE" "DONE"))
        (let ((count (cl-count state stats :test #'string=)))
          (insert (format "%s: %d\n" state count)))))))

;; Magit Forge setup (if available)
(when (require 'forge nil t)
  (defun guile-changeflow-setup-forge ()
    "Set up Magit Forge for GitHub issues."
    (interactive)
    (let ((default-directory guile-changeflow-root))
      (forge-add-repository)))

  (defun guile-changeflow-list-issues ()
    "List GitHub issues using Forge."
    (interactive)
    (let ((default-directory guile-changeflow-root))
      (forge-list-issues))))

;; Key bindings
(defvar guile-changeflow-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p a") 'guile-changeflow-setup-agenda)
    (define-key map (kbd "C-c p p") 'guile-changeflow-find-file)
    (define-key map (kbd "C-c p m") 'guile-changeflow-monitor-agents)
    (define-key map (kbd "C-c p w") 'guile-changeflow-list-worktrees)
    (define-key map (kbd "C-c p v") 'guile-changeflow-visit-agent)
    (define-key map (kbd "C-c p i") 'guile-changeflow-create-issue)
    map)
  "Keymap for Guile ChangeFlow project commands.")

;; Minor mode
(define-minor-mode guile-changeflow-mode
  "Minor mode for Guile ChangeFlow project management."
  :lighter " GCF"
  :keymap guile-changeflow-mode-map)

;; Auto-enable in project files
(defun guile-changeflow-maybe-enable ()
  "Enable guile-changeflow-mode if in project."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name guile-changeflow-root)
                              (expand-file-name buffer-file-name)))
    (guile-changeflow-mode 1)))

(add-hook 'find-file-hook 'guile-changeflow-maybe-enable)

;; Initialize on load
(when (not noninteractive)
  (guile-changeflow-setup-agenda)
  (guile-changeflow-setup-custom-agenda)
  (message "Guile ChangeFlow project management loaded"))

;;; Project Status Functions (from project-status.el)

(defun guile-changeflow-generate-project-report ()
  "Generate comprehensive project status report."
  (let ((report-file "PROJECT-STATUS.org")
        (todos '())
        (stats '((TODO . 0) (ASSIGNED . 0) (ACTIVE . 0) (BLOCKED . 0) (COMPLETE . 0) (DONE . 0))))

    (guile-changeflow-setup-agenda)

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

(defun guile-changeflow-generate-agent-status ()
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

;; Batch mode support for status reports
(when noninteractive
  (when (member "--project-report" command-line-args)
    (guile-changeflow-generate-project-report)
    (message "Project report generated"))
  (when (member "--agent-status" command-line-args)
    (guile-changeflow-generate-agent-status)
    (message "Agent status generated")))

(provide 'guile-changeflow)

;;; guile-changeflow.el ends here