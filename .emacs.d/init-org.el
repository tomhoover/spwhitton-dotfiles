;;; init-org --- Sean's Org-mode configuration

;;; Commentary:

;;; Code:

;;;; ---- packages ----

;;; this config uses `f-glob'

(use-package f)

;;; with the new exporter in Org version 8, must explicitly require
;;; the exporters I want to use

(use-package ox-html)
(use-package ox-latex)
(use-package ox-ascii)
(use-package ox-odt)

;;; checklist helper functions including automatic resetting

(use-package org-checklist :load-path "/usr/share/org-mode/lisp")

;;; inline tasks

(use-package org-inlinetask)

;;; links to mairix messages by message-id in Org

;;(use-package org-mairix-el
;;  :bind ("C-c m" . org-mairix-el-insert-link)
;;  :commands (org-mairix-el-insert-link org-mairix-el-link org-mairix-el-open))

;;; highlight current sentence

;; This is in ~/.emacs.d/site-lisp because its copyright status is a
;; bit murky.  The code comes from the Emacs Wiki, but was the
;; "everything on Emacs Wiki is GPL" notice present when the code was
;; first posted?  TODO find out, and package for Debian if possible

(use-package hl-sentence
  :commands hl-sentence-mode
  :demand
  :config
  (add-hook 'org-mode-hook 'hl-sentence-mode)
  (set-face-background 'hl-sentence-face "#4F4F4F"))

;;; graphical calendar

(use-package calfw
  :disabled t
  :init (load-library "calfw-org"))

(defun spw/org-calfw ()
  (interactive)
  (let ((org-deadline-warning-days 0)
        (org-agenda-files (list "~/doc/org/diary.org")))
    (save-window-excursion
      (cfw:open-org-calendar)
      (switch-to-buffer "*cfw-calendar*")
      (call-interactively 'htmlize-buffer)
      (write-file "~/tmp/calendar.html"))))



;;;; ---- preferences ----

;; diminish `org-indent-mode' lighter

(require 'diminish)
(require 'org-indent)
(diminish 'org-indent-mode)

;; custom doesn't actually set all the faces it should, so we'll do
;; some manually
;;(set-face-foreground 'org-hide "#3f3f3f")
(ignore-errors (set-face-font 'org-hide "Terminus-11"))

(setq
 org-alphabetical-lists t
 org-startup-indented 1
 org-indent-indentation-per-level 4
 org-adapt-indentation nil
 org-directory "~/doc/org"

 org-tag-alist '((:startgroup)
                 ("@Tucson"       . ?T)
                 ("@campus"       . ?C)
                 ("@Sheffield"    . ?S)
                 ("@CClubRd"      . ?H)
                 (:endgroup)
                 ("@iPad"         . ?I)
                 (:startgroup)
                 ("@Emacs"        . ?E) ; SSH Emacs only
                 ("@workstation"  . ?M) ; on my fully set-up personal (m)achine
                 (:endgroup)
                 ("UA"            . ?W)) ; academic work

 ;; enable speed commands and bind N to narrow to subtree
 org-use-speed-commands t
 org-speed-commands-user '(("N" . org-narrow-to-subtree)
                           ("h" . hide-other)
                           ("k" . org-kill-note-or-show-branches)
                           ("s" . org-save-all-org-buffers)
                           ("z" . org-add-note)
                           ("c" . self-insert-command)
                           ("C" . self-insert-command)
                           ("1" . delete-other-windows)
                           ("2" . split-window-vertically)
                           ("3" . split-window-horizontally)
                           ("m" . org-mark-subtree))
 org-agenda-include-all-todo nil
 org-agenda-files "~/doc/emacs-org-agenda-files"
 org-agenda-persistent-filter t
 org-agenda-diary-file "~/.labbook.gpg"
 org-agenda-insert-diary-strategy 'date-tree
 org-goto-auto-isearch t
 org-goto-interface 'outline
 org-archive-mark-done nil
 org-archive-save-context-info '(time file olpath)
 org-archive-location "~/doc/org/archive/archive.org::* From %s"
 org-cycle-global-at-bob t
 org-startup-folded t

 ;; using indirect buffers for DnD and for now want them in their own
 ;; frames (use C-u)
 org-indirect-buffer-display 'dedicated-frame
 org-agenda-dim-blocked-tasks nil
 org-stuck-projects (quote ("" nil nil ""))
 org-hide-emphasis-markers nil
 org-use-fast-todo-selection t
 org-treat-S-cursor-todo-selection-as-state-change nil
 org-treat-insert-todo-heading-as-state-change t
 org-fast-tag-selection-include-todo t
 org-show-entry-below (quote ((default)))
 org-log-into-drawer t
 ;; org-log-state-notes-insert-after-drawers t
 org-log-states-order-reversed nil
 org-log-done t
 ;; org-log-redeadline 'note
 ;; org-log-reschedule 'time
 org-log-redeadline nil
 org-log-reschedule nil
 ;; org-log-refile 'time
 org-log-refile nil
 org-enforce-todo-dependencies t
 org-cycle-separator-lines 2
 org-blank-before-new-entry (quote ((heading)
                                    (plain-list-item)))
 org-insert-heading-respect-content nil
 org-reverse-note-order nil
 org-show-following-heading t
 org-show-siblings t
 org-show-hierarchy-above t
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-yank-adjusted-subtrees t
 org-yank-folded-subtrees nil
 org-id-method (quote uuidgen)
 org-deadline-warning-days 60
 org-return-follows-link nil
 org-display-internal-link-with-indirect-buffer t
 org-remove-highlights-with-change nil
 org-M-RET-may-split-line '((default . nil))
 org-table-export-default-format "orgtbl-to-csv"
 org-link-frame-setup '((gnus . gnus-other-frame)
                        (vm . vm-visit-folder-other-frame)
                        (file . find-file-other-window))

 org-remove-highlights-with-change nil
 org-read-date-prefer-future t
 ;; TODO needs updating for Org-mode version 9 (see changelog)
 ;; org-file-apps (quote ((auto-mode . emacs)
 ;;                       ("\\.mm\\'" . system)
 ;;                       ("\\.x?html?\\'" . system)
 ;;                       ("\\.pdf\\'" . system)
 ;;                       ("\\.jpg\\'" . "/usr/bin/feh %s")
 ;;                       ("\\.png\\'" . "/usr/bin/feh %s")
 ;;                       ("\\.gif\\'" . "/usr/bin/feh %s")))
 org-list-demote-modify-bullet (quote (("-" . "+")
                                       ("+" . "*")
                                       ("*" . "-")
                                       ("1." . "-")
                                       ("1)" . "-")))
 org-list-use-circular-motion t
 org-M-RET-may-split-line '((default . t))
 org-agenda-todo-ignore-with-date nil
 org-agenda-todo-ignore-deadlines nil
 org-agenda-todo-ignore-scheduled 'future
 org-agenda-todo-list-sublevels nil
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
 org-agenda-skip-additional-timestamps-same-entry nil
 org-agenda-skip-timestamp-if-done t
 org-agenda-start-on-weekday nil
 org-tags-match-list-sublevels t
 org-agenda-persistent-filter t
 org-agenda-skip-deadline-prewarning-if-scheduled 3
 org-agenda-window-setup 'other-window
 org-agenda-entry-text-maxlines 3

 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
   (sequence "WAITING(w@)" "SOMEDAY(s)" "|" "CANCELLED(c)"))

 org-todo-keyword-faces '(("SOMEDAY" . (:foreground "#94BFF3" :weight bold)) ; zenburn-blue+1
                          ("NEXT" . (:foreground "#F0DFAF" :weight bold))) ; zenburn-yellow

 ;; Include agenda archive files when searching for things
 org-agenda-text-search-extra-files (quote (agenda-archives))

 ;; weekends in a different colour
 org-agenda-date-weekend t
 org-default-notes-file (concat org-directory "/refile.org")
 org-completion-use-ido t

 ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
 org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))

 ;; Targets start with the file name - allows creating level 1 tasks
 org-refile-use-outline-path (quote file)

 ;; This has to be nil to work with helm (http://lists.gnu.org/archive/html/emacs-orgmode/2014-06/msg00846.html)
 ;; org-outline-path-complete-in-steps nil

 ;; Allow refile to create parent tasks with confirmation
 org-refile-allow-creating-parent-nodes (quote confirm)

 ido-enable-tramp-completion t
 ido-confirm-unique-completion nil
 ido-show-dot-for-dired nil
 org-export-with-LaTeX-fragments t
 ;; org-export-initial-scope 'subtree
 org-html-inline-images 'maybe ; need this to export images correctly for PyBlosxom
 ;; doesn't appear to work atm (possibly being cancelled out by
 ;; org-export-date-timestamp-format)
 org-html-metadata-timestamp-format "%A %Y-%m-%d"
 org-latex-pdf-process '("texi2dvi --pdf --clean --batch %f" "rm %f" "rm -rf auto")
 org-export-date-timestamp-format "%e %B %Y"
 org-html-footnotes-section "<h3>%s</h3>\n%s"
 org-export-with-smart-quotes t
 org-export-htmlize-output-type 'css

 org-latex-default-class "wordlike"

 org-export-headline-levels 3   ; set to 2 for spwoutline

 ;; org-export-latex-low-levels '("\\begin{lowitemize}\\setlength{\\parindent}{2em}" "\\end{lowitemize}" "\\item \\textbf{%s}\\indent %s")

 ;; used after things like e.g. to prevent a double space
 org-entities-user '(("space" "\\ " nil " " " " " " " "))

 org-export-with-toc nil         ; default to no table of contents
 org-footnote-section "Notes")

;; Org setting bookmarks that I never use makes for git merge
;; conflicts that I don't need to spend time resolving.  Eventually I
;; plan to write a git merge driver to do it for me but until then,
;; just don't set the bookmarks.
(setq org-bookmark-names-plist nil)

;;; add all my notes files to Org text search (e.g. C-c a /)

;; first clear out the (agenda-archives) from such searches; I only
;; have one so can easily search it manually
(setq org-agenda-text-search-extra-files nil)

(when (file-exists-p abbrev-file-name)
  (with-current-buffer (find-file-noselect org-agenda-files)
    (dolist (file (directory-files org-directory nil "\\.org$" t))
      (save-excursion
        (goto-char (point-min))
        ;; search for the file name in ~/doc/org-agenda-files.  If it's
        ;; there, don't add to the list: agenda files are included in
        ;; text searches automatically
        (if (not (search-forward file nil t))
            (add-to-list 'org-agenda-text-search-extra-files (concat "~/doc/org/" file)))))))

;; remove my massive archive files; can search that manually if
;; necessary
(dolist (file (f-glob "~/doc/org/archive/archive*.org"))
  (delete file org-agenda-text-search-extra-files))

;;; Org habit

(use-package org-habit)
;; (setq org-habit-graph-column nil)

;;;; ---- agenda and refile ----

(defun org-agenda--switch-projectile-project (&rest ignore)
  "Switch perspective project before compiling the agenda."
  ;; don't do it if we're already there so that the buffer we're
  ;; switching from remains top of the buffer list
  (unless
      (ignore-errors (string= "~/doc/" (abbreviate-file-name (projectile-project-root))))
    (projectile-switch-project-by-name "~/doc")))
;; (advice-add 'org-agenda :before #'org-agenda--switch-projectile-project)

(setq
 org-agenda-custom-commands
 '(("a" "Primary agenda view"
    ((agenda "day" ((org-agenda-span 'day)
                    (org-agenda-overriding-header
                     "Tasks, appointments and waiting tasks to be chased today")
                    (org-agenda-include-deadlines nil)
                    (org-agenda-time-grid nil))))
    ((org-agenda-start-with-log-mode nil)
     ;; (org-agenda-tag-filter-preset '("-Sariul"))
     (org-agenda-start-with-follow-mode nil))
    ("/var/www/spw/org/agenda.html" "/tmp/dionysus/Agenda/Today's agenda.html"))
   ("A" "Daily planning view"
    ((agenda "day" ((org-agenda-span 'day)
                    (org-agenda-time-grid nil)
                    (org-agenda-overriding-header "Plan for today & upcoming deadlines")))
     (todo "TODO|NEXT" ((org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines 'far)
                        (org-agenda-overriding-header "Unscheduled standalone tasks & project next actions")
                        (org-agenda-skip-function 'spw/skip-non-actionable)))
     (agenda "" ((org-agenda-span 3)
                 (org-agenda-start-day "+1d")
                 (org-agenda-time-grid nil)
                 (org-agenda-repeating-timestamp-show-all t)
                 (org-agenda-entry-types '(:timestamp :sexp))
                 (org-agenda-show-all-dates nil)
                 (org-agenda-overriding-header "Coming up")
                 (org-agenda-files (quote ("~/doc/org/diary.org"))))))
    nil
    ("/var/www/spw/org/full.html" "/tmp/dionysus/Agenda/Day-planning agenda.html"))
   ("#" "Weekly review view"
    ((todo "WAITING" ((org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines nil)
                      (org-agenda-todo-ignore-with-date nil)
                      (org-agenda-overriding-header "Waiting on others & not scheduled to chase up")))
     (todo "TODO|NEXT" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-overriding-header "Stuck projects")
                        (org-agenda-skip-function 'spw/skip-non-stuck-projects)))
     (tags "LEVEL=1+REFILE"
           ((org-agenda-todo-ignore-with-date nil)
            (org-agenda-todo-ignore-deadlines nil)
            (org-agenda-todo-ignore-scheduled nil)
            (org-agenda-overriding-header "Items to add context and priority, and refile")
            (org-agenda-start-with-entry-text-mode t)))

     ;; This view shows *only top-level* TODOs (i.e. projects) that
     ;; are complete (and that, for safety, contain no incomplete
     ;; (sub)projects or tasks).  Sometimes I want to archive complete
     ;; subprojects of very large projects that are not yet complete,
     ;; but I don't want to have to make that decision when looking at
     ;; my review agenda.  I can archive these as required.
     (todo "DONE|CANCELLED"
           ((org-agenda-overriding-header "Tasks to be archived")
            (org-agenda-todo-ignore-scheduled nil)
            (org-agenda-todo-ignore-deadlines nil)
            (org-agenda-todo-ignore-with-date nil)
            (org-agenda-tag-filter-preset '("-APPT"))
            (org-agenda-skip-function
             'spw/skip-incomplete-projects-and-all-subprojects)))))

   ("d" "Six-month diary" agenda ""
    ((org-agenda-span 180)
     ;; (org-agenda-start-on-weekday 1)
     (org-agenda-time-grid nil)
     (org-agenda-repeating-timestamp-show-all t)
     (org-agenda-entry-types '(:timestamp :sexp))
     (org-agenda-show-all-dates nil)
     (org-agenda-overriding-header "Sean's diary for the next six months")
     (org-agenda-files (quote ("~/doc/org/diary.org"))))
    ("/var/www/spw/org/diary.html" "/tmp/dionysus/Agenda/Six month diary.html"))))

;;; sensible automatic tag filtering

(defun org-my-auto-exclude-function (tag)
  (and
   (cond
    ;; tags passed to org-agenda-auto-exclude-function always
    ;; lower case per version Org 6.34 changelog
    ((string= tag "@cclubrd")
     (not (string= (system-name) "artemis")))
    ((string= tag "@tucson")
     (not (string= (system-name) "artemis")))
    ((string= tag "@sheffield")
     (not (string= (system-name) "zephyr")))
    ((string= tag "@tucson")
     (not (or (string= (system-name) "iris")
              (string= (system-name) "hephaestus"))))
    ((string= tag "@campus")
     (string= (system-name) "athena"))
    ((string= tag "ua")
     (= (calendar-day-of-week (calendar-current-date)) 6))
    ((string= tag "@workstation")
     (not (or (string= (system-name) "iris")
              (string= (system-name) "zephyr")
              (string= (system-name) "hephaestus")))))
   (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'org-my-auto-exclude-function)

;;; agenda skipping functions

(defun spw/org-get-todo-keyword ()
  (let ((todo-state (save-match-data (ignore-errors (org-get-todo-state)))))
    (spw--strip-text-properties todo-state)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun spw/skip-subprojects ()
  "Skip trees that are subprojects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        next-headline
      nil)))

(defun spw/skip-non-actionable ()
  "Skip:
- standalone tasks with deadlines
- projects
- subtasks of projects that are not NEXT actions
- subtasks of SOMEDAY projects
- subtasks of WAITING projects
- subtasks of scheduled projects

In the last case, the idea is that if I've scheduled the project
then I intend to tackle all the NEXT actions on that date (or at
least the next chunk of them).  I've broken the project down into
NEXT actions but not for the purpose of handling them on
different occasions."
  ;; TODO probably better if it skipped only scheduled, not deadlined
  ;; projects: merely deadlined ones actionable
  (let ((next-headline
         ;; Catch the end of the buffer to ensure we never return nil,
         ;; since if the code below returns next-headline we need to
         ;; go forward
         (let ((try (save-excursion (outline-next-heading))))
           (if try
               try
             (save-excursion (forward-line 1) (point))))))
    (if (or (bh/is-project-p)
            (and (bh/is-subproject-p)
                 (or
                  (not (string= (spw/org-get-todo-keyword) "NEXT"))
                  (save-excursion
                    (org-up-heading-safe)
                    (or
                     (spw/org-is-scheduled-p)
                     (string= (spw/org-get-todo-keyword) "SOMEDAY")
                     (string= (spw/org-get-todo-keyword) "WAITING")))))
            (and (bh/is-task-p)
                 (spw/org-has-deadline-p)))
        next-headline
      nil)))

(defun spw/org-forward-heading ()
  (beginning-of-line)
  (let ((start (point)))
    (org-forward-heading-same-level 1 t)
    ;; Check if that failed to move us, which means we reached
    ;; the end of the buffer and so have to move ourselves to
    ;; avoid an infinite loop
    (if (eq (point) start)
        (outline-next-heading)
      (point))))

(defun spw/org-is-scheduled-p ()
  "A task that is scheduled"
  (let ((is-dated)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
        (regexp (org-re-timestamp 'scheduled)))
    (save-excursion
      ;; Ignore errors if we fail to expand a subtree because we're
      ;; before the first heading
      (ignore-errors (outline-show-subtree))
      (forward-line)
      (org-beginning-of-line)

      ;; if a task is scheduled and deadlined, and the DEADLINE: comes
      ;; before the SCHEDULED:, the regexp won't match (if the
      ;; DEADLINE: comes second, it will match).  So skip over
      ;; DEADLINE, if it appears
      (when (looking-at (org-re-timestamp 'deadline))
        (forward-sexp 3)
        (forward-char))

      (if (looking-at regexp)
          (setq is-dated t)))
    (and is-a-task is-dated)))

(defun spw/org-has-deadline-p ()
  "A task that has a deadline"
  (let ((is-dated)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
        (regexp (org-re-timestamp 'deadline)))
    (save-excursion
      ;; Ignore errors if we fail to expand a subtree because we're
      ;; before the first heading
      (ignore-errors (outline-show-subtree))
      (forward-line)
      (org-beginning-of-line)
      (if (looking-at regexp)
          (setq is-dated t)))
    (and is-a-task is-dated)))

(defun spw/org-is-scheduled-or-deadlined-p ()
  "A task that is scheduled or deadlined"
  (let ((is-dated)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
        (regexp (org-re-timestamp 'scheduled-or-deadline)))
    (save-excursion
      ;; Ignore errors if we fail to expand a subtree because we're
      ;; before the first heading
      (ignore-errors (outline-show-subtree))
      (forward-line)
      (org-beginning-of-line)
      (if (looking-at regexp)
          (setq is-dated t)))
    (and is-a-task is-dated)))

(defun spw/has-scheduled-or-deadlined-subproject-p ()
  "A task that has a scheduled or deadlined subproject"
  (let (has-scheduled-or-deadlined-subproject)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (while (ignore-errors (outline-next-heading))
          (if (spw/org-is-scheduled-or-deadlined-p)
              (setq has-scheduled-or-deadlined-subproject t)))))
    has-scheduled-or-deadlined-subproject))

(defun spw/has-next-action-p ()
  "A task that has a NEXT subproject"
  (let (has-next-subproject)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (while (ignore-errors (outline-next-heading))
          (if (string= (spw/org-get-todo-keyword) "NEXT")
              (setq has-next-subproject t)))))
    has-next-subproject))

(defun spw/has-incomplete-subproject-or-task-p ()
  "A task that has an incomplete subproject or task."
  (let (has-incomplete-subproject)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (while (ignore-errors (outline-next-heading))
          (unless
              (or (string= (spw/org-get-todo-keyword) "DONE")
                  (string= (spw/org-get-todo-keyword) "CANCELLED"))
            (setq has-incomplete-subproject t)))))
    has-incomplete-subproject))

(defun spw/skip-projects-with-scheduled-or-deadlined-subprojects ()
  "Skip projects that have subtasks, where at least one of those
  is scheduled or deadlined"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (spw/has-scheduled-or-deadlined-subproject-p)
        next-headline
      nil)))

(defun spw/skip-subprojects-and-projects-with-scheduled-or-deadlined-subprojects ()
  "Skip subprojects projects that have subtasks, where at least
  one of those is scheduled or deadlined.  Currently fails to
  exclude subprojects that are the very last headline in a
  buffer"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (or (bh/is-subproject-p) (spw/has-scheduled-or-deadlined-subproject-p))
        next-headline
      nil)))

(defun spw/skip-non-stuck-projects ()
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (or (bh/is-task-p)
            (spw/has-scheduled-or-deadlined-subproject-p)
            (spw/has-next-action-p))
        ;; THEN: skip, and handle special case of the final entry in a
        ;; buffer which cannot be a stuck project and so should always
        ;; be skipped, but which won't be since `next-headline' will
        ;; be nil
        (if next-headline next-headline (point-max))
      ;; ELSE: don't skip
      nil)))

(defun spw/skip-incomplete-projects-and-all-subprojects ()
  "Skip all subprojects and projects with subprojects not yet completed."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (or (bh/is-subproject-p)
            (spw/has-incomplete-subproject-or-task-p))
        next-headline
      nil)))

(setq
 org-capture-templates
 '(("t" "Task to be refiled" entry (file "~/doc/org/refile.org")
    "* TODO %^{Title}
%?")
   ("n" "Information to be refiled" entry (file "~/doc/org/refile.org")
    "* %^{Title}
%?")
   ("a" "Appointment" entry (file+datetree "~/doc/org/diary.org")
    "* %^{Time} %^{Title & location}
%^t" :immediate-finish t)
   ("A" "Appointment (untimed)" entry (file+datetree "~/doc/org/diary.org")
    "* %^{Title & location}
%^t" :immediate-finish t)
   ("s" "Task for the future to be refiled" entry (file "~/doc/org/refile.org")
    "* SOMEDAY %^{Title}
%?")
   ("d" "Diary entry" entry (file+datetree "~/.labbook.gpg")
    "* %^{Title}
%U

%?")
   ("u" "URI on clipboard" entry (file "~/doc/org/refile.org")
    "* SOMEDAY [[%^{URI|%x}][%^{Title}]]" :immediate-finish t)))

;;; function and advice for my weekly review process (see the
;;; docstrings immediately below)

(defun spw/find-non-agenda-todos ()
  "Find Org files that aren't in `org-agenda-files` that probably
  should be"
  (interactive)
  (let ((default-directory org-directory)
        (args-together))
    (setq args-together "")
    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (dolist (elt (split-string (buffer-string) "\n" t) args-together)
        (if (not (f-directory? elt))
            (setq args-together (concat args-together " -not -name '" (file-name-nondirectory elt) "'")))))
    (grep-find (concat "find " org-directory
                       " -regextype posix-egrep -type f"
                       args-together
                       " -not -regex '" (expand-file-name org-directory) "/archive/.*'"
                       " -not -regex '" (expand-file-name org-directory) "/philos/.*'"
                       " -not -name reading.org"
                       " -not -name archive.org -not -regex '" (expand-file-name org-directory) "/[ABCDEFGHIJKLMNOPQRSTUVWXYZ].*' -exec egrep -nH -e \"\\* \(TODO\|SOMEDAY\|WAITING\|SOONDAY\) \" {} +"))))

(defun org-agenda--run-find-non-agenda-todos (&rest ignore)
  "Call grep to find Org files that aren't in `org-agenda-files'
  but should be, when opening my agenda for my weekly Org-mode
  review"
  (when (equal (buffer-name) "*Org Agenda(#)*")
    (delete-other-windows)
    (call-interactively 'spw/find-non-agenda-todos)))
(advice-add 'org-agenda :after #'org-agenda--run-find-non-agenda-todos)

;;;; ---- export and referencing ----

(unless (boundp 'org-export-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             ;; we drop the default packages then re-add almost all of them--amssymb doesn't play nice with our font package
             '("spwoutline"
               "\\documentclass{spwoutline}
\[NO-DEFAULT-PACKAGES\]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{hyperref}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("spwessay"
               "\\documentclass{spwessay}
\[NO-DEFAULT-PACKAGES\]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{hyperref}
                    \\renewcommand{\\tableofcontents}{}
                    [EXTRA]"
               ("
\\section{%s}" . "
\\section*{%s}")))

(add-to-list 'org-latex-classes
             '("spwpaper"
               "\\documentclass{spwpaper}
\[NO-DEFAULT-PACKAGES\]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage[pdftex]{hyperref}
\\hypersetup{colorlinks,citecolor=black,filecolor=black,linkcolor=black,urlcolor=black}
                    \\renewcommand{\\tableofcontents}{}
                    [EXTRA]"
               ("
\\section{%s}" . "
\\section*{%s}")))

(add-to-list 'org-latex-classes
             '("spwdoc"
               "\\documentclass{spwdoc}
                    [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("spwdnd"
               "\\documentclass{spwdnd}
                    [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("wordlike"
               "\\documentclass[12pt,a4paper]{article}
                  \\usepackage{wordlike}
\\usepackage{fancyhdr}
\\pagestyle{fancy}

\\fancyhead{}
\\fancyhead[R]{\\textsf{\\thepage}}
\\fancyfoot{}
\\renewcommand{\\headrulewidth}{0pt}

\\fancypagestyle{plain}{ %
\\fancyhf{} % remove everything
\\renewcommand{\\headrulewidth}{0pt} % remove lines as well
\\renewcommand{\\footrulewidth}{0pt}}
                    [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;;; Org publishing settings

;; following is purely so that we can export my Org files to
;; the desktop, not just into ~/doc/org
(add-to-list 'org-publish-project-alist
             '("doc"
               :base-directory "~/doc/org"
               :base-extension "org"
               :publishing-function org-latex-publish-to-pdf
               :publishing-directory "~/tmp"
               :completion-function spw/cleanup-org-pdfs))

;;; web-accessible and WebDAV-accessible notes

;; Note that the path ~/lib/fm needs to be either mounted with davfs2
;; or uploaded with cadaver.  So these projects should only be
;; published by shell scripts calling `emacs --batch' and then
;; sorting out davfs2 or cadaver

(add-to-list
 'org-publish-project-alist
 `("philos"
   :base-directory "~/doc/org/philos"
   :base-extension "org"
   :recursive nil
   :publishing-directory "/tmp/dionysus/Philos notes"
   :publishing-function org-html-publish-to-html
   :auto-sitemap t
   :sitemap-filename "index.html"
   :sitemap-title "Sean's reading notes"
   :table-of-contents t
   :html-head ,(concat
                "<style type=\"text/css\">"
                (when (f-exists? "~/doc/org/philos/style1.css")
                  (with-temp-buffer
                    (insert-file-contents "~/doc/org/philos/style1.css")
                    (buffer-string)))
                "</style>")))

(add-to-list
 'org-publish-project-alist
 `("org-tmp"
   :base-directory "~/doc/org"
   :base-extension "org"
   :recursive nil
   :publishing-directory "/tmp/dionysus/Org docs"
   :publishing-function org-html-publish-to-html
   :auto-sitemap t
   :sitemap-filename "123Index.html"
   :sitemap-title "Sean's ~/doc"
   :html-head ,(concat
                "<style type=\"text/css\">"
                (when (f-exists? "~/doc/org/worg.css")
                  (with-temp-buffer
                    (insert-file-contents "~/doc/org/worg.css")
                    (buffer-string)))
                "</style>")
   :html-head-include-default-style nil
   :table-of-contents t))

(add-to-list
 'org-publish-project-alist
 '("org-web"
   :base-directory "~/doc/org"
   :base-extension "org"
   :recursive nil
   :publishing-directory "/var/www/spw/org"
   :publishing-function org-html-publish-to-html
   :auto-sitemap t
   :sitemap-filename "index.html"
   :sitemap-title "Sean's ~/doc"
   :html-head "<link rel=\"stylesheet\" title=\"Worg\" href=\"/org/worg.css\" type=\"text/css\">
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/org/worg-zenburn.css\" type=\"text/css\">
<link rel=\"icon\" href=\"/org/org-mode-unicorn.ico\" type=\"image/vnd.microsoft.icon\" />
<link rel=\"SHORTCUT ICON\" href=\"https://spwhitton.name/org/org-mode-unicorn.ico\" type=\"image/vnd.microsoft.icon\" />"
   :html-preamble "<script type=\"text/javascript\">
    document.addEventListener('DOMContentLoaded',function() {
        document.getElementById(\"table-of-contents\").onclick = function() {
            var elem = document.getElementById(\"text-table-of-contents\");
            elem.style.display = elem.style.display == \"block\" ? \"none\" : \"block\";
        }
    });
</script>
<p><a href=\"/org\">Personal wiki index</a> &middot; <a href=\"/org/agenda.html\">Daily agenda view</a> &middot; <a href=\"/org/full.html\">Full agenda view</a> &middot; <a href=\"/org/diary.html\">Three month diary</a></p>"
   :html-head-include-default-style nil
   :table-of-contents t))

(add-to-list
 'org-publish-project-alist
 '("org-web-static"
   :base-directory "~/doc/org"
   :base-extension "css\\|ico"
   :recursive nil
   :publishing-directory "/var/www/spw/org"
   :publishing-function org-publish-attachment))

(defun spw/cleanup-org-pdfs ()
  (interactive)
  (dolist (file (f-glob "~/doc/org/*.pdf"))
    (delete-file file)))

(defun spw/end-of-refile ()
  (interactive)
  (if (get-buffer "refile.org")
      (progn
        (switch-to-buffer "refile.org")
        (unless (buffer-modified-p)
          (revert-buffer t t)))
    (find-file "~/doc/org/refile.org"))
  (end-of-buffer))

;;; reftex setup from
;;; http://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/

(org-add-link-type "ebib" 'ebib)

(org-add-link-type
 "cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citep" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citep{%s}" path)
       (format "\\citep[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citet" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citet{%s}" path)
       (format "\\citet[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citealp" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citealp{%s}" path)
       (format "\\citealp[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citealt" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citealt{%s}" path)
       (format "\\citealt[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citeauthor" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citeauthor{%s}" path)
       (format "\\citeauthor[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citeyear" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citeyear{%s}" path)
       (format "\\citeyear[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "citeyearpar" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\citeyearpar{%s}" path)
       (format "\\citeyearpar[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

;; (org-add-link-type
;;  "rep" 'ebib
;;  (lambda (path desc format)
;;    (cond
;;     ((eq format 'html)
;;      (format "(<cite>%s</cite>)" path))
;;     ((eq format 'latex)
;;      (if (or (not desc) (equal 0 (search "cite:" desc)))
;;          (format "\\cite{%s}" path)
;;        (format "\\cite[][\\nopage{%s}]{republic}"
;;                  path))))))

;; not calling this function atm
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         (global-auto-revert-mode t)
         (reftex-parse-all)
         (reftex-set-cite-format
          "[[cite:%l][]]"))))

;;;; ---- functions ----

(defun spw/new-philos-notes (name)
  "Create a file NAME.org in my philosophy notes directory.

The purpose of this function is to allow that name easily to have
spaces in it and to remove any colons."
  (interactive "sTitle: ")
  (let ((sanitised (replace-regexp-in-string "\?" "" (replace-regexp-in-string ":" "," name))))
    (find-file (expand-file-name
                (concat sanitised ".org")
                (f-join org-directory "philos")))
    (insert "philos")
    (yas-expand)
    (insert name)
    (yas-next-field)))

;;; the default C-c [ and C-c ] expand the directory ~/doc/org in the
;;; org-agenda-files variable using the local path,
;;; e.g. /meta/s/spw/doc/org, which is not good when init-custom.el is
;;; held in git.  So use alternative behaviour of storing the agenda
;;; paths in a file (see documentation for `org-agenda-files').  Two
;;; functions to do the work

(defun spw/org-agenda-file-to-front ()
  (interactive)
  (let ((path (abbreviate-file-name buffer-file-name)))
    (with-current-buffer (find-file-noselect org-agenda-files)
      (save-excursion
        (goto-char (point-min))
        (if (not (search-forward path nil t))
            (progn
              (goto-char (point-max))
              (insert path)
              (save-buffer)
              (message "added")))))))

(defun spw/org-remove-file ()
  (interactive)
  (let ((path (abbreviate-file-name buffer-file-name)))
    (with-current-buffer (find-file-noselect org-agenda-files)
      (save-excursion
        (goto-char (point-min))
        (if (search-forward path nil t)
            (progn
              (beginning-of-line)
              (kill-line 1)
              (save-buffer)
              (message "remove")))))))

;; ;; defeat variable-pitch-mode for tables and source blocks, per
;; ;; http://stackoverflow.com/a/16819449

;; (face-override-variable-pitch 'org-code)
;; (face-override-variable-pitch 'org-block)
;; (face-override-variable-pitch 'org-table)
;; ;;(face-override-variable-pitch 'org-block-background)

(defun spw/org-agenda-priority-filter (arg)
  (interactive "P")
  (if arg
      (push "\[#A\]\\|Appt" org-agenda-regexp-filter)
    (push "\[#[AB]\]\\|Appt" org-agenda-regexp-filter))
  (org-agenda-filter-apply org-agenda-regexp-filter 'regexp))

;;;; ---- hooks and keys ----

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(add-hook
 'org-mode-hook
 '(lambda ()
    ;; (when window-system
    ;;   (org-display-inline-images))
    (turn-on-auto-fill)
    ;; (org-mode-reftex-setup)
    ;; (smartparens-mode)
    ))

(add-hook
 'org-agenda-mode-hook
 '(lambda ()
    ;; always hilight the current agenda line
    (hl-line-mode 1)
    ;; make space work from the agenda to cycle the actual tree in the split
    (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)))

;; (defadvice org-agenda (after spw/open-weekday-schedule)
;;   (when (and window-system
;;              (not (get-buffer-window "fall_2015_weekday_schedule.org" 0))
;;              (y-or-n-p "Also load Fall 2015 weekday schedule?"))
;;     (find-file-other-window "~/doc/org/fall_2015_weekday_schedule.org")))

(define-key org-mode-map (kbd "C-c C-SPC") 'org-mark-subtree)
(define-key org-mode-map (kbd "<f11>") 'org-toggle-link-display)
;; (define-key org-mode-map (kbd "C-c )") 'reftex-citation)

(bind-key "C-c [" 'spw/org-agenda-file-to-front org-mode-map)
(bind-key "C-c ]" 'spw/org-remove-file org-mode-map)

;;; urxvt bindings.  shift left and right seem to work

(define-key org-mode-map (kbd "M-[ 1 ; 3 d") 'org-metaleft)
(define-key org-mode-map (kbd "M-[ 1 ; 3 C") 'org-metaright)
(define-key org-mode-map (kbd "M-[ 1 ; 3 A") 'org-metaup)
(define-key org-mode-map (kbd "M-[ 1 ; 3 B") 'org-metadown)
(define-key org-mode-map (kbd "<select>") 'org-shiftup)
(define-key org-mode-map (kbd "M-[ 1 ; 2 A") 'org-shiftup)
(define-key org-mode-map (kbd "M-[ 1 ; 2 b") 'org-shiftdown)
(define-key org-mode-map (kbd "M-[ 1 ; 2 C") 'org-shiftright)
(define-key org-mode-map (kbd "M-[ 1 ; 2 D") 'org-shiftleft)

;;; rebind <space> in agenda mode so that have access to evil's leader

(bind-key (kbd "M-SPC") 'org-agenda-cycle-show org-agenda-mode-map)

;;; hide low-priority tasks
(bind-key (kbd "&") 'spw/org-agenda-priority-filter org-agenda-mode-map)

;;; escape to get out of date entry

(define-key org-read-date-minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)

;;; export essays via Pandoc not ox.el

(bind-key "C-c M-e" 'spw/pandoc-paper-compile org-mode-map)

(provide 'init-org)
;;; init-org.el ends here
