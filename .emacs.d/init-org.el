;;; Sean's Org-mode configuration

;;;; ---- packages ----

;;; this config uses `f-glob' and `diminish'

(require 'f)
(require 'diminish)

;;; with the new exporter in Org version 8, must explicitly require
;;; the exporters I want to use

(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)
(require 'ox-odt)

;;; checklist helper functions including automatic resetting

(require 'org-checklist)

;;; inline tasks

(require 'org-inlinetask)

;;; links to notmuch messages

(require 'org-notmuch)

;;; org-indent-mode

(require 'org-indent)

;;; I occasionally use org-habit

(require 'org-habit)



;;;; ---- preferences ----

(setq
 ;; use virtual indentation.  I chose this a long time ago and I'm not
 ;; sure whether that was wise, but turning it off is probably even
 ;; less wise
 org-startup-indented t
 org-indent-indentation-per-level 2
 org-adapt-indentation nil

 ;; view
 org-startup-folded t
 org-hide-emphasis-markers nil
 org-cycle-separator-lines 0
 org-show-following-heading t
 org-show-siblings t
 org-show-hierarchy-above t

 ;; links
 org-return-follows-link nil

 ;; agenda
 org-agenda-files "~/doc/emacs-org-agenda-files"
 org-agenda-sticky t
 ;; org-agenda-dim-blocked-tasks nil
 org-deadline-warning-days 60
 org-agenda-skip-deadline-prewarning-if-scheduled 3
 ;; we just use a completely custom agenda view
 ;; org-agenda-todo-ignore-with-date nil
 ;; org-agenda-todo-ignore-deadlines nil
 ;; org-agenda-todo-ignore-scheduled 'future
 ;; org-agenda-todo-list-sublevels nil
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
 ;; org-agenda-skip-additional-timestamps-same-entry nil
 org-agenda-skip-timestamp-if-done t
 org-agenda-start-on-weekday nil
 org-agenda-persistent-filter t
 org-agenda-window-setup 'reorganize-frame
 org-agenda-restore-windows-after-quit t
 org-agenda-entry-text-maxlines 3


 ;; org-goto preferences
 org-goto-auto-isearch t
 org-goto-interface 'outline

 ;; archiving
 org-archive-mark-done nil
 org-archive-save-context-info '(time file olpath)
 org-archive-location "~/doc/org/archive/archive.org::* From %s"

 ;; inline tasks
 ;; prefix arg can be used to override this setting
 org-inlinetask-default-state "TODO"

 ;; we don't actually use Org's built-in stuck project support,
 ;; instead generating our own review agenda from scratch which
 ;; includes the right tasks.  See the view assigned to the '#' key
 org-stuck-projects '("TODO" '("NEXT") nil "")

 ;; manipulating headlines
 org-use-fast-todo-selection t
 org-treat-S-cursor-todo-selection-as-state-change nil
 org-treat-insert-todo-heading-as-state-change t
 org-fast-tag-selection-include-todo nil
 org-enforce-todo-dependencies t
 org-insert-heading-respect-content nil

 ;; refiling
 org-refile-targets '((org-agenda-files :maxlevel . 5)
		      (nil :maxlevel . 5))
 org-refile-use-outline-path 'file
 org-refile-allow-creating-parent-nodes 'confirm
 ;; This has to be nil to work with helm;
 ;; see http://lists.gnu.org/archive/html/emacs-orgmode/2014-06/msg00846.html
 ;; org-outline-path-complete-in-steps nil

 ;; completion
 org-completion-use-ido t

 ;; manipulating subtrees
 org-yank-adjusted-subtrees t
 org-yank-folded-subtrees nil

 ;; logging and notes
 org-log-into-drawer "LOGBOOK"
 org-log-states-order-reversed nil
 org-reverse-note-order nil
 org-log-done t
 org-log-redeadline nil
 org-log-reschedule nil
 org-log-refile nil

 ;; bindings
 org-special-ctrl-a/e t
 org-special-ctrl-k t

 ;; dates
 org-read-date-prefer-future t

 ;; lists
 org-list-demote-modify-bullet
 '(("-" . "+")
   ("+" . "*")
   ("*" . "-")
   ("1." . "-")
   ("1)" . "-"))
 org-list-use-circular-motion t

 ;; searching
 org-tags-match-list-sublevels 'indented

 ;; my tags and todo keywords
 org-tag-alist
 '((:startgroup)
   ("@Tucson"       . ?t)
   ("@Sheffield"    . ?s)
   ("@LaAldea"      . ?h)
   (:endgroup)
   ("@iPad"         . ?i)
   ;; following are needed when at times when I'm regularly accessing
   ;; my Org-mode agenda over SSH
   ;; (:startgroup)
   ;; ("@Emacs"        . ?e) ; SSH Emacs only
   ;; ("@workstation"  . ?m) ; on my fully set-up personal (m)achine
   ;; (:endgroup)
   ("UA"            . ?w) ; academic work
   ("Debian"        . ?d))
 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
   (sequence "WAITING(w@)" "SOMEDAY(s)" "|" "CANCELLED(c)"))
 org-todo-keyword-faces
 '(("SOMEDAY" . (:foreground "#94BFF3" :weight bold)) ; zenburn-blue+1
   ("NEXT" . (:foreground "#F0DFAF" :weight bold))) ; zenburn-yellow

 ;; capture
 org-default-notes-file (concat org-directory "/refile.org")

 ;; general export
 org-export-headline-levels 3
 org-export-with-toc nil
 ;; org-export-with-smart-quotes t
 org-export-date-timestamp-format "%e %B %Y"

 ;; LaTeX export
 org-latex-pdf-process
 '("texi2dvi --pdf --clean --batch %f" "rm %f" "rm -rf auto")
 org-latex-default-class "wordlike"

 ;; HTML export
 ;; org-export-htmlize-output-type 'css

 ;; ODT export
 org-odt-preferred-output-format "pdf"

 ;; bookmarks
 ;; don't generate bookmarks as causes git merge conflicts
 org-bookmark-names-plist nil)

;;;; ---- agenda ----

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
     ;; (org-agenda-start-with-entry-text-mode t)
     (org-agenda-start-with-follow-mode nil)))
   ("A" "Daily planning view"
    ((agenda "day" ((org-agenda-span 'day)
                    (org-agenda-time-grid nil)
                    (org-agenda-overriding-header
                     "Plan for today & upcoming deadlines")))
     (todo "TODO|NEXT" ((org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines 'far)
                        (org-agenda-overriding-header'
                         "Unscheduled standalone tasks & project next actions")
                        (org-agenda-skip-function 'spw--skip-non-actionable)))
     ;; commented out as not using Org as an appointments diary at present
     ;; (agenda "" ((org-agenda-span 3)
     ;;             (org-agenda-start-day "+1d")
     ;;             (org-agenda-time-grid nil)
     ;;             (org-agenda-repeating-timestamp-show-all t)
     ;;             (org-agenda-entry-types '(:timestamp :sexp))
     ;;             (org-agenda-show-all-dates nil)
     ;;             (org-agenda-overriding-header "Coming up")
     ;;             (org-agenda-files (quote ("~/doc/org/diary.org")))))
     ))
   ("#" "Weekly review view"
    ((todo "WAITING" ((org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines nil)
                      (org-agenda-todo-ignore-with-date nil)
                      (org-agenda-overriding-header
                       "Waiting on others & not scheduled to chase up")))
     (todo "TODO|NEXT" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-overriding-header "Stuck projects")
                        (org-agenda-skip-function 'spw--skip-non-stuck-projects)))
     (tags "LEVEL=1+REFILE"
           ((org-agenda-todo-ignore-with-date nil)
            (org-agenda-todo-ignore-deadlines nil)
            (org-agenda-todo-ignore-scheduled nil)
            (org-agenda-overriding-header
             "Items to add context tag and priority, and refile")))

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
             'spw--skip-incomplete-projects-and-all-subprojects)))))

   ("d" "Six-month diary" agenda ""
    ((org-agenda-span 180)
     ;; (org-agenda-start-on-weekday 1)
     (org-agenda-time-grid nil)
     (org-agenda-repeating-timestamp-show-all t)
     (org-agenda-entry-types '(:timestamp :sexp))
     (org-agenda-show-all-dates nil)
     (org-agenda-overriding-header "Sean's diary for the next six months")
     (org-agenda-files '("~/doc/org/diary.org"))))))

;;; sensible automatic tag filtering

(defun spw--org-auto-exclude-function (tag)
  (and
   (cond
    ;; tags passed to org-agenda-auto-exclude-function always
    ;; lower case per Org version 6.34 changelog
    ((string= tag "@laaldea")
     (not (string= (system-name) "hephaestus")))
    ((string= tag "@sheffield")
     (not (string= (system-name) "zephyr")))
    ((string= tag "@tucson")
     (not (or (string= (system-name) "iris")
              (string= (system-name) "hephaestus"))))
    ;; ((string= tag "@campus")
    ;;  (string= (system-name) "athena"))
    ;; ((string= tag "@workstation")
    ;;  (not (or (string= (system-name) "iris")
    ;;           (string= (system-name) "zephyr")
    ;;           (string= (system-name) "hephaestus"))))
    ((string= tag "ua")
     (= (calendar-day-of-week (calendar-current-date)) 6)))
   (concat "-" tag)))
(setq org-agenda-auto-exclude-function 'spw--org-auto-exclude-function)

;;; agenda skipping functions.  Many of these are adapted from Bernt
;;; Hansen's http://doc.norang.ca/org-mode.html

(defun bh--is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh--is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh--is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defmacro spw--skip-when (&rest condition)
  "Skip trees where CONDITION is false when evaluated when point is on the headline of the tree."
  `(let ((next-headline (save-excursion (outline-next-heading))))
     (if ,@condition
         (or next-headline
             ;; if there is no next headline, skip by going to the end
             ;; of the buffer.  An alternative would be `(save-excursion
             ;; (forward-line 1) (point))'
             (point-max))
       nil)))

(defun spw--skip-subprojects ()
  "Skip trees that are subprojects"
  (spw--skip-when (bh--is-subproject-p)))

(defun spw--skip-projects-with-scheduled-or-deadlined-subprojects ()
  "Skip projects that have subtasks, where at least one of those
  is scheduled or deadlined"
  (spw--skip-when (spw--has-scheduled-or-deadlined-subproject-p)))

(defun spw--skip-subprojects-and-projects-with-scheduled-or-deadlined-subprojects ()
  "Skip subprojects projects that have subtasks, where at least
  one of those is scheduled or deadlined."
  (spw--skip-when
   (or (bh--is-subproject-p)
       (spw--has-scheduled-or-deadlined-subproject-p))))

(defun spw--skip-incomplete-projects-and-all-subprojects ()
  "Skip all subprojects and projects with subprojects not yet completed."
  (spw--skip-when
   (or (bh--is-subproject-p)
       (spw--has-incomplete-subproject-or-task-p))))

(defun spw--skip-non-stuck-projects ()
  (spw--skip-when
   (or (bh--is-task-p)
       (spw--has-scheduled-or-deadlined-subproject-p)
       (spw--has-next-action-p))))

(defun spw--skip-non-actionable ()
  "Skip:
- anything tagged @Sheffield when I'm in Tucson
- anything tagged @Tucson when I'm in Sheffield
- projects (i.e. has subtasks)
- subtasks of projects that are not NEXT actions
- subtasks of SOMEDAY projects
- subtasks of WAITING projects
- subtasks of scheduled projects

In the last case, the idea is that if I've scheduled the project
then I intend to tackle all the NEXT actions on that date (or at
least the next chunk of them); I've broken the project down into
NEXT actions but not for the purpose of handling them on
different occasions."
  (spw--skip-when
   (or
    ;; #1
    ;; iris is a laptop, but usually it's not in Sheffield
    (and (or
          (string= (system-name) "iris")
          (string= (system-name) "hephaestus"))
         (member "@Sheffield" (org-get-tags)))
    ;; #2
    (and (string= (system-name) "zephyr")
         (member "@Tucson" (org-get-tags)))
    ;; #3
    (bh--is-project-p)

    ;; we used to skip deadlined standalone tasks but actually those
    ;; are actionable in general
    ;; (and (bh--is-task-p)
    ;;      (spw--org-has-deadline-p))

    ;; #4--#7
    (and (bh--is-subproject-p)
         (or
          ;; #4
          (not (string= (nth 2 (org-heading-components)) "NEXT"))
          (save-excursion
            (and
             (org-up-heading-safe)
             (or
              ;; # 5
              (string= (nth 2 (org-heading-components)) "SOMEDAY")
              ;; # 6
              (string= (nth 2 (org-heading-components)) "WAITING")
              ;; # 7
              (spw--org-is-scheduled-p)))))))))

(defun spw--org-is-scheduled-p ()
  "A task that is scheduled"
  (let ((is-dated)
        (is-a-task
         (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
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
      (when (looking-at (org-re-timestamp 'scheduled))
        (setq is-dated t)))
    (and is-a-task is-dated)))

(defun spw--org-has-deadline-p ()
  "A task that has a deadline"
  (let ((is-dated)
        (is-a-task
         (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      ;; Ignore errors if we fail to expand a subtree because we're
      ;; before the first heading
      (ignore-errors (outline-show-subtree))
      (forward-line)
      (org-beginning-of-line)
      (when (looking-at (org-re-timestamp 'deadline))
        (setq is-dated t)))
    (and is-a-task is-dated)))

(defun spw--org-is-scheduled-or-deadlined-p ()
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
      (when (looking-at regexp)
        (setq is-dated t)))
    (and is-a-task is-dated)))

(defun spw--has-scheduled-or-deadlined-subproject-p ()
  "A task that has a scheduled or deadlined subproject"
  (let (has-scheduled-or-deadlined-subproject)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (while (ignore-errors (outline-next-heading))
          (when (spw--org-is-scheduled-or-deadlined-p)
            (setq has-scheduled-or-deadlined-subproject t)))))
    has-scheduled-or-deadlined-subproject))

(defun spw--has-next-action-p ()
  "A task that has a NEXT subproject"
  (let (has-next-subproject)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (while (ignore-errors (outline-next-heading))
          (when (string= (nth 2 (org-heading-components)) "NEXT")
            (setq has-next-subproject t)))))
    has-next-subproject))

(defun spw--has-incomplete-subproject-or-task-p ()
  "A task that has an incomplete subproject or task."
  (let (has-incomplete-subproject)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (while (ignore-errors (outline-next-heading))
          (unless (member (nth 2 (org-heading-components))
                          (list "DONE" "CANCELLED")))
          (setq has-incomplete-subproject t)))))
  has-incomplete-subproject)

;;;; ---- capture templates ----

(setq
 org-capture-templates-contexts
 '(("t" "m" ((in-mode . "notmuch-show-mode")))
   ("t" ((not-in-mode . "notmuch-show-mode")))
   ("T" ((in-mode . "notmuch-show-mode")))
   ("m" ((in-mode . "notmuch-show-mode"))))
 org-capture-templates
 '(("t" "Task to be refiled" entry (file org-default-notes-file)
    "* TODO %^{Title}\n%?")
   ("T" "Task to be refiled" entry (file org-default-notes-file)
    "* TODO %^{Title}\n%?")
   ("n" "Information to be refiled" entry (file org-default-notes-file)
    "* %^{Title}\n%?")
   ("m" "Task from mail to be refiled" entry (file org-default-notes-file)
    ;; lisp is to filter square brackets out of the subject as these
    ;; mean that the Org-mode link does not properly form
    "* TODO [[notmuch:id:%:message-id][%^{Title|\"%(replace-regexp-in-string \"\\\\\\[\\\\\\|\\\\\\]\" \"\" \"%:subject\")\" from %:fromname}]]\n%?")
   ;; ("a" "Appointment" entry (file+datetree "~/doc/org/diary.org")
   ;;     "* %^{Time} %^{Title & location}
   ;; %^t" :immediate-finish t)
   ;;    ("A" "Appointment (untimed)" entry (file+datetree "~/doc/org/diary.org")
   ;;     "* %^{Title & location}
   ;; %^t" :immediate-finish t)
   ("s" "Task for the future to be refiled" entry (file org-default-notes-file)
    "* SOMEDAY %^{Title}\n%?")
   ("d" "Diary entry" entry (file+datetree "~/.labbook.gpg")
    "* %^{Title}\n%U\n\n%?")
   ("u" "URI on clipboard" entry (file org-default-notes-file)
    "* SOMEDAY [[%^{URI|%x}][%^{Title}]]" :immediate-finish t)))

;;;; ---- for weekly review process ----

(defun spw--find-non-agenda-todos ()
  "Find Org files that aren't in `org-agenda-files` that probably
  should be

Ignore SOMEDAYs as might have those in old notes but not important to include them"
  (interactive)
  (let ((default-directory org-directory)
        (args-together ""))
    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (dolist (elt (split-string (buffer-string) "\n" t) args-together)
        (if (not (f-directory? elt))
            (setq args-together (concat args-together " -not -name '" (file-name-nondirectory elt) "'")))))
    (grep-find
     (concat
      "find " org-directory
      " -regextype posix-egrep -type f"
      args-together
      " -not -regex '" (expand-file-name org-directory) "/archive/.*'"
      " -not -regex '" (expand-file-name org-directory) "/philos/.*'"
      " -not -name reading.org"
      " -not -name archive.org -not -regex '"
      (expand-file-name org-directory)
      "/[ABCDEFGHIJKLMNOPQRSTUVWXYZ].*' -exec egrep -nH -e \"\\* \(TODO\|NEXT\|WAITING\) \" {} +"))))

(defun org-agenda--run-find-non-agenda-todos (&rest ignore)
  "Call grep to find Org files that aren't in `org-agenda-files'
  but should be, when opening my agenda for my weekly Org-mode
  review"
  (when (equal (buffer-name) "*Org Agenda(#)*")
    (delete-other-windows)
    (call-interactively 'spw--find-non-agenda-todos)))
(advice-add 'org-agenda :after #'org-agenda--run-find-non-agenda-todos)

;;;; ---- export ----

(add-to-list
 'org-latex-classes
 '("wordlike"
   "\\documentclass[12pt]{article}
\\usepackage{wordlike}
\\usepackage{parskip}
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

;;;; ---- adding and removing agenda files ----

;;; the default C-c [ and C-c ] expand the directory ~/doc/org in the
;;; org-agenda-files variable using the local path,
;;; e.g. /meta/s/spw/doc/org, which is not good when init-custom.el is
;;; held in git.  So use alternative behaviour of storing the agenda
;;; paths in a file (see documentation for `org-agenda-files').  Two
;;; functions to do the work

(defun spw--org-agenda-file-to-front ()
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
(bind-key "C-c [" 'spw--org-agenda-file-to-front org-mode-map)

(defun spw--org-remove-file ()
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
              (message "removed")))))))
(bind-key "C-c ]" 'spw--org-remove-file org-mode-map)

;;;; ---- hooks and bindings ----

(defun spw--org-agenda-priority-filter (arg)
  "Hide low-priority items.  If ARG, hide slightly fewer."
  (interactive "P")
  (if arg
      (push "\[#A\]\\|Appt" org-agenda-regexp-filter)
    (push "\[#[AB]\]\\|Appt" org-agenda-regexp-filter))
  (org-agenda-filter-apply org-agenda-regexp-filter 'regexp))
(bind-key (kbd "&") 'spw--org-agenda-priority-filter org-agenda-mode-map)

;; save all Org buffers once an hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; auto-fill-mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; hl-line-mode in agenda buffers
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; space to cycle remote visibility in agenda
(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)))

;; this binding seems to have dropped out of upstream, so define it again
(bind-key (kbd "C-c C-SPC") 'org-mark-subtree org-mode-map)
