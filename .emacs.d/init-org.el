;;; init-org --- Sean's Org-mode configuration

;;; Commentary:

;;; Code:

;;;; ---- packages ----

;;; get Org headline completion in helm-mini

;; (use-package helm-org)

;;; my helper functions for blog posts

(use-package spw-pyblosxom)

;;; with the new exporter in Org version 8, must explicitly require
;;; the exporters I want to use

(use-package ox-html)
(use-package ox-latex)
(use-package ox-ascii)
(use-package ox-odt)

;;; checklist helper functions including automatic resetting

(use-package org-checklist)

;;; inline tasks

(use-package org-inlinetask)

;;; links to mairix messages by message-id in Org

(use-package org-mairix-el
  :bind ("C-c m" . org-mairix-el-insert-link)
  :commands (org-mairix-el-insert-link org-mairix-el-link org-mairix-el-open))

;;; highlight current sentence

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
(when (member "Terminus" (font-family-list))
  (set-face-font 'org-hide "Terminus"))

(setq
 org-alphabetical-lists t
 org-startup-indented 1
 org-indent-indentation-per-level 4
 org-adapt-indentation nil
 org-directory "~/doc/org"

 org-tag-alist '((:startgroup)
                 ("@libDASL" . ?l)
                 ("@Tucson" . ?e)
                 ("@campus" . ?m)
                 ("@Sheffield" . ?u)
                 ("@E5thSt" . ?h)
                 ("@www" . ?i)
                 (:endgroup))

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
 org-agenda-diary-file "~/doc/org/refile.org"
 org-agenda-insert-diary-strategy 'top-level
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
 org-log-into-drawer nil
 org-log-state-notes-insert-after-drawers t
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
 org-file-apps (quote ((auto-mode . emacs)
                       ("\\.mm\\'" . system)
                       ("\\.x?html?\\'" . system)
                       ("\\.pdf\\'" . system)
                       ("\\.jpg\\'" . "/usr/bin/feh %s")
                       ("\\.png\\'" . "/usr/bin/feh %s")
                       ("\\.gif\\'" . "/usr/bin/feh %s")))
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
 org-agenda-window-setup 'current-window
 org-agenda-entry-text-maxlines 3

 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
   (sequence "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)"))

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

;; remove my massive archive file; can search that manually if necessary
(delete "~/doc/org/archive.org" org-agenda-text-search-extra-files)

;;; Org habit

(use-package org-habit)
;; (setq org-habit-graph-column nil)

;;;; ---- agenda and refile ----

(defun org-agenda--switch-projectile-project ()
  "Switch perspective project before compiling the agenda."
  ;; don't do it if we're already there so that the buffer we're
  ;; switching from remains top of the buffer list
  (unless
      (ignore-errors (string= "~/doc/" (abbreviate-file-name (projectile-project-root))))
    (projectile-switch-project-by-name "~/doc")))
(advice-add 'org-agenda :before #'org-agenda--switch-projectile-project)

(setq
 org-agenda-custom-commands
 '(("a" "Primary agenda view"
    ((agenda "day" ((org-agenda-ndays 1)
                    (org-agenda-overriding-header
                     "Tasks, appointments and waiting tasks to be chased today")
                    (org-agenda-include-deadlines nil)
                    (org-agenda-time-grid nil))))
    ((org-agenda-start-with-log-mode nil)
     ;; (org-agenda-tag-filter-preset '("-Sariul"))
     (org-agenda-start-with-follow-mode nil))
    ("/var/www/spw/org/agenda.html" "~/lib/fm/dionysus/Agenda/Today's agenda.html"))
   ("A" "Daily planning view"
    ((agenda "day" ((org-agenda-ndays 1)
                    (org-agenda-time-grid nil)
                    (org-agenda-overriding-header "Plan for today & upcoming deadlines")))
     (todo "TODO|NEXT" ((org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines 'far)
                        (org-agenda-overriding-header "Unscheduled standalone tasks & project next actions")
                        (org-agenda-skip-function 'spw/skip-projects-and-non-next-subprojects)))
     (agenda "" ((org-agenda-ndays 3)
                 (org-agenda-start-day "+1d")
                 (org-agenda-time-grid nil)
                 (org-agenda-repeating-timestamp-show-all t)
                 (org-agenda-entry-types '(:timestamp :sexp))
                 (org-agenda-show-all-dates nil)
                 (org-agenda-overriding-header "Coming up")
                 (org-agenda-files (quote ("~/doc/org/diary.org"))))))
    nil
    ("/var/www/spw/org/full.html" "~/lib/fm/dionysus/Agenda/Day-planning agenda.html"))
   ("#" "Weekly review view"
    ((todo "WAITING" ((org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines nil)
                      (org-agenda-todo-ignore-with-date nil)
                      (org-agenda-overriding-header "Waiting on others & not scheduled to chase up")))
     (todo "TODO" ((org-agenda-todo-ignore-with-date t)
                   (org-agenda-overriding-header "Stuck projects")
                   (org-agenda-skip-function 'spw/skip-non-stuck-projects)))
     (tags "LEVEL=1+REFILE"
           ((org-agenda-todo-ignore-with-date nil)
            (org-agenda-todo-ignore-deadlines nil)
            (org-agenda-todo-ignore-scheduled nil)
            (org-agenda-overriding-header "Items to add context and priority, and refile")
            (org-agenda-start-with-entry-text-mode t)))
     (todo "DONE|CANCELLED"
           ((org-agenda-overriding-header "Tasks to be archived CAREFUL DON'T ARCHIVE SUBTASKS OF INCOMPLETE PROJECTS")
            (org-agenda-todo-ignore-scheduled nil)
            (org-agenda-todo-ignore-deadlines nil)
            (org-agenda-todo-ignore-with-date nil)
            (org-agenda-tag-filter-preset '("-APPT"))))))

   ("d" "Six-month diary" agenda ""
    ((org-agenda-ndays 180)
     ;; (org-agenda-start-on-weekday 1)
     (org-agenda-time-grid nil)
     (org-agenda-repeating-timestamp-show-all t)
     (org-agenda-entry-types '(:timestamp :sexp))
     (org-agenda-show-all-dates nil)
     (org-agenda-overriding-header "Sean's diary for the next six months")
     (org-agenda-files (quote ("~/doc/org/diary.org"))))
    ("/var/www/spw/org/diary.html" "~/lib/fm/dionysus/Agenda/Six month diary.html"))))

;;; sensible automatic tag filtering

(defun org-my-auto-exclude-function (tag)
  (and
   (cond
    ;; tags passed to org-agenda-auto-exclude-function always
    ;; lower case per version Org 6.34 changelog
    ((string= tag "@e5thst")
     (not (string= (system-name) "artemis.silentflame.com")))
    ((string= tag "@tucson")
     (not (string= (system-name) "artemis.silentflame.com")))
    ((string= tag "@sheffield")
     (not (string= (system-name) "zephyr.silentflame.com")))
    ((string= tag "@campus")
     (string= (system-name) "artemis.silentflame.com"))
    ((string= tag "@libdasl")
     (not (string= (system-name) "ma.sdf.org"))))
   (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'org-my-auto-exclude-function)

;;; agenda skipping functions

(defun spw/org-get-todo-keyword ()
  (let ((todo-state (save-match-data (ignore-errors (org-get-todo-state)))))
    (spw/strip-text-properties todo-state)))

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

(defun spw/skip-projects-and-non-next-subprojects ()
  "Skip projects and subtasks of projects that are not NEXT actions"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (or (and (bh/is-subproject-p) (not (string= (spw/org-get-todo-keyword) "NEXT")))
            (bh/is-project-p))
        next-headline
      nil)))

(defun spw/org-is-scheduled-or-deadlined-p ()
  "A task that is scheduled or deadlined"
  (let ((is-dated)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
        (regexp (org-re-timestamp 'scheduled-or-deadline)))
    (message regexp)
    (save-excursion
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
        next-headline
      nil)))

(setq
 org-capture-templates
 '(("t" "Task to be refiled" entry (file "~/doc/org/refile.org")
    "* TODO %^{Title} %^G
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
            (setq args-together (concat args-together " -not -name " (file-name-nondirectory elt))))))
    (grep-find (concat "find " org-directory
                       " -regextype posix-egrep -type f"
                       args-together
                       " -not -regex '" (expand-file-name org-directory) "/archive/.*'"
                       " -not -regex '" (expand-file-name org-directory) "/philos/.*'"
                       " -not -name reading.org"
                       " -not -name archive.org -not -regex '" (expand-file-name org-directory) "/[ABCDEFGHIJKLMNOPQRSTUVWXYZ].*' -exec egrep -nH -e \"\\* \(TODO\|SOMEDAY\|WAITING\|SOONDAY\) \" {} +"))))

(defun org-agenda--run-find-non-agenda-todos ()
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

;;; load up Org publishing settings

(load "~/doc/www/org-publish.el" 'noerror)
;; (load "~/doc/sf/www/org-publish.el" 'noerror)

;;; set some more Org publishing settings

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
   :publishing-directory "~/lib/fm/dionysus/Philos notes"
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
 `("org-dav"
   :base-directory "~/doc/org"
   :base-extension "org"
   :recursive nil
   :publishing-directory "~/lib/fm/dionysus/Org docs"
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

(defun spw/fmr-sync-doc ()
  "Perform `mr sync' command in ~/doc."
  (interactive)
  (org-save-all-org-buffers)
  (let ((default-directory (expand-file-name "~/doc/"))
        (buffer (get-buffer-create "*mr sync*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (display-buffer "*mr sync*")
    (async-shell-command "remdocsync" "*mr sync*")))

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

;; defeat variable-pitch-mode for tables and source blocks, per
;; http://stackoverflow.com/a/16819449

(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(mapc
 (lambda (face)
   (set-face-attribute
    face nil
    :inherit
    (my-adjoin-to-list-or-symbol
     'fixed-pitch
     (face-attribute face :inherit))))
 (list 'org-code 'org-block 'org-table 'org-block-background))

;;;; ---- hooks and keys ----

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(add-hook
 'org-mode-hook
 '(lambda ()
    ;; (when window-system
    ;;   (org-display-inline-images))
    (turn-on-auto-fill)
    ;; (org-mode-reftex-setup)
    (smartparens-mode)))

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

;; Redefine `org-auto-repeat-maybe' with abo-abo's patch from
;; <http://stackoverflow.com/a/18125783> so that when a task is both
;; scheduled and deadlined and only the deadline repeats, the
;; scheduled timestamp will be cleared when the deadline is moved
;; forward

(defun org-auto-repeat-maybe (done-word)
  "Check if the current headline contains a repeated deadline/schedule.
If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.
This function is run automatically after each state change to a DONE state."
  ;; last-state is dynamically scoped into this function
  (let* ((repeat (org-get-repeat))
	 (aa (assoc org-last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (whata '(("h" . hour) ("d" . day) ("m" . month) ("y" . year)))
	 (msg "Entry repeats: ")
	 (org-log-done nil)
	 (org-todo-log-states nil)
	 re type n what ts time to-state)
    (when (and repeat (not (zerop (string-to-number (substring repeat 1)))))
      (if (eq org-log-repeat t) (setq org-log-repeat 'state))
      (setq to-state (or (org-entry-get nil "REPEAT_TO_STATE")
			 org-todo-repeat-to-state))
      (unless (and to-state (member to-state org-todo-keywords-1))
	(setq to-state (if (eq interpret 'type) org-last-state head)))
      (org-todo to-state)
      (when (or org-log-repeat (org-entry-get nil "CLOCK"))
	(org-entry-put nil "LAST_REPEAT" (format-time-string
					  (org-time-stamp-format t t))))
      (when org-log-repeat
	(if (or (memq 'org-add-log-note (default-value 'post-command-hook))
		(memq 'org-add-log-note post-command-hook))
	    ;; OK, we are already setup for some record
	    (if (eq org-log-repeat 'note)
		;; make sure we take a note, not only a time stamp
		(setq org-log-note-how 'note))
	  ;; Set up for taking a record
	  (org-add-log-setup 'state (or done-word (car org-done-keywords))
			     org-last-state
			     'findpos org-log-repeat)))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed)
      (setq re (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
		       org-deadline-time-regexp "\\)\\|\\("
		       org-ts-regexp "\\)"))
      (while (re-search-forward
	      re (save-excursion (outline-next-heading) (point)) t)
	(setq type (if (match-end 1) org-scheduled-string
		     (if (match-end 3) org-deadline-string "Plain:"))
	      ts (match-string (if (match-end 2) 2 (if (match-end 4) 4 0))))
	(if (not (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)" ts))
            (org-remove-timestamp-with-keyword org-scheduled-string)
          (setq	n (string-to-number (match-string 2 ts))
                what (match-string 3 ts))
          (if (equal what "w") (setq n (* n 7) what "d"))
          (if (and (equal what "h") (not (string-match "[0-9]\\{1,2\\}:[0-9]\\{2\\}" ts)))
              (user-error "Cannot repeat in Repeat in %d hour(s) because no hour has been set" n))
          ;; Preparation, see if we need to modify the start date for the change
          (when (match-end 1)
            (setq time (save-match-data (org-time-string-to-time ts)))
            (cond
             ((equal (match-string 1 ts) ".")
              ;; Shift starting date to today
              (org-timestamp-change
               (- (org-today) (time-to-days time))
               'day))
             ((equal (match-string 1 ts) "+")
              (let ((nshiftmax 10) (nshift 0))
                (while (or (= nshift 0)
                           (<= (time-to-days time)
                               (time-to-days (current-time))))
                  (when (= (incf nshift) nshiftmax)
                    (or (y-or-n-p (message "%d repeater intervals were not enough to shift date past today.  Continue? " nshift))
                        (user-error "Abort")))
                  (org-timestamp-change n (cdr (assoc what whata)))
                  (org-at-timestamp-p t)
                  (setq ts (match-string 1))
                  (setq time (save-match-data (org-time-string-to-time ts)))))
              (org-timestamp-change (- n) (cdr (assoc what whata)))
              ;; rematch, so that we have everything in place for the real shift
              (org-at-timestamp-p t)
              (setq ts (match-string 1))
              (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)" ts))))
          (save-excursion (org-timestamp-change n (cdr (assoc what whata)) nil t))
          (setq msg (concat msg type " " org-last-changed-timestamp " "))))
      (setq org-log-post-message msg)
      (message "%s" msg))))

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

;;; escape to get out of date entry

(define-key org-read-date-minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)

;;; export essays via Pandoc not ox.el

(bind-key "C-c M-e" 'spw/pandoc-paper-compile org-mode-map)

(provide 'init-org)
;;; init-org.el ends here
