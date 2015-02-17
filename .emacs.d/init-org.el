;;; init-org --- Sean's Org-mode configuration

;;; Commentary:

;;; Code:

;;;; ---- packages ----

;;; get Org headline completion in helm-mini

(use-package helm-org)

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

;;; links to mairix messages by message-id in Org

(use-package org-mairix-el
  :bind ("C-c m" . org-mairix-el-insert-link)
  :commands (org-mairix-el-insert-link org-mairix-el-link org-mairix-el-open))

;;;; ---- preferences ----

;; custom doesn't actually set all the faces it should, so we'll do
;; some manually
;;(set-face-foreground 'org-hide "#3f3f3f")
(when (set-default-font "Terminus-11")
  (set-face-font 'org-hide "Terminus"))

(setq org-alphabetical-lists t
      org-startup-indented 1
      org-indent-indentation-per-level 4
      org-adapt-indentation nil
      org-directory "~/doc/org"

      org-tag-alist '((:startgroup)
                      ("@work" . ?W)
                      ("@home" . ?H)
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
                            ("\\.jpg\\'" . "/usr/bin/eog %s")
                            ("\\.png\\'" . "/usr/bin/eog %s")
                            ("\\.gif\\'" . "/usr/bin/eog %s")))
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
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WAITING(w)" "SOONDAY(f)" "SOMEDAY(s)" "|" "CANCELLED(c)"))

      org-todo-keyword-faces '(("SOMEDAY" . (:foreground "#94BFF3" :weight bold)) ; zenburn-blue+1
                               ("SOONDAY" . (:foreground "#F0DFAF" :weight bold))) ; zenburn-yellow

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
      org-outline-path-complete-in-steps nil

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

      org-latex-default-class "wordlike"

      org-export-headline-levels 3   ; set to 2 for spwoutline

      ;; org-export-latex-low-levels '("\\begin{lowitemize}\\setlength{\\parindent}{2em}" "\\end{lowitemize}" "\\item \\textbf{%s}\\indent %s")

      ;; used after things like e.g. to prevent a double space
      org-entities-user '(("space" "\\ " nil " " " " " " " "))

      org-export-with-toc nil         ; default to no table of contents
      org-footnote-section "Notes"

      reftex-default-bibliography
      (quote
       ("~/doc/spw.bib")))

;;; add all my notes files to Org text search (e.g. C-c a /)

;; first clear out the (agenda-archives) from such searches; I only
;; have one so can easily search it manually
(setq org-agenda-text-search-extra-files nil)

(with-current-buffer (find-file-noselect org-agenda-files)
  (dolist (file (directory-files org-directory nil "\\.org$" t))
    (save-excursion
      (goto-char (point-min))
      ;; search for the file name in ~/doc/org-agenda-files.  If it's
      ;; there, don't add to the list: agenda files are included in
      ;; text searches automatically
      (if (not (search-forward file nil t))
          (add-to-list 'org-agenda-text-search-extra-files (concat "~/doc/org/" file))))))

;; remove my massive archive file; can search that manually if necessary
(delete "~/doc/org/archive.org" org-agenda-text-search-extra-files)

;;; Org habit

(use-package org-habit)
;; (setq org-habit-graph-column nil)

;;;; ---- agenda and refile ----

(defadvice org-agenda (before persp-org-agenda activate)
  "Switch perspective project before compiling the agenda."
  (projectile-persp-switch-project "~/doc"))

(setq org-agenda-custom-commands
      '(("a" "Primary agenda view"
         ((agenda "day" ((org-agenda-ndays 1)
                         (org-agenda-overriding-header "Today")
                         (org-agenda-time-grid nil)))
          (agenda "" ((org-agenda-ndays 3)
                      (org-agenda-start-day "+1d")
                      (org-agenda-time-grid nil)
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))
                      (org-agenda-show-all-dates nil)
                      (org-agenda-overriding-header "Coming up")
                      (org-agenda-files (quote ("~/doc/org/diary.org")))))
          (todo "SOONDAY" ((org-agenda-overriding-header "Things to be done that shouldn't be dated"))))
         ((org-agenda-start-with-log-mode nil)
          (org-agenda-start-with-follow-mode nil)
          ;; (org-agenda-tag-filter-preset '("-Sariul"))
          ) ("/ma:html/day/index.html"))
        ("w" "Weekly agenda"
         ((agenda "week" ((org-agenda-ndays 7)))))
        ("#" "Review view"
         (
          (todo "WAITING" ((org-agenda-todo-ignore-scheduled nil)
                           (org-agenda-todo-ignore-deadlines nil)
                           (org-agenda-todo-ignore-with-date nil)
                           (org-agenda-overriding-header "Things waiting on others: chase them now or schedule a TODO to do so")))
          (tags "LEVEL=1+REFILE"
                ((org-agenda-todo-ignore-with-date nil)
                 (org-agenda-todo-ignore-deadlines nil)
                 (org-agenda-todo-ignore-scheduled nil)
                 (org-agenda-overriding-header "Items to refile")
                 (org-agenda-start-with-entry-text-mode t))) ; doesn't work per block, so this does nothing atm

          ;; A headline with TODO needs to be done so it should be
          ;; dated.  This block finds undated headlines.  Subprojects
          ;; are skipped because it is only if they are subprojects of
          ;; a TODO that they need to be scheduled; subtasks of a
          ;; SOONDAY might well be TODO, but they need not be
          ;; scheduled.  And projects with scheduled or deadlines
          ;; subprojects are skipped because actioning the project has
          ;; been scheduled or deadlined, which is sufficient.
          (todo "TODO" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-overriding-header "Undated TODO items: add schedule or deadline to the project or a subtask, or change keyword to SOONDAY")
                        (org-agenda-skip-function 'spw/skip-subprojects-and-projects-with-scheduled-or-deadlined-subprojects)))

          (agenda "day" ((org-agenda-ndays 7) (org-agenda-overriding-header "Schedule undated into the following schedule") (org-agenda-time-grid nil)))) ((org-agenda-dim-blocked-tasks nil) (org-agenda-tag-filter-preset '("-REPEATED"))))
        ("A" "Tasks to be Archived" todo "DONE|CANCELLED|DELEGATED"
         ((org-agenda-overriding-header "Tasks to archive")
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-tag-filter-preset '("-APPT"))))
        ("d" "Three-month diary" agenda ""
         ((org-agenda-ndays 90)
          (org-agenda-start-on-weekday 1)
          (org-agenda-time-grid nil)
          (org-agenda-repeating-timestamp-show-all t)
          (org-agenda-entry-types '(:timestamp :sexp))
          (org-agenda-show-all-dates nil)
          (org-agenda-overriding-header "Sean's diary for the next three months")
          (org-agenda-files (quote ("~/doc/org/diary.org")))
          ) ("/ma:html/cal/index.html"))

        ("D" "One day diary" agenda ""
         ((org-agenda-ndays 1)
          (org-agenda-start-on-weekday 1)
          (org-agenda-time-grid nil)
          (org-agenda-repeating-timestamp-show-all t)
          (org-agenda-entry-types '(:timestamp :sexp))
          (org-agenda-show-all-dates nil)
          (org-agenda-overriding-header "Sean's diary for today")
          (org-agenda-files (quote ("~/doc/org/diary.org")))
          ) ("/tmp/diary.txt"))))

;;; sensible automatic tag filtering

(defun org-my-auto-exclude-function (tag)
  (and (cond
        ((string= tag "@home")
         (string= (system-name) "SPWHITTON"))
        ((string= tag "@work")
         (not (string= (system-name) "SPWHITTON"))))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'org-my-auto-exclude-function)

;;; agenda skipping functions

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun spw/skip-subprojects ()
  "Skip trees that are subprojects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
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

(setq org-capture-templates
      '(("t" "Task" entry (file "~/doc/org/refile.org")
         "* TODO %^{Title}
%?")
        ("e" "Task from an e-mail" entry (file "~/doc/org/refile.org")
         "* TODO %^{Title}
%(org-mairix-el-link)
%?")
        ("w" "Work task" entry (file "~/doc/org/refile.org")
         "* TODO %^{Title}                      :@work:
%?")
        ("n" "Note" entry (file "~/doc/org/refile.org")
         "* %^{Title}
%?")
        ("a" "Appointment" entry (file+datetree "~/doc/org/diary.org")
         "* %^{Time} %^{Title & location}
%^t" :immediate-finish t)
        ("A" "Appointment (untimed)" entry (file+datetree "~/doc/org/diary.org")
         "* %^{Title & location}
%^t" :immediate-finish t)
        ("s" "For the future" entry (file "~/doc/org/refile.org")
         "* SOMEDAY %^{Title}
%?")
        ("f" "For the near future" entry (file "~/doc/org/refile.org")
         "* SOONDAY %^{Title}
%?")
        ;; ("d" "Diary" entry (file+datetree+prompt "~/doc/misc/daily.org") ; produces "invalid time specification" atm :(
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
                       " -not -name archive.org -not -regex '" (expand-file-name org-directory) "/[ABCDEFGHIJKLMNOPQRSTUVWXYZ].*' -exec egrep -nH -e \"\\* \(TODO\|SOMEDAY\|WAITING\|SOONDAY\) \" {} +"))))

(defadvice org-agenda (after spw/org-agenda-run-find-non-agenda-todos activate)
  "Call grep to find Org files that aren't in `org-agenda-files'
  but should be, when opening my agenda for my weekly Org-mode
  review"
  (if (equal (buffer-name) "*Org Agenda(#)*")
      (progn
        (delete-other-windows)
        (call-interactively 'spw/find-non-agenda-todos))))

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
             '("spw-doc"
               :base-directory "~/doc/org"
               :base-extension "org"
               :publishing-function org-latex-publish-to-pdf
               :publishing-directory "~/tmp"
               :completion-function spw/cleanup-org-pdfs))

(defun spw/export-org-wiki ()
  (interactive)
  (if (not (string= "ma.sdf.org" (system-name)))
      (message "not gonna do this unless on the MetaArray")
    (let ((org-export-htmlize-output-type 'css)
          (org-export-with-toc t)
          (org-export-in-background t)
          (org-publish-project-alist '(("spw-wiki"
                                        :base-directory "~/doc/org"
                                        :base-extension "org"
                                        :recursive nil
                                        :publishing-directory "/meta/www/s/spw/wiki"
                                        :publishing-function org-html-publish-to-html
                                        :auto-sitemap t
                                        :sitemap-filename "index.html"
                                        :sitemap-title "Sean's ~/doc"
                                        :html-head "<link rel=\"stylesheet\" title=\"Worg\" href=\"/inc/worg.css\" type=\"text/css\">
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/inc/worg-zenburn.css\" type=\"text/css\">
<link rel=\"icon\" href=\"/inc/org-mode-unicorn.ico\" type=\"image/vnd.microsoft.icon\" />
<link rel=\"SHORTCUT ICON\" href=\"https://spw.sdf.org/inc/org-mode-unicorn.ico\" type=\"image/vnd.microsoft.icon\" />"
                                        :html-preamble "<script type=\"text/javascript\">
    document.addEventListener('DOMContentLoaded',function() {
        document.getElementById(\"table-of-contents\").onclick = function() {
            var elem = document.getElementById(\"text-table-of-contents\");
            elem.style.display = elem.style.display == \"block\" ? \"none\" : \"block\";
        }
    });
</script>
<p><a href=\"/wiki\">Personal wiki index</a> &middot; <a href=\"/wiki/agenda.html\">Daily agenda view</a> &middot; <a href=\"/wiki/cal.html\">Three month diary</a></p>"
                                        :html-head-include-default-style nil
                                        :table-of-contents t))))
      (org-publish-project "spw-wiki"))))

(defun spw/cleanup-org-pdfs ()
  (interactive)
  (dolist (file (f-glob "~/doc/org/*.pdf"))
    (delete-file file)))

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
              (save-buffer)))))))

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
              (save-buffer)))))))

;;;; ---- hooks and keys ----

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;;; popwin.el interferes with Org goto: it keeps the help window
;;; selected.
(defadvice org-goto (around org-goto-disable-popwin)
  "Disable popwin around Org-goto"
  (if popwin-mode
      (progn
        (popwin-mode -1)
        ad-do-it
        (popwin-mode 1))
    ad-do-it))
(ad-activate 'org-goto)

(add-hook 'org-mode-hook '(lambda ()
                            (if window-system
                                (org-display-inline-images))
                            (turn-on-auto-fill)
                            (org-mode-reftex-setup)
                            (smartparens-mode)))

(add-hook 'org-agenda-mode-hook '(lambda ()
                                   ;; always hilight the current agenda line
                                   (hl-line-mode 1)
                                   ;; make space work from the agenda to cycle the actual tree in the split
                                   (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)))

(define-key org-mode-map (kbd "C-c C-SPC") 'org-mark-subtree)
(define-key org-mode-map (kbd "<f11>") 'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c )") 'reftex-citation)

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

(provide 'init-org)
;;; init-org.el ends here
