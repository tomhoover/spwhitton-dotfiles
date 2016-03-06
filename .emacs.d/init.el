;;; init --- Sean's Emacs configuration

;;; Commentary:

;;; Code:

;;;; ---- package management ----

;; be sure not to load stale bytecode-compiled lisp
(setq load-prefer-newer t)

;; this is where all subtree packages are
(defconst emacs-pkg-dir (concat user-emacs-directory "pkg"))

;; load up f, and its dependencies s and dash, so we can use `f-glob'
;; and `f-join'
(dolist (pkg '(("f" . "f.el")
               ("dash" . "dash.el")
               ("s" . "s.el")))
  (unless (locate-library (car pkg))
    (add-to-list 'load-path (concat emacs-pkg-dir "/" (cdr pkg)))))
(require 'f) (require 's) (require 'dash)

;; helper function
(defun expand-all-globs (root globs)
  (let ((do-glob (lambda (glob) (f-glob (f-join root glob)))))
    (apply 'nconc (mapcar do-glob globs))))

;; now add all my pkg lisp directories

;; As of Mar-16 we're appending rather than prepending to the
;; load-path so that any installed Debian ELPA packages take
;; precedence over those in `emacs-pkg-dir'.

;; If I want some lisp in `emacs-pkg-dir' to take precedence over
;; system-wide lisp, I should create a file ~/.emacs.d/pkg/overrides
;; and have the following code prepend a directory to the load path if
;; it is listed in that file.
(let* ((globs '("*" "*/lisp"))
       (dirs (expand-all-globs emacs-pkg-dir globs)))
  (dolist (dir dirs)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir t))))

;; finally put my own site-lisp at the front of `load-path'
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;; we will use use-package to load everything else
(require 'use-package)

;;;; ---- basic settings ----

;;; customisation -- must be loaded early so that zenburn theme is
;;; considered safe

(defconst custom-file (concat user-emacs-directory "init-custom.el"))
(load custom-file 'noerror)

;; when I do elisp experimentation I use IELM, so make scratch buffer
;; a little more co-operative
(setq initial-major-mode 'text-mode)

;;; load terminal fixes

(load-file (concat user-emacs-directory "init-term.el"))

;;; put backups and autosaves in /tmp

(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(make-directory emacs-tmp-dir t)
(chmod emacs-tmp-dir (string-to-number "700" 8))
(setq backup-by-copying t                    ; don't clobber symlinks
      backup-directory-alist `((".*" . ,emacs-tmp-dir))
      tramp-backup-directory-alist backup-directory-alist
      auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
      auto-save-list-file-prefix emacs-tmp-dir
      tramp-auto-save-directory emacs-tmp-dir

      ;; disable backups for files accessed through tramp's sudo and su
      ;; protocols, to prevent copies of root-owned files being in a user's
      ;; homedir
      backup-enable-predicate (lambda (name)
                                (and (normal-backup-enable-predicate name)
                                     (not
                                      (let ((method (file-remote-p name 'method)))
                                        (when (stringp method)
                                          (member method '("su" "sudo"))))))))

;;; misc display settings

;; focus follow mouse
(setq mouse-autoselect-window nil
      focus-follows-mouse window-system)

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; warn before killing Emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; don't handle keyboard events before redrawing
(setq redisplay-dont-pause t)

;; don't prompt to create scratch buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; initial frame width -- commented out in favour of maximising
;; (if window-system (set-frame-width (selected-frame) 80))

;; soft word wrapping for easier editing of long lines
(setq-default visual-line-mode t
              word-wrap t
              wrap-prefix "    ")
(diminish 'visual-line-mode)

;; ;; kill the fringes, if we have window system support compiled in
;; (if (fboundp 'set-fringe-mode)
;;     (set-fringe-mode 0))

;; Terminus
(add-to-list 'default-frame-alist '(font . "Terminus-11"))

;; disable GUI elements
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; cursor settings

(setq x-stretch-cursor t)
(setq-default cursor-type 'box)

;; turns off blink-cursor-mode if it ended up on
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;; get the mouse out of the way
(mouse-avoidance-mode 'exile)

;;; zenburn

(use-package zenburn-theme
  :init
  (add-to-list
   'custom-theme-load-path
   (concat user-emacs-directory "pkg/zenburn-emacs"))
  (load-theme 'zenburn))

;;; I'm in Arizona

(unless (eq system-type 'windows-nt)
  (set-time-zone-rule "/usr/share/zoneinfo/US/Mountain"))

;;; be sure to start the server

;; According to <http://emacs.stackexchange.com/a/12789> this might be
;; causing Emacs to fail to start up properly sometimes after a
;; reboot, giving me "Error reading from stdin".  I don't see how that
;; would be but since I'm starting with --daemon anyway, let's try
;; commenting it out.

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))



;;;; ---- packages ----

;;; clean up the mode line

(use-package diminish)

;;; instead of vim text objects

(use-package expand-region
  :bind ("M-i" . er/expand-region)
  :init
  (setq expand-region-contract-fast-key (kbd "o"))

  ;; fill out the region to the beginning and ends of the
  ;; lines at either end of it when we're not using
  ;; expand-region but we've activated the mark (but only do
  ;; this once)
  (defun er/expand-region--fill-out-region (orig-fun &rest args)
    (if (or (not (region-active-p))
            (eq last-command 'er/expand-region))
        (apply orig-fun args)
      (if (< (point) (mark))
          (let ((beg (point)))
            (goto-char (mark))
            (end-of-line)
            (forward-char 1)
            (push-mark)
            (goto-char beg)
            (beginning-of-line))
        (let ((end (point)))
          (goto-char (mark))
          (beginning-of-line)
          (push-mark)
          (goto-char end)
          (end-of-line)
          (forward-char 1)))))
  (advice-add 'er/expand-region :around #'er/expand-region--fill-out-region))

;;; sexp management

(defmacro spw/paredit-unsteal (map)
  "Reclaim core Emacs bindings from Paredit in MAP."
  `(progn
     (define-key ,map (kbd "M-s") nil)
     (define-key ,map (kbd "M-r") nil)
     (define-key ,map (kbd "M-U") 'paredit-splice-sexp)
     (define-key ,map (kbd "M-<up>") 'paredit-raise-sexp)))

(use-package paredit
  :commands paredit-mode
  :init
  (dolist
      (hook
       '(emacs-lisp-mode-hook
         lisp-mode-hook
         lisp-interaction-mode-hook
         ielm-mode-hook
         scheme-mode-hook
         inferior-scheme-mode-hook))
    (add-hook hook 'paredit-mode))

  (use-package paredit-everywhere
    :commands paredit-everywhere-mode
    :init
    (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
    (add-hook 'minibuffer-setup-hook 'paredit-everywhere-mode)
    :config
    (spw/paredit-unsteal paredit-everywhere-mode-map))
  :config
  (spw/paredit-unsteal paredit-mode-map)
  ;; C-j is useless with `aggressive-indent-mode'
  (setq )
  )

(electric-pair-mode 1)
(show-paren-mode 1)

;; based on http://emacs.stackexchange.com/a/2554/8610
(defmacro spw/add-mode-pairs (hook pairs)
  `(add-hook ,hook
             (lambda ()
               (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
               (setq-local electric-pair-text-pairs electric-pair-pairs))))

(spw/add-mode-pairs 'emacs-lisp-mode-hook '((?` . ?')))

;;; keep parentheses under control: modern replacement for the mighty paredit

(use-package smartparens
  :disabled t
  :bind (("M-J" . sp-join-sexp)

         ;; for when I use Emacs via PuTTY
         ("M-<right>" . sp-forward-slurp-sexp)
         ("M-<left>" . sp-forward-barf-sexp))
  :commands (smartparens-strict-mode
             smartparens-mode
             show-smartparens-global-mode)
  :defer 5
  :init

  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  scheme-mode-hook
                  inferior-scheme-mode-hook
                  python-mode-hook
                  minibuffer-setup-hook
                  ;; haskell-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (smartparens-strict-mode 1))))

  :config

  ;; always highlight parens
  (show-smartparens-global-mode 1)

  (require 'smartparens-config)
  (setq sp-navigate-consider-symbols t)

  (smartparens-global-mode)

  ;; global smartparens bindings
  (sp-use-smartparens-bindings)
  (bind-key "C-w" 'sp-backward-kill-word emacs-lisp-mode-map)
  (bind-key "C-k" 'sp-kill-hybrid-sexp emacs-lisp-mode-map)
  (bind-key "M-<up>" 'sp-raise-sexp smartparens-mode-map)

  ;; and now undo some of that in particular major modes
  (add-hook 'org-mode-hook (lambda ()
                             (crowding/local-set-minor-mode-key
                              'smartparens-mode (kbd "M-<up>") nil)))

  ;; override smartparens binding for C-k outside of lisp,
  ;; since sp-kill-hybrid-sexp isn't very smart in comint
  ;; and I like using C-u C-k
  ;; (define-key smartparens-strict-mode-map [remap kill-line] 'kill-line)
  ;; (bind-key "C-k" 'sp-kill-hybrid-sexp
  ;; emacs-lisp-mode-map)

  ;; when killing the first word of a sentence, leave the
  ;; two spaces after the previous sentence's terminal
  ;; period
  (defun sp-backward-kill-word--fix-punctuation (&rest ignore)
    (save-excursion
      (backward-char 2)
      (when (and (or (looking-at "\\. ")
                     (looking-at   "! ")
                     (looking-at "\\? "))
                 (not (looking-back "^[1-9]+")))
        (forward-char 1)
        (insert " "))))
  (advice-add 'sp-backward-kill-word :after #'sp-backward-kill-word--fix-punctuation)

  ;; fix cursor position after M-d at the beginning of a line
  (defun sp-kill-word--beg-of-line-fix ()
    (when (looking-back "^[[:space:]]")
      (backward-char 1)))
  (advice-add 'sp-kill-word :after #'sp-kill-word--beg-of-line-fix)

  ;; when after whitespace at the beginning of a line or
  ;; an Org bullet or heading, delete it all
  (defun sp-backward-delete-char--remove-indentation (orig-fun &rest args)
    (if (and
         ;; do it if we're not at the beginning of the line,
         ;; and there's whitespace: if we're at the
         ;; beginning of the line we should always delete
         (not (equal (point) (line-beginning-position)))
         (or
          (looking-back "^[[:space:]]+")
          (looking-back "^[[:space:]]*- ")
          (looking-back "^[*]+ ")))
        (kill-line 0)
      ;; if not after whitespace at the beginning of the
      ;; line, just call as usual
      (apply orig-fun args)))
  (advice-add 'sp-backward-delete-char :around #'sp-backward-delete-char--remove-indentation)

  ;; when after whitespace at the beginning of a line or an Org bullet
  ;; or heading, delete it all.  This is more intuitive when C-w is
  ;; one's main way to delete stuff
  (defun sp-backward-delete-word--remove-indentation (orig-fun &rest args)
    (if (and
         ;; do it if we're not at the beginning of the line,
         ;; and there's whitespace: if we're at the
         ;; beginning of the line we should always delete
         (not (equal (point) (line-beginning-position)))
         (or
          (looking-back "^[[:space:]]+")
          (looking-back "^[[:space:]]*- ")
          (looking-back "^[*]+ ")))
        (kill-line 0)
      ;; if not after whitespace at the beginning of the
      ;; line, just call as usual
      (apply orig-fun args)))
  (advice-add 'sp-backward-delete-word :around #'sp-backward-delete-word--remove-indentation)

  ;; define an additional pairing for Org-mode (verbatim text)
  (sp-local-pair 'org-mode "=" "=")

  (sp-local-pair 'markdown-mode "`" "`"))

;;; Org

(eval-after-load 'org '(load "~/.emacs.d/init-org.el"))

(use-package org
  ;; :ensure org-plus-contrib ; Debian Jessie archive version is new enough
  ;; :pin org
  :mode (("\\.org" . org-mode)
         ("\\.org_archive" . org-mode))
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o [" . spw/org-agenda-file-to-front)
         ("C-c o ]" . spw/org-remove-file)
         ("C-c o n" . spw/new-philos-notes)
         ("C-c o s" . spw/fmr-sync-doc))
  :commands (org-capture
             org-store-link
             org-agenda
             spw/org-agenda-file-to-front
             spw/org-remove-file
             spw/new-philos-notes
             spw/fmr-sync-doc))

;;; save my places in buffers; this is all the session management I need

(setq recentf-save-file "~/.emacs.d/recentf")

;; the three hooks added by the idle progn below don't stay set when
;; set by (require 'saveplace), nor do they remain in place if simply
;; added in this config file or even in 'after-init-hook.  So have
;; use-package add them a few seconds after Emacs starts

(use-package saveplace
  :init (setq-default save-place t
                      save-place-file "~/.emacs.d/saveplace")
  :defer 5
  :config
  (add-hook 'find-file-hook 'save-place-find-file-hook t)
  (add-hook 'kill-emacs-hook 'save-place-kill-emacs-hook)
  (add-hook 'kill-buffer-hook 'save-place-to-alist))

;;; more useful unique buffer names

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward))

;;; OpenWith

(use-package openwith
  :commands openwith-mode
  ;; I want this immediately
  :demand
  :config (openwith-mode 1))

;; thanks to openwith, the warning for large files can be at a much
;; larger threshold as the chances of hitting it are low (this is
;; about 100MB)

(setq large-file-warning-threshold 100000000)

;;; doc-view

(use-package doc-view
  :init (setq doc-view-resolution 150
              doc-view-continuous t)
  :config (add-hook 'doc-view-mode-hook 'auto-revert-mode))

;;; magit

(use-package magit
  :diminish magit-wip-after-save-local-mode
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :demand
  :config

  ;; Fix magit-version: doesn't work when magit is a subtree
  (defun magit-version ()
    (setq magit-version "2.3.1")
    (when (called-interactively-p 'any)
      (message "Magit %s, Git %s, Emacs %s"
               (or magit-version "(unknown)")
               (or (magit-git-version) "(unknown)")
               emacs-version)))

  (setq magit-completing-read-function 'magit-ido-completing-read
        magit-push-always-verify nil
        magit-revert-buffers 'silent)

  ;; C-c C-a to amend without any prompt
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (magit-with-refresh
       (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
  (bind-key "C-c C-a" 'magit-just-amend magit-status-mode-map)

  (use-package magit-annex))

;;; pointback mode: make sure that point is back where I left it when
;;; switching between buffers where at least one buffer is displayed
;;; in more than one window

(use-package pointback
  :commands global-pointback-mode
  :defer 5
  :config
  (global-pointback-mode 1))

;;; colour those parentheses

(use-package rainbow-delimiters
  :init (setq-default frame-background-mode 'dark)
  :commands rainbow-delimiters-mode)

;;; and colour those colours

(use-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

;;; keep reindenting lisp

(use-package aggressive-indent
  :commands aggressive-indent-mode)

;;; ElDoc and rainbow delimiters activation

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook))
  (add-hook hook
            (lambda ()
              (eldoc-mode 1)
              (aggressive-indent-mode 1)
              (rainbow-delimiters-mode 1))))

;;; boxquotes

(use-package boxquote
  :commands (boxquote-title
             boxquote-region
             boxquote-buffer
             boxquote-insert-file
             boxquote-insert-buffer
             boxquote-kill-ring-save
             boxquote-yank
             boxquote-defun
             boxquote-paragraph
             boxquote-boxquote
             boxquote-describe-function
             boxquote-describe-variable
             boxquote-describe-key
             boxquote-shell-command
             boxquote-where-is
	     boxquote-text
             boxquote-narrow-to-boxquote
             boxquote-narrow-to-boxquote-content
             boxquote-kill
             boxquote-fill-paragraph
             boxquote-unbox-region
             boxquote-unbox))

;;; word count in modeline, when I want it

(use-package wc-mode
  :init
  (setq wc-modeline-format "%tw words"))

;;; company-mode for smart and easy completion

(use-package company
  ;; :commands global-company-mode
  ;; :bind ("<tab>" . company-complete)
  ;; :idle (global-company-mode)
  :diminish company-mode
  :config

  ;; startup company

  (defun spw/company-prog-setup ()
    "Setup company mode carefully when its needed, rather than using the brash global-company-mode"
    (company-mode 1)
    (define-key (current-local-map) (kbd "M-/") 'company-complete))
  (add-hook 'prog-mode-hook 'spw/company-prog-setup)
  ;; alternative approach: https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387

  ;; I like my C-w binding so move one of company's bindings
  (define-key company-active-map "\C-w" nil)
  (bind-key "M-o" 'company-show-location company-active-map)

  ;; settings

  (setq company-idle-delay nil
        company-minimum-prefix-length 0
        company-echo-delay 0)

  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-transformers 'company-sort-by-occurrence)

  ;; python code completion

  (use-package anaconda-mode
    :disabled t
    :config (progn
              (add-hook 'python-mode-hook 'anaconda-mode)
              (add-to-list 'company-backends 'company-anaconda)
              (add-hook 'python-mode-hook 'anaconda-eldoc))))
;; C-o during company isearch narrows to stuff matching that search;
;; mnemonic 'occur'.  C-M-s while outside of search to do the same
;; thing

;;; Randomize the order of lines in a region

(use-package randomize-region :commands randomize-region)

;;; Markdown mode

(use-package markdown-mode
  :mode "\\.md"

  :init
  (add-hook 'markdown-mode-hook 'turn-on-orgstruct)
  (add-hook 'markdown-mode-hook 'turn-on-orgstruct++)
  (spw/add-mode-pairs 'markdown-mode-hook '((?` . ?`)))

  :config
  ;; This binding replaces a `markdown-export'.
  (bind-key "C-c M-e" 'spw/pandoc-paper-compile markdown-mode-map))

;;; RefTeX

(use-package reftex
  :init
  (add-hook 'markdown-mode-hook 'turn-on-reftex)
  (defun spw/org-maybe-turn-on-reftex ()
    (when (string= default-directory (expand-file-name "~/doc/papers/"))
      (turn-on-reftex)))
  (add-hook 'org-mode-hook 'spw/org-maybe-turn-on-reftex)
  :config
  ;; This setup binds `C-c [ RET search-string RET' to try to insert a
  ;; citation
  (setq reftex-default-bibliography (quote ("~/doc/spw.bib"))
        reftex-cite-format '((?\C-m . "[@%l]")
                             (?- . "[-@%l]"))))

;;; PHP mode

(use-package php-mode :mode (("\\.php" .  php-mode)))

;;; YAML mode

(use-package yaml-mode :mode (("\\.yaml" .  yaml-mode)))

;;; Deft

(use-package deft
  :commands deft
  :bind ("C-c f" . deft)
  :init
  (setq deft-extensions '("org" "mdwn")
        deft-text-mode 'org-mode
        deft-directory "~/doc/org/"
        deft-recursive t
        deft-use-filename-as-title nil

        ;; trying snake_case for now (CamelCase means main
        ;; agenda files)
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "_")
                                 (nospace . "_")
                                 (case-fn . downcase))

        deft-auto-save-interval 20.0
        deft-incremental-search t
        deft-org-mode-title-prefix t)
  :config
  (bind-key "C-w" 'deft-filter-decrement-word deft-mode-map)

  ;; With my xmonad setup, when `window-width' is x then only x-1
  ;; characters will actually fit in the window.  Advise deft so its
  ;; display doesn't wrap unreadably.
  (defun deft-buffer-setup--fix-window-width (orig-fun &rest args)
    (let ((width (window-width)))
      ;; this is just an flet; see
      ;; <http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html>
      (cl-letf (((symbol-function 'window-width) (lambda  () (- width 1))))
        (apply orig-fun args))))
  (advice-add 'deft-buffer-setup :around #'deft-buffer-setup--fix-window-width))

;;; The following two python packages seem to be unavailable for
;;; download.  Disable them for now.

;;; allow lisp to interact with python

(use-package pymacs :disabled t)

;;; Get Python documentation as info files

(use-package python-info :disabled t)
;;(use-package pydoc-info :ensure)

;;; flycheck

(use-package flycheck
  :demand
  :init

  ;; try to disable flymake; having both running at the same time is annoying
  (setq flymake-allowed-file-name-masks nil)

  ;; make sure flycheck doesn't complain about our use of `require'
  ;; (might have to disable this sometimes: see docstring for the var)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; disable flycheck in org-mode as it clobbers the important C-c !
  (defun flycheck-mode--org-disable-flycheck (orig-fun &rest args)
    (unless (eq major-mode 'org-mode)
      (apply orig-fun args)))
  (advice-add 'flycheck-mode :around #'flycheck-mode--org-disable-flycheck)

  (use-package flycheck-haskell
    :demand
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
    :config
    ;; override stack defaults
    (setq flycheck-haskell-runghc-command (list "runghc")))

  :config

  ;; don't check too often: brief Emacs lock-ups are annoying
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (global-flycheck-mode 1))

;;; TRAMP

(use-package tramp
  :config

  ;; usernames on hosts
  (add-to-list 'tramp-default-user-alist '(nil "sdf" "spw"))
  (add-to-list 'tramp-default-user-alist '("sudo" "localhost" "root"))
  (add-to-list 'tramp-default-user-alist '(nil nil "swhitton") t)
  (add-to-list 'tramp-default-user-alist '(nil "ma" "spw"))
  (add-to-list 'tramp-default-user-alist '(nil "sage" "spwhitton"))

  ;; TRAMP and zsh are not friends so might as well switch
  ;; over here
  (setenv "SHELL" "/bin/bash"))

;;; ebib for editing BiBTeX databases

(use-package ebib
  :bind ("C-c g e" . ebib)
  :init (setq ebib-preload-bib-files '("~/doc/spw.bib")))

;;; dired enhancements

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t)

;; should be able to unzip with Z
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(use-package dired-sort-map
  :init (setq dired-listing-switches "--group-directories-first -alh"))

(use-package git-annex)

;;; close old buffers once per day

(use-package midnight
  :init (midnight-delay-set 'midnight-delay "3am"))

;;; simple concept of projects

(use-package projectile
  :commands projectile-vc
  :diminish projectile-mode
  :bind (("C-c p" . projectile-command-map)
         ("C-c j" . projectile-find-file)
         ("C-c v" . projectile-vc))
  :demand
  :config
  (projectile-global-mode 1)
  (setq projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'ido)
  (add-to-list 'projectile-globally-ignored-directories ".stack-work")
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories ".cabal-sandbox")
  (add-to-list 'projectile-globally-ignored-directories "docsets")

  ;; as part of daily cleanup, clean-up projects that no longer exist
  (add-hook 'midnight-hook 'projectile-cleanup-known-projects)

  ;; bind opening programming projects (also see projectile() in .zshrc)
  (bind-key "n" 'spw/open-programming-project projectile-command-map))

;;; completion with ido

;; fix C-w
(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key
     ido-completion-map "\C-w"
     'ido-delete-backward-word-updir)))

;; override ido-use-filename-at-point for dired buffers
;; from http://emacs.stackexchange.com/a/5331
(defun spw/ido-ignore-file-at-point ()
  "Disable ido-use-filename-at-point for the current buffer."
  (when (bound-and-true-p ido-use-filename-at-point)
    (setq-local ido-use-filename-at-point nil)))
(add-hook 'dired-mode-hook #'spw/ido-ignore-file-at-point)

(setq ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".org" ".mdwn" ".hs" ".tex" ".py" )
      ido-default-file-method 'selected-window
      ido-max-directory-size 100000
      ido-auto-merge-delay-time 99999 ; only search when I tell you to M-s
      ido-use-virtual-buffers t
      ido-use-virtual-buffers-automatically t
      ido-enable-regexp nil
      ido-use-url-at-point nil
      ido-max-file-prompt-width 0.1
      ido-save-directory-list-file "~/.emacs.d/ido.last"

      ;; Have Ido respect completion-ignored-extensions
      ido-ignore-extensions t

      ;; When moving through work directories with M-n/M-p, ignore those
      ;; that don't match the current input
      ido-work-directory-match-only t)

;; disable *Completions* and *Ido Completions* buffers for the sake of
;; `frames-only-mode'
(setq ido-completion-buffer nil
      completion-auto-help  nil)

(ido-mode 1)
(ido-everywhere 1)

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        flx-ido-threshhold 7500
        gc-cons-threshold 20000000))

(use-package ido-ubiquitous
  :config (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :disabled t
  :init (ido-vertical-mode 1))

(use-package smex
  :bind ("C-x C-m" . smex))

;; imenu

(use-package imenu-anywhere)

;;; snippets

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (yas-global-mode 1))

;;; htmlize for Org HTML export/publishing

(use-package htmlize)

;;; make indentation in python nice and visible

(use-package highlight-indentation
  :init
  (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode))

;;; jump around what's visible

;; ace-jump-mode just as a dependency of ace-link (below)
(use-package ace-jump-mode)

;; do the real work with avy
(use-package avy
  :bind (("M-o" . spw/avy-goto-word)
         ;; if one types numbers, avy-goto-line will switch to old M-g
         ;; g behaviour so may override default M-g g binding
         ("M-g g" . avy-goto-line))
  :config

  (setq
   ;; Make avy overlays look more like ace-jump
   avy-style       'at-full
   ;; Make avy work over all visible frames (nice with
   ;; frames-only-mode)
   avy-all-windows 'all-frames)

  ;; Attempt to restore ace-jump-mode functionality whereby M-o jumps
  ;; by word start, C-u M-o by any char in the word.  We're taking the
  ;; input ourselves so that `avy-goto-word-1' doesn't see arg and
  ;; thus narrow to the current window
  (defun spw/avy-goto-word (char &optional arg)
    (interactive (list (read-char "char: ")
                       current-prefix-arg))
    (if arg (avy-goto-char char nil)
      (avy-goto-word-1 char nil)))

  (face-override-variable-pitch 'avy-lead-face-0)
  (face-override-variable-pitch 'avy-lead-face-1)
  (face-override-variable-pitch 'avy-lead-face-2)
  (face-override-variable-pitch 'avy-lead-face))

;; use ace-jump-mode to move between links in help file
(use-package ace-link
  :defer 5
  :config
  (ace-link-setup-default))

;;; make dired copy and move asynchronously

(use-package async
  :init (when (require 'dired-aux)
          (require 'dired-async)))

;;; give dired some nice keybindings for browsing images

(use-package image-dired
  :init (image-dired-setup-dired-keybindings))

;;; alternative to my old `spw/centre-window'

(use-package centered-window-mode :disabled t :commands centered-window-mode)

;;; zap-up-to-char is at least as useful as zap-to-char, so load it
;;; out of misc.el.

(use-package misc
  :commands zap-up-to-char)

;;; make Emacs regexps easier

(use-package visual-regexp)

;;; advanced key binding techniques with hydra

(use-package hydra
  :config
  (setq hydra-windows-config nil)
  (defun spw/maybe-delete-other-windows ()
    (interactive)
    (if (= (count-windows) 1)
        (set-window-configuration hydra-windows-config)
      (setq hydra-windows-config (current-window-configuration))
      (delete-other-windows)))
  (defhydra hydra-windows (global-map "C-x" :color red)
    "windows"
    ("o" other-window "next" :color red)
    ("O" (lambda () (interactive) (other-window -1)) "previous" :color red)
    ("S" spw/toggle-window-split "toggle" :color red)
    ("0" delete-window "del" :color red)
    ("1" spw/maybe-delete-other-windows "max" :color red)
    ("2" split-window-below "horiz" :color red)
    ("3" split-window-right "vert" :color red))

  ;;; winner mode: undo window configuration changes

  (winner-mode 1)

  ;; Undo the bindings so that the hydra bindings take precedence
  (define-key winner-mode-map (kbd "C-c <left>") nil)
  (define-key winner-mode-map (kbd "C-c <right>") nil)

  (defhydra hydra-winner (global-map "C-c" :color red)
    "winner"
    ("<left>" winner-undo "back" :color red)

    ;; We need a lambda here to override `winner-redo''s check that
    ;; the last command was winner-undo, since the last command will
    ;; be `hydra-winner/winner-undo' if we replace the lambda with
    ;; just `winner-undo'
    ("<right>" (lambda () (interactive)
                 (let ((last-command 'winner-undo))
                   (winner-redo))) "forward" :color red)))

;;; aligning rule for Haskell adapted from Haskell mode wiki

(use-package align
  :config
  (add-to-list 'align-rules-list
               '(haskell-defns
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\|=\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "^[^:\n]+\\(\\s-+\\)\\(->\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))

;;; Load up Haskell mode settings if Debian haskell-mode package
;;; installed (and load here as after other packages these settings
;;; depend on)

(when (fboundp 'haskell-mode)
  ;; Fix broken lack of ghc-init.
  (defun ghc-init () t)
  (load "~/.emacs.d/init-haskell.el"))

;; key-chord to save my hands

(use-package key-chord
  :config
  (key-chord-mode 1)
  ;; access the C-c keymap with a comfortable key-chord
  (key-chord-define-global "jk" mode-specific-map))

;; good key chords from
;; <http://www.johndcook.com/blog/2015/02/01/rare-bigrams/> via
;; <https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-key-chord.el>

;; BF BG BQ BX BZ
;; CJ CV
;; DX DZ
;; FB FJ FQ FV FZ
;; GB GJ GP GQ GV GX GZ
;; HJ HQ HV
;; JJ JC JF JG JH JK JL JM JP JQ JS JT JV JW JX JY JZ
;; KK KJ KQ KV KX KZ
;; LJ LQ
;; MJ MQ MV MX MZ
;; PG PJ PQ PV PZ
;; QQ QB QF QG QH QJ QK QL QM QP QT QV QW QX QY QZ
;; SJ SX SZ
;; TJ TQ
;; VV VC VF VG VH VJ VK VM VP VQ VW VZ
;; WW WJ WQ WV WX WZ
;; XB XD XG XJ XK XM XQ XS XW XZ
;; YY YJ YQ
;; ZB ZD ZF ZG ZJ ZK ZM ZP ZQ ZS ZV ZW ZX

;;; jump back and forth to and from Emacs lisp definitions with
;;; M-. and M-, and C-c C-d [C-]d to describe

(use-package elisp-slime-nav
  :commands (turn-on-elisp-slime-nav-mode elisp-slime-nav-mode)
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook 'elisp-slime-nav-mode)))

(use-package world-time-mode
  :commands world-time-list
  :bind ("C-c g t" . spw/world-time-list))

;; and set up time zones I'm interested in

(if (eq system-type 'windows-nt)

    (setq display-time-world-list '(("MST7" "Phoenix")
                                    ("GMT0BST" "London")
                                    ("CET-1CDT" "Paris")
                                    ("KST-9" "Seoul")))

  (setq display-time-world-list '(("America/Phoenix" "Phoenix")
                                  ("Europe/London" "London")
                                  ("Europe/Paris" "Paris")
                                  ("Asia/Seoul" "Seoul"))))

(use-package frames-only-mode
  ;; only under X11, thanks
  :if (call-process-shell-command "pgrep" nil nil nil "lightdm"))

;;; Documentation browsing

(defun spw/helm-dash (arg)
  (interactive "P")
  (when (eq major-mode 'haskell-mode)
    (spw/helm-dash-haskell arg)))
(bind-key "C-c d" 'spw/helm-dash)

;; TODO maybe projectile-project-root instaed?
(defun spw/helm-dash-haskell (arg)
  (let ((project-docsets (f-join (magit-toplevel) "docsets/")))
    (when (f-directory? project-docsets)
      ;; helm-dash ignores local versions of these variables, so call
      ;; the function within a let binding
      (let* ((helm-dash-docsets-path project-docsets)
             (helm-dash-common-docsets (helm-dash-installed-docsets)))
        (if arg (helm-dash-at-point) (helm-dash))))))

(use-package helm-dash
  :commands (helm-dash
             helm-dash-at-point
             helm-dash-install-docset
             helm-dash-installed-docsets)
  :config
  (use-package helm :init (require 'helm-config)))

(use-package debpaste
  :commands (debpaste-display-paste
             debpaste-paste-region
             debpaste-paste-buffer
             debpaste-delete-paste)
  :init
  (setq debpaste-user-name "spwhitton")
  :config
  ;; Straight from the README
  (delete 'debpaste-display-received-info-in-minibuffer
          debpaste-received-filter-functions)
  (define-key debpaste-command-map "i"
    'debpaste-display-received-info-in-buffer)
  (define-key debpaste-command-map "l"
    'debpaste-display-posted-info-in-buffer))

(package-initialize)
(when (fboundp 'pdf-tools-install)
  (pdf-tools-install)


  (defun spw/highlight-and-tidy ()
    "Highlight current selection and tidy mouse pointer away."
    (interactive)
    (call-interactively 'pdf-annot-add-highlight-markup-annotation)
    (call-process "xmousetidy"))

  (bind-key (kbd "<XF86Launch7>") 'spw/highlight-and-tidy pdf-view-mode-map))



;;;; ---- functions ----

(defun mwf/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens.  Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;;; toggle some features on and off to make Emacs better at prose editing

(defun spw/writing-on ()
  "Activate my prose writing features."
  (wc-mode 1)
  (variable-pitch-mode 1)
  (setq-local cursor-type 'bar)
  (unless (or (eq system-type 'windows-nt)
              (not (fboundp 'set-fringe-mode)))
    ;; (centered-window-mode 1)
    ;; indent mode need only be turned off if we're using centered
    ;; window mode
    (when (eq major-mode 'org-mode)
      (org-indent-mode 0)))
  (if (and (or (eq system-type 'windows-nt)
               (not (fboundp 'set-fringe-mode)))
           (> (window-width) 120))
      (spw/centre-window nil)))

(defun spw/writing-off ()
  "Deactivate my prose writing features."
  (wc-mode 0)
  (variable-pitch-mode 0)
  (setq-local cursor-type 'box)
  (unless (or (eq system-type 'windows-nt)
              (not (fboundp 'set-fringe-mode)))
    ;; (centered-window-mode 0)
    ;; indent mode need only be turned off if we're using centered
    ;; window mode
    (when (eq major-mode 'org-mode)
      ;; TODO: finesse this.  don't turn it on if it wouldn't be on by default
      (org-indent-mode 1)))
  (if (and (or (eq system-type 'windows-nt)
               (not (fboundp 'set-fringe-mode)))
           (> (window-width) 120))
      (delete-other-windows)))

(defun spw/writing-toggle ()
  "Toggle on and off my prose writing features."
  (interactive)
  (let ((activate (if (boundp 'buffer-face-mode) buffer-face-mode)))
    (if activate
        (spw/writing-off)
      (spw/writing-on))))
;; note: activate this in a whole file tree by putting
;; e.g. `((org-mode . ((eval . (spw/writing-toggle)))))` in
;; .dir-locals.el

(defun spw/eval-surrounding-sexp (levels)
  "Go up LEVELS sexps from point and eval.

Originally from http://stackoverflow.com/a/2172827"
  (interactive "p")
  (save-excursion
    (if (looking-at "(")
        (progn (forward-char 1) (eval-surrounding-sexp 1))
      (up-list (abs levels))
      (eval-last-sexp nil))))

;;; backwards and forward deletions of words

(defun spw/delete-word (arg)
  "Delete ARG characters forward until encountering the end of a word."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun spw/backward-delete-word-dwim (arg)
  "Delete characters ARG backward until encountering the end of a
word, unless the region is active, in which case kill it."
  (interactive "p")
  (if (region-active-p)
      (call-interactively 'kill-region)
    (spw/delete-word (- arg))))

(bind-key "C-w" 'spw/backward-delete-word-dwim)
;; (bind-key "C-x C-k" 'kill-region)
;; (global-set-key "\M-d" 'spw/delete-word)

;;; my buffer save cleanup functions

(defun spw/delete-trailing-whitespace-except-current-line ()
  "Delete trailing whitespace on all lines except the current one.

Originally from <http://stackoverflow.com/a/3533815>."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (1+ end) (point-max))
          (delete-trailing-whitespace))))))

(defun spw/compact-blank-lines ()
  "Replace multiple empty blank lines in the buffer with single blank lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\n\n\n+" nil "noerror")
      (replace-match "\n\n"))))

(defun spw/clean-lisp-dangling-brackets ()
  "Clean up dangling brackets."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^[[:space:]\\)]*\)[[:space:]\\)]*$" nil "noerror")
      (save-excursion
        (previous-line)
        (beginning-of-line)
        (when (not (looking-at ".*;.*"))
          (next-line)
          (delete-indentation))))))

(defun spw/auto-cleanup ()
  "Unoffensive automatic cleanups."
  (interactive)
  (case major-mode
    (haskell-mode
     (spw/delete-trailing-whitespace-except-current-line)
     (spw/compact-blank-lines))
    (python-mode
     (spw/delete-trailing-whitespace-except-current-line)
     (spw/compact-blank-lines))
    (message-mode
     (save-excursion
       (message-goto-body)
       (save-restriction
         (narrow-to-region (point) (point-max))
         ;; (fill-region (point-min) (point-max))
         (whitespace-cleanup))))
    (emacs-lisp-mode
     (spw/delete-trailing-whitespace-except-current-line)
     (spw/compact-blank-lines))))

(defun spw/manual-cleanup ()
  "Clean up a buffer depending on major mode.

Sufficiently aggressive clean-ups that should not be called
automatically."
  (interactive)
  (spw/auto-cleanup)
  (untabify (point-min) (point-max))
  (unless (or (not (fboundp 'aggressive-indent-mode))
              (eq major-mode 'haskell-mode)
              aggressive-indent-mode)
    (indent-region (point-min) (point-max)))
  (case major-mode
    (emacs-lisp-mode
     (spw/clean-lisp-dangling-brackets))
    (haskell-mode
     (haskell-mode-stylish-buffer))))

;; (add-hook 'before-save-hook 'spw/auto-cleanup)

;;; Typing Hangul

(defun spw/input-method-setup ()
  "Set up or tear down hangeul input method."
  (cond ((equal current-language-environment "English")
         (set-input-method nil))
        ((equal current-language-environment "Korean")
         (set-input-method "korean-hangul"))))
(add-hook 'set-language-environment-hook 'spw/input-method-setup)

(defun spw/toggle-language-environment ()
  "Toggle typing hangeul."
  (interactive)
  (set-language-environment
   (if (equal current-language-environment "English")
       "Korean" "English")))

;; <menu> should activate ibus Korean typing but in case we want the
;; Emacs version, bind that to S-<menu>.  Hanja key is the same as the
;; key imenu uses: Alt Gr.  I might want to use this as a compose key
;; outside of Emacs but only when Hangeul typing is disabled so it's
;; okay to find it for this purpose.

(bind-key "S-<menu>" 'spw/toggle-language-environment)
(bind-key "<Multi_key>" 'hangul-to-hanja-conversion)

;; kill the binding korea-utils.el seems to be setting

(global-unset-key (kbd "S-SPC"))

(defun spw/centre-window (arg)
  "Make editing window 95 cols wide and centre it in the frame.

With argument ARG, also bound it on the right."
  (interactive "P")
  (delete-other-windows)
  (split-window-horizontally)
  (if arg (split-window-horizontally))
  (shrink-window-horizontally (- (window-width) (/ (- (frame-width) 97) 2)))
  (switch-to-buffer "*blank*")
  (toggle-read-only 1)
  (setq mode-line-format nil)
  (other-window 1)
  (when arg
    (shrink-window-horizontally (- (window-width) 95))
    (other-window 1)
    (switch-to-buffer "*blank*")
    (other-window -1)))

(defun spw/toggle-window-split ()
  "Toggle the orientation of a two-window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;; join up setqs when editing Emacs config

(defun spw/join-setqs ()
  "Interactively join a series of setq forms into a single definition."
  (interactive)
  (open-line 1)
  (insert "(setq)")
  (next-line)
  (beginning-of-line)
  (while (looking-at "(setq")
    (call-interactively 'sp-kill-word)
    (sp-backward-sexp)
    (sp-join-sexp)
    (next-line)
    (beginning-of-line))
  (sp-backward-sexp)
  (next-line)
  (delete-indentation)
  (beginning-of-line)
  (mark-sexp)
  (indent-region (region-beginning) (region-end)))

(defun magnars/move-beginning-of-line-dwim (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;; tidy up troublesome unicode

(defun gleitzman/unicode-hunt ()
  "Destroy some special Unicode characters like smart quotes.

Originally from <http://blog.gleitzman.com/post/35416335505/hunting-for-unicode-in-emacs>."
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("[\u2013\|\u2014]" . "-")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun prelude/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun magnars/new-line-dwim ()
  "Smart way to open lines below.  Originally by magnars."
  (interactive)
  (let* ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                              (and (looking-back ">" 1) (looking-at "<"))
                              (and (looking-back "(" 1) (looking-at ")"))
                              ;; we always break out in elisp since
                              ;; it's easier to see for writing code,
                              ;; and then my cleanup function will
                              ;; handle the dangling parentheses
                              (and (eq major-mode 'emacs-lisp-mode)
                                   (looking-at ")"))
                              (and (looking-back "\\[" 1) (looking-at "\\]"))))
         (break-open-list (and (eq major-mode 'org-mode)
                               (not break-open-pair))))
    (if break-open-list
        (org-meta-return)
      (newline)
      (when break-open-pair
        (save-excursion
          (newline)
          (indent-for-tab-command))))
    (indent-for-tab-command)))

;;; my functions for a very useful eshell

(defun evil/resize-window (new-size &optional horizontal)
  "Set the current window's with or height to NEW-SIZE.

With optional argument HORIZONTAL, change the horizontal size instead.

Originally from evil's `evil-window.el'."
  (let ((wincfg (current-window-configuration))
        (nwins (length (window-list)))
        (count (if horizontal
                   (- new-size (window-width))
                 (- new-size (window-height)))))
    (catch 'done
      (save-window-excursion
        (while (not (zerop count))
          (if (> count 0)
              (progn
                (enlarge-window 1 horizontal)
                (setq count (1- count)))
            (progn
              (shrink-window 1 horizontal)
              (setq count (1+ count))))
          (if (= nwins (length (window-list)))
              (setq wincfg (current-window-configuration))
            (throw 'done t)))))
    (set-window-configuration wincfg)))

(defun spw/small-vertical-split (&optional height)
  "Make a HEIGHT-lines vertical split.  HEIGHT defaults to 8."
  (interactive)
  (let ((split-height (or height 8)))
    (split-window-vertically)
    (other-window 1)
    (evil/resize-window split-height)))

(defun spw/persp-eshell (arg)
  "Switch to perspective's eshell or create it.

With arg ARG, put shell in current window."
  (interactive "P")
  (let* ((in-project (and (projectile-project-p)
                          (not (equal (persp-name persp-curr) "main"))))
         (correct-eshell-name (if in-project
                                  (concat "*eshell* (" (persp-name persp-curr) ")")
                                "*eshell"))
         (eshell-window (get-buffer-window correct-eshell-name))
         (cd-to default-directory))
    ;; first check if eshell already visible
    (if eshell-window
        (select-window eshell-window)
      ;; create split
      (unless arg (spw/small-vertical-split))
      ;; switch to buffer
      (if in-project
          ;; we're in a project: name buffer carefully
          (if (get-buffer correct-eshell-name)
              (switch-to-buffer correct-eshell-name)
            (eshell "a")
            (rename-buffer correct-eshell-name))
        ;; we're not in a project: don't
        (if (get-buffer "*eshell*")
            (switch-to-buffer "*eshell*")
          (call-interactively 'eshell))))
    ;; now change current dir and position the cursor
    (end-of-buffer)
    (eshell-bol)
    (ignore-errors (kill-line))
    (unless (string= default-directory cd-to)
      (insert (concat "cd " cd-to))
      (eshell-send-input))))

(defun spw/dired-jump (arg)
  "Call `dired-jump'.  Unless ARG, in a small vertical split."
  (interactive "P")
  (if arg
      (dired-jump)
    (spw/small-vertical-split 10)
    (let ((dired-name (buffer-file-name)))
      (dired-jump nil dired-name))))

;;; Sariul functions

(defun spw/tblesson (grade lesson period)
  "Start a textbook-based lesson plan for grade GRADE, lesson LESSON, period PERIOD."
  (interactive "sGrade: \nsGrade %s, lesson: \nsGrade %s, lesson %s, period: ")
  (let* ((read-only (not (string= (system-name) "SPWHITTON")))
         (teaching (if read-only "s:/" "~/Documents/Teaching"))
         (parent (f-join teaching grade lesson))
         (path (f-join teaching grade lesson period))
         (name (f-join path (concat grade "-" lesson "-" period ".org"))))
    (if (not read-only)
        (projectile-persp-switch-project teaching)
      (persp-switch "Teaching")
      (dired teaching))
    (when (and (not (f-exists? name))
               (not read-only))
      (f-mkdir parent)
      (f-mkdir path))
    (if read-only
        (dired (f-dirname name))
      (find-file name))
    (when (and (not (f-exists? name))
               (not read-only))
      (insert "tblesson")
      ;; (if evil-mode (evil-append-line 1))
      (yas-expand))))

(defun spw/textbook (grade lesson)
  "Open the textbook's accompanying CD for grade GRADE and lesson LESSON."
  (interactive "sGrade: \nsGrade %s, lesson: ")
  (let* ((g (string-to-number grade))
         (l (string-to-number lesson))
         (cd (if (and (<= g 4) (<= l 7))                "1"
               (if (and (<= g 4) (> l 7))               "2"
                 (if (and (>= g 5) (<= l 5))            "1"
                   (if (and (>= g 5) (> l 5) (<= l 10)) "2"
                     (if (and (>= g 5) (> l 10))        "3"))))))
         (circle (if (= g 3) "①" "②"))
         (path (if (<= g 4)
                   (concat "C:\\e-Book\\start\\초등영어\\3~4학년군 영어 "
                           circle " 지도서_CD " cd "\\start.exe")
                 (concat "D:\\Textbook CDs\\Grade "
                         grade "\\CD" cd "\\start.exe"))))
    (call-process path nil 0)))

(defun spw/teachers-book (grade lesson)
  "Open the textbook's teacher's guide for grade GRADE and lesson LESSON."
  (interactive "sGrade: \nsGrade %s, lesson: ")
  (let* ((lesson-string (if (<= (string-to-number grade) 4)
                            (concat "_" lesson "단")
                          (concat "L" lesson "-")))
         (path (car (f-glob (concat "D:\\Teacher's guides\\*" grade "]*" lesson-string "*.pdf")))))
    (find-file path)))

(defun spw/auto-textbook ()
  "Call `spw/textbook', guessing appropriate arguments from current directory."
  (interactive)
  (let* ((path (split-string default-directory "/"))
         (grade (car (last path 4)))
         (lesson (car (last path 3))))
    (spw/textbook grade lesson)))

(defun spw/auto-teachers-book ()
  "Call `spw/teachers-book', guessing appropriate arguments from current directory."
  (interactive)
  (let* ((path (split-string default-directory "/"))
         (grade (car (last path 4)))
         (lesson (car (last path 3))))
    (spw/teachers-book grade lesson)))

(defun spw/persp-clone (new-name)
  "Clone current perspective with the name NEW-NAME."
  (interactive "sNew name: \n")
  (make-persp
   :name new-name
   :buffers (persp-buffers persp-curr)
   :window-configuration (current-window-configuration))
  (persp-switch new-name))

(defun spw/align-dwim ()
  "Align, if region inactive first mark paragraph."
  (interactive)
  ;; should do this kind of aligning with spaces only
  (let ((indent-tabs-mode nil))
    (if (region-active-p)
        (let ((align-region-separate 'entire))
          (call-interactively 'align))
      (save-excursion
        (mark-paragraph)
        (call-interactively 'align)))))

(defun crowding/local-set-minor-mode-key (mode key def)
  "Overrides a minor mode MODE's binding to KEY with DEF for the local buffer.

Does this by creating or altering keymaps stored in buffer-local
`minor-mode-overriding-map-alist'.

From <http://stackoverflow.com/a/14769115>."
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)
                       map))))
    (define-key newmap key def)))

(defun spw/set-from-address ()
  "Set e-mail From: address correctly by looking at other headers."
  (interactive)
  (save-excursion
    (message-narrow-to-headers)
    (goto-char (point-min))
    (when (or (search-forward-regexp "^To:.*arizona.edu" nil t)
              (search-forward-regexp "^Cc:.*arizona.edu" nil t)
              (search-forward-regexp "^Bcc:.*arizona.edu" nil t))
      (goto-char (point-min))
      (search-forward-regexp "^From:.*$")
      (replace-match "From: Sean Whitton <spwhitton@email.arizona.edu>"))
    (widen)))

;;; Find and open projects in ~/src/ that aren't yet known to
;;; projectile.  Inspired by
;;; <https://alanpearce.uk/post/opening-projects-with-projectile>.

(defvar programming-projects-dir (expand-file-name "~/src"))

(defun spw/get-programming-projects (dir)
  "Find all projectile projects in DIR that are presently unknown to projectile."
  (-filter (lambda (d)
             (and (file-directory-p d)
                  (not (-contains?
                        projectile-known-projects
                        (f-slash (replace-regexp-in-string (expand-file-name "~") "~" d))))
                  (-any? (lambda (f) (funcall f d))
                         projectile-project-root-files-functions)))
           (directory-files dir t "^[^.]")))

(defun spw/open-programming-project (arg)
  "Open a programming project that is presently unknown to projectile.

Passes ARG to `projectile-switch-project-by-name'."
  (interactive "P")
  (let ((project-dir
         (projectile-completing-read "open new project: "
                                     (spw/get-programming-projects programming-projects-dir))))
    (projectile-switch-project-by-name project-dir arg)))

(defun spw/pandoc-paper-compile (arg)
  "Compile a paper to PDF with pandoc into ~/tmp.

If ARG, put into my annex instead.

Lightweight alternative to both pandoc-mode and ox-pandoc.el.

Generates calls to pandoc that look like this: pandoc -s --filter pandoc-citeproc --bibliography=$HOME/doc/spw.bib --filter pandoc-citeproc-preamble --template pessay -V documentclass=pessay input.[md|org] -o output.pdf"
  (interactive "P")
  (when (and (string= default-directory (expand-file-name "~/doc/papers/"))
             (or (eq major-mode 'markdown-mode)
                 (eq major-mode 'org-mode)))
    (let ((output-file (f-join (if arg "~/lib/annex/doc/papers" "~/tmp")
                               (f-filename (f-swap-ext (buffer-file-name) "pdf")))))
      (when (and arg (f-symlink? output-file))
        (call-process-shell-command
         "git" nil "*git annex output*" nil
         "-C" "~/lib/annex" "annex" "unlock" output-file))
      (call-process-shell-command
       "pandoc" nil "*pandoc output*" nil
       "-s" "--filter" "pandoc-citeproc"
       (concat "--bibliography=" (expand-file-name "~/doc/spw.bib"))
       "--filter" "pandoc-citeproc-preamble"
       "--template" "pessay" "-V" "documentclass=pessay"
       (buffer-file-name) "-o" output-file)
      (find-file output-file))))

;; Move lines around smartly.  Originally from
;; <http://emacswiki.org/emacs/CopyingWholeLines>.

(defun spw/copy-line (arg)
  "Copy ARG lines into the kill ring, and move to the start of the next line.

If region is active, instead call `kill-ring-save'.

Appends to the kill ring entry on sequential calls.

Ensures the kill ring entry always ends with a newline."
  (interactive "p")
  (if mark-active
      (call-interactively 'kill-ring-save)
    (let ((beg (point))
          (end (line-end-position arg)))
      (if (eq last-command 'spw/copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end))
      (kill-append "\n" nil)
      (beginning-of-line (or (and arg (1+ arg)) 2))
      (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))))

(defhydra hydra-whole-lines (global-map "C-c" :color red)
  "whole lines"

  ;; The following might do something smarter for LISPs
  ("k" kill-whole-line "kill another whole line" :color red)
  ("/" undo "get that line back" :color red
   ;; override bind so not bound outside the hydra
   :bind nil))

(defun spw/save-dir ()
  "Copy buffer's directory to kill ring."
  (interactive)
  (kill-new default-directory)
  (message (concat "Saved \"" default-directory "\" to the kill ring")))

(defun spw/tile ()
  "Tile three buffers."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (call-interactively 'ido-switch-buffer)
  (split-window-below)
  (other-window 1)
  (call-interactively 'ido-switch-buffer)
  (other-window 1)
  (persp-basewc-save)
  (message "Perspective base window configuration saved; C-c q q to restore it"))

;; Access the power of git grep: from
;; <http://stackoverflow.com/a/25633595>

;; alternative: https://www.ogre.com/node/447

(defcustom git-grep-command "git --no-pager grep --no-color --line-number <C> <R> `git rev-parse --show-toplevel`"
  "The command to run with `git-grep'.")
(defun git-grep (regexp)
  "Search for REGEXP using `git grep' in the current directory."
  (interactive "sRegexp: ")
  (unless (boundp 'grep-find-template) (grep-compute-defaults))
  (let ((old-command grep-find-template))
    (grep-apply-setting 'grep-find-template git-grep-command)
    (rgrep regexp "*" """")
    (grep-apply-setting 'grep-find-template old-command)))

(defun spw/open-term-here ()
  "Open a fresh urxvt terminal in current directory."
  (interactive)
  (call-process "urxvtcd" nil "*errors*" nil
                "-cd" (expand-file-name  default-directory)
                "-e"  "/bin/zsh"))

(defun spw/world-time-list ()
  (interactive)
  (make-frame)
  (other-frame 1)
  (world-time-list))

;; Make `C-x z' repeat zap-up-to-char without requiring typing the
;; char again.  From Chris Done's Emacs config.

(defvar zap-up-to-char-last-char nil
  "The last char used with zap-up-to-char-repeateable.")
(defvar zap-up-to-char-last-arg 0
  "The last direction used with zap-up-to-char-repeateable.")
(defun zap-up-to-char-repeatable (arg char)
  "Case is ignored if `case-fold-search' is non-nil in the current buffer.
  Goes backward if ARG is negative; error if CHAR not found."
  (interactive (if (and (eq last-command 'zap-up-to-char-repeatable)
                        (eq 'repeat real-this-command))
                   (list zap-up-to-char-last-arg
                         zap-up-to-char-last-char)
                 (list (prefix-numeric-value current-prefix-arg)
                       (read-char "Zap to char: " t))))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (let ((start (point))
        (end (save-excursion
               (when (eq last-command 'zap-up-to-char-repeatable)
                 (forward-char))
               (search-forward (char-to-string char) nil nil arg)
               (forward-char -1)
               (point))))
    (cond
     ((and (eq last-command 'zap-up-to-char-repeatable)
           (eq 'repeat real-this-command))
      (let ((last-command 'kill-region))
        (kill-region start end)))
     (t
      (kill-region start end))))
  (setq zap-up-to-char-last-char char)
  (setq zap-up-to-char-last-arg arg)
  (setq this-command 'zap-up-to-char-repeatable))

(defun spw/strip-text-properties (txt)
  "From http://stackoverflow.com/questions/8372722/print-only-text-discarding-text-properties"
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun spw/dotfiles-rebase ()
  "Rebase & push dotfiles."
  (interactive)
  (let ((default-directory (expand-file-name "~/src/dotfiles/"))
        (buffer (get-buffer-create "*dotfiles rebase*")))
    (display-buffer "*dotfiles rebase*")
    (async-shell-command "git-dotfiles-rebase" "*dotfiles rebase*")))

;;; defeat variable-pitch-mode for avy and Org tables and source
;;; blocks, per http://stackoverflow.com/a/16819449

(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(defun face-override-variable-pitch (face)
  (set-face-attribute
   face nil
   :inherit
   (my-adjoin-to-list-or-symbol
    'fixed-pitch
    (face-attribute face :inherit))))



;;;; ---- personal settings ----

;;; no tabs please

(setq-default indent-tabs-mode nil)

;;; key bindings

;; I don't often want to quit
(bind-key "C-x C-c" 'delete-frame)

;; opening new lines below
(bind-key "M-RET" 'magnars/new-line-dwim)

;; `reindent-then-newline-and-indent' tends to get things wrong more
;; often than it gets things right with my typing habits.  I hit <TAB>
;; a lot.
(bind-key "RET" 'newline-and-indent)

;; fixup-whitespace seems to make just-one-space redundant
(bind-key "M-SPC" 'fixup-whitespace)

;; never want to send any e-mail
(unbind-key "C-x m")

;; fallback expanding
(bind-key "M-/" 'hippie-expand)

;; smart versions of C-a, M-w from magnars
(bind-key "C-a" 'magnars/move-beginning-of-line-dwim)
(bind-key "M-w" 'spw/copy-line)

;; don't think about narrowing and widening
(bind-key "C-c n" 'mwf/narrow-or-widen-dwim)

;; aligning Haskell
(bind-key "C-c a" 'spw/align-dwim)
(bind-key "C-c A" 'align-regexp)

;; copy current directory for use in a shell or moving a file in dired
(bind-key "C-c D" 'spw/save-dir)

;; don't need default M-m binding as have smarter C-a
(bind-key "M-m" 'zap-up-to-char-repeatable)

;;; launching

(bind-key "C-c g g" 'spw/open-term-here)
(bind-key "C-c g k" 'kill-emacs)
(bind-key "C-c g c" 'spw/manual-cleanup)
(bind-key "C-c g l" 'spw/tblesson)
(bind-key "C-c g r" '(lambda ()
                       (interactive)
                       (projectile-persp-switch-project "~/src/dotfiles")
                       (find-file "~/src/dotfiles/.emacs.d/init.el")
                       (eval-buffer)))
(bind-key "C-c g d" 'spw/dotfiles-rebase)

(bind-key "C-c S l" 'spw/tblesson)
(bind-key "C-c S S" 'spw/auto-textbook)
(bind-key "C-c S s" 'spw/textbook)
(bind-key "C-c S t" 'spw/auto-teachers-book)

;;; toggling

(bind-key "C-c t e" 'toggle-debug-on-error)
(bind-key "C-c t i" 'org-indent-mode)
(bind-key "C-c t w" 'spw/writing-toggle)

;;; evaluation

(bind-key "C-c e e" 'spw/eval-surrounding-sexp)
(bind-key "C-c e f" 'eval-defun)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e b" 'eval-buffer)
(bind-key "C-c E" 'prelude/eval-and-replace)

;;; insertion

(bind-key "C-c i h" 'add-file-local-variable-prop-line)

;;; Conditionally disable C-z as interacts badly with xmonad
(defun suspend-frame--not-if-xmonad (orig-fun &rest args)
  (unless window-system
    (apply orig-fun args)))
(advice-add 'suspend-frame :around #'suspend-frame--not-if-xmonad)

;;; abbreviations

(setq abbrev-file-name "~/doc/emacs-abbrevs")

;; turn on for all buffers, if our abbrevs file is checked out
(when (file-exists-p abbrev-file-name)
  (setq save-abbrevs t)
  (setq-default abbrev-mode t)
  (diminish 'abbrev-mode)
  (quietly-read-abbrev-file))

;;; bookmarks

;; again, only do stuff if our bookmarks file is checked out
(when (file-exists-p "~/doc/emacs-bookmarks")
  (setq bookmark-default-file "~/doc/emacs-bookmarks"
        ;; avoid annoying "Buffer emacs-bookmarks modified; kill anyway?" messages
        bookmark-save-flag 1)

  ;; something involved in setting bookmarks likes to try to kill the
  ;; bookmarks file buffer which means an annoying y/n query since
  ;; something likes setting the modified flag without actually
  ;; modifying anything.  So save it, or for the very frequently
  ;; called `kill-buffer', clear modification flag for these bogus
  ;; modifications
  (defun bookmark-write-file--save-bookmarks-buffer (&rest ignore)
    (when (get-buffer "emacs-bookmarks")
      (with-current-buffer (get-buffer "emacs-bookmarks")
        (save-buffer))))
  (advice-add 'bookmark-write-file :before #'bookmark-write-file--save-bookmarks-buffer)
  (defun kill-buffer--clear-modified (&rest ignore)
    (when (get-buffer "emacs-bookmarks")
      (with-current-buffer (get-buffer "emacs-bookmarks")
        (set-buffer-modified-p nil))))
  (advice-add 'kill-buffer :before #'kill-buffer--clear-modified))

;;; miscellaneous personal settings

;; isearch should leave you at the beginning of the match
(add-hook 'isearch-mode-end-hook 'spw/isearch-match-beginning)
(defun spw/isearch-match-beginning ()
  "Move point to beginning of isearch match."
  (when isearch-other-end
    (when isearch-forward (goto-char isearch-other-end))))

;; save script files as executable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; always update file contents
(global-auto-revert-mode 1)

;; and a binding to do so manually
(bind-key "C-x C-r" (lambda () (interactive) (revert-buffer nil t)))

;; mark settings
(setq transient-mark-mode t
      set-mark-command-repeat-pop t)

;; recursive minibuffers
(setq enable-recursive-minibuffers t)

;; explicitly end sentences with double spaces
(setq sentence-end-double-space t)

;; C-n can't move us past the end of the buffer
(setq next-line-add-newlines nil)

;; re-indent and add newlines automatically, sometimes
;; (electric-layout-mode 1)
(electric-indent-mode 1)

;; templates when creating new files
;; (auto-insert-mode 1)

;; disable for python mode where it makes a mess
(defun electric-indent-ignore-python ()
  "Ignore electric indentation for `python-mode'."
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
;;(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; browser
(setq browse-url-generic-program "iceweasel"
      browse-url-browser-function 'browse-url-generic)

;; clipboard & primary selection: see https://www.gnu.org/software/emacs/manual/html_node/emacs/Clipboard.html

;; with this setup, C-y and M-w access the clipboard, while selecting
;; a region and then pressing C-g and middle click access the primary
;; selection

(setq select-active-regions t
      mouse-drag-copy-region t
      x-select-enable-primary nil
      x-select-enable-clipboard t
      mouse-yank-at-point t
      yank-pop-change-selection t
      save-interprogram-paste-before-kill t
      x-select-enable-clipboard-manager t)

(global-set-key [mouse-2] 'mouse-yank-primary)

;; require a buffer to have a final newline
(setq require-final-newline 'visit-save)

;; scrolling
(setq scroll-preserve-screen-position t)

(setq switch-to-visible-buffer nil)
(setq ido-default-buffer-method 'selected-window)

;; dabbrev should be case-insensitive
(setq dabbrev-case-fold-search t)

;; view mode should be read-only
(setq view-read-only t)

;;; don't ask me before following symlinks to files in version control
(setq vc-follow-symlinks t)

;;; colours in comint modes

(ansi-color-for-comint-mode-on)

;;; avoid some prompts when saving all buffers

(defun spw/save-org-buffers-first (&rest ignore)
  "Save all Org buffers without prompting."
  (when (featurep 'org)
    ;; gotta remove this advice first since `org-save-all-org-buffers'
    ;; calls `save-some-buffers'
    (advice-remove 'save-some-buffers #'spw/save-org-buffers-first)
    (org-save-all-org-buffers)
    (advice-add 'save-some-buffers :before #'spw/save-org-buffers-first)))
(advice-add 'save-some-buffers :before #'spw/save-org-buffers-first)

;;; show column numbers as well as line numbers in the mode line

(setq column-number-mode t)



;;;; ---- modes configuration ----

;;; auto fill comments in modes with a reliable comment syntax

(defun spw/turn-on-comment-filling ()
  "Turn on filling comments."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(add-hook 'emacs-lisp-mode-hook 'spw/turn-on-comment-filling)

;;; mail mode for mutt

(use-package message
  :mode ("/mutt-.*$" . message-mode)
  :init

  ;; used in message-mode yasnippets
  (defun spw/recipient-first-name ()
    (save-excursion
      (message-goto-to)
      (message-beginning-of-line)
      ;; handle Microsoft Exchange
      (when (looking-at "\"")
        (forward-word 1)
        (forward-char 2))
      (let ((beg (point))
            (end (progn (forward-word 1) (point))))
        (filter-buffer-substring beg end))))

  (defun spw/fix-initial-signature (&rest ignore)
    "Ensure enough space above signature to type."
    (when (looking-at "
-- ")
      (open-line 1)))
  (defun spw/fix-signature-kill (&rest ignore)
    "Ensure enough space above signature to type."
    (unless (looking-back "

")
      (newline))
    (unless (looking-back "

")
      (newline))
    (spw/fix-initial-signature))

  ;; slightly modify C-c C-z behaviour: fix Mutt and Emacs which both
  ;; think that there doesn't need to be a newline before the
  ;; signature dashes
  (add-hook 'message-mode-hook 'spw/fix-initial-signature)
  ;; (advice-add 'message-kill-to-signature :after #'spw/fix-signature-kill)

  (defun spw/debbugs-no-ack ()
    (save-excursion
      (message-goto-to)
      (when (looking-back ".*bugs\.debian\.org.*")
        (message-carefully-insert-headers (list (cons 'X-Debbugs-No-Ack "thanks"))))
      (message-goto-cc)
      (when (looking-back ".*bugs\.debian\.org.*")
        (message-carefully-insert-headers (list (cons 'X-Debbugs-No-Ack "thanks"))))))

  (defun message-newline-and-reformat--delete-superflous-newlines (&rest ignore)
    "Have `message-newline-and-reformat' get rid of some more
superflous blank quoted lines."
    (save-excursion
      (beginning-of-line)
      (when (looking-at ">[[:space:]]*$")
        (kill-line 1))))
  (advice-add 'message-newline-and-reformat
              :before #'message-newline-and-reformat--delete-superflous-newlines)

  (setq mail-header-separator "")

  (bind-key "C-c C-c" (lambda ()
                        (interactive)
                        (save-buffer)
                        (delete-frame)) message-mode-map)

  (add-hook 'message-mode-hook
            (lambda ()
              (auto-fill-mode)
              ;; (spw/set-from-address)
              (footnote-mode)
              (spw/debbugs-no-ack)
              (message-goto-body))))

(defun djcb/snip (b e summ)
  "Replace region B to E with SUMM like this: [snip:summary (n lines)]."
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]"
                    (if (= 0 (length summ)) "" (concat ": " summ))
                    n
                    (if (= 1 n) "" "s")))))

;;; IELM

(setq ielm-dynamic-return nil)

;;; text mode

(add-hook 'text-mode 'turn-on-auto-fill)
(diminish 'auto-fill-function)
;; (add-hook 'text-mode 'refill-mode)

;;; dired

(use-package dired-x)
(setq-default dired-omit-mode t)
(setq dired-omit-files "^\\...+$")
(setq dired-isearch-filenames 'dwim)

;; dired omit mode mapping conflicts with my avy binding
(define-key dired-mode-map (kbd "M-o") 'nil)
(bind-key "C-c g o" 'dired-omit-mode dired-mode-map)

;;; LaTeX

(setq TeX-auto-save t
      TeX-parse-self t
      LaTeX-indent-level 4
      LaTeX-item-indent -2
      TeX-newline-function 'reindent-then-newline-and-indent)

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince %o")
        ("^html?$" "." "iceweasel %o"))))

;;; fixes for exporting from Org-mode

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
(setq-default TeX-master t)
(make-variable-buffer-local 'TeX-master)

;;; the Emacs calculator

;; disable line numbering primarily so that killing and copying stack
;; entries puts the number alone in the kill-ring
(setq calc-line-numbering nil)
(put 'narrow-to-region 'disabled nil)

;; on my "Amazon Basics" keyboard
(bind-key "<XF86Calculator>" 'calc)

;;; javascript

;; don't insert a newline after a semicolon
(add-hook
 'js-mode-hook
 (lambda ()
   (setq-local electric-layout-rules
               (remove (quote (?\; . after)) electric-layout-rules))))

;;; eshell prompt

;; from the Emacs Wiki [[EshellPrompt]]
(defun fish-path (path max-len)
  "Maybe trim PATH down to MAX-LEN (sans slashes).

Replaces parent directories with their initial characters."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(setq eshell-prompt-function
      (lambda nil
        (concat
         (propertize (car (split-string (system-name) "\\.")) 'face `(:foreground "#DFAF8F"))
         " "
         (propertize (fish-path (eshell/pwd) 40) 'face `(:foreground "#60b48a"))
         (propertize " $" 'face `(:foreground "#506070"))
         " ")))
(setq eshell-highlight-prompt nil)

;;; changelogs

(defun spw/change-log-setup ()
  (setq-local indent-tabs-mode nil)
  (setq-local left-margin 2)
  (orgstruct++-mode 1))
(add-hook 'change-log-mode-hook 'spw/change-log-setup)

(setq debian-changelog-mailing-address "spwhitton@spwhitton.name")

(provide 'init)
;;; init.el ends here
