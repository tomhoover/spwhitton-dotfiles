;;; Sean's Emacs configuration

;;;; ---- security ----

;; don't accept invalid SSL certs
(eval-after-load 'gnutls
  '(setq gnutls-verify-error t))

;;;; ---- package management ----

;;; ~/.emacs.d

;; libs in ~/.emacs.d/site-lisp can override system packages
;; This is for my personal, possibly patched versions of libraries.
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;; libs in ~/.emacs.d/lisp are overridden by system packages
;; This is for fallback copies of libraries needed to init Emacs.
;; Possible alternate name, if this is breaking Emacs conventions for
;; dirs called 'lisp': initlibs
(add-to-list 'load-path (concat user-emacs-directory "lisp") t)

;; be sure not to load stale byte-compiled lisp
(setq load-prefer-newer t)

;;; `use-package'

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Marking packages as optional:
;;
;;   (use-package foo
;;     :if (spw--optional-pkg-available-p "foo"))
;;
;; This causes `use-package' to silently ignore foo's config if the
;; package is not available.  We use this for packages like `magit',
;; which is nice to have, but not needed for most uses of Emacs.
;; 
;; Some packages are necessary to properly init my standard editing
;; environment.  For example, without `key-chord', I would type a lot
;; of spurious 'j's and 'k's into buffers.
;; 
;; Such packages are not marked as optional, and `use-package' will
;; complain at startup if they are not available.  Fallback copies
;; should be present in ~/.emacs.d/lisp.

(defun spw--optional-pkg-available-p (pkg)
  (or (bound-and-true-p use-package-always-ensure)
      (locate-library pkg)))

;;; MELPA and friends

;; these lines are for use on hosts on which I cannot install system
;; packages, but still want my optional packages to be available

;; (setq
;;  ;; install all packages
;;  use-package-always-ensure t
;;
;;  ;; set up standard package sources
;;  package-user-dir (expand-file-name "~/local/elpa")
;;  package-archives
;;  '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
;;    ("MELPA Stable" . "https://stable.melpa.org/packages/")
;;    ("MELPA" . "https://melpa.org/packages/"))
;;  package-archive-priorities
;;  '(("GNU ELPA" . 10)
;;    ("MELPA Stable" . 5)
;;    ("MELPA" . 0)))



;;;; ---- basic settings ----

;; TRAMP and zsh are not friends
(setenv "SHELL" "/bin/bash")

;;; customisation -- must be loaded early so that zenburn theme is
;;; considered safe

(defconst custom-file (concat user-emacs-directory "init-custom.el"))
(load custom-file 'noerror)

;; when I do elisp experimentation I use IELM, so we can make
;; *scratch* easier to use
(setq initial-major-mode 'text-mode)

;;; load terminal fixes

(load-file (concat user-emacs-directory "init-term.el"))

;;; put backups and autosaves in /tmp

;; set up tmp files dir
(defconst emacs-tmp-dir
  (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(make-directory emacs-tmp-dir t)
(chmod emacs-tmp-dir (string-to-number "700" 8))

(setq
 ;; now teach Emacs to put stuff there
 backup-directory-alist `((".*" . ,emacs-tmp-dir))
 tramp-backup-directory-alist backup-directory-alist
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
 auto-save-list-file-prefix emacs-tmp-dir
 tramp-auto-save-directory emacs-tmp-dir

 ;; avoid clobbering symlinks
 backup-by-copying t

 ;; disable backups for files accessed through tramp's sudo and su
 ;; protocols, to prevent copies of root-owned files being under my uid 
 backup-enable-predicate
 (lambda (name)
   (and (normal-backup-enable-predicate name)
        (not
         (let ((method (file-remote-p name 'method)))
           (when (stringp method)
             (member method '("su" "sudo"))))))))

;;; misc display and interface settings

;; focus follow mouse

(setq mouse-autoselect-window t
      focus-follows-mouse t)

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; warn before killing Emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; don't handle keyboard events before redrawing
(setq redisplay-dont-pause t)

;; prompt to create scratch buffers: I usually use files in ~/tmp instead
(setq confirm-nonexistent-file-or-buffer t)

;; soft word wrapping for easier editing of long lines
(setq-default visual-line-mode t
              word-wrap t
              wrap-prefix "    ")
(diminish 'visual-line-mode)

;; Terminus
(add-to-list 'default-frame-alist '(font . "Terminus-11"))

;; disable GUI elements
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; cursor settings

(setq x-stretch-cursor t)
(setq-default cursor-type 'box)

;; turn off blink-cursor-mode if it ended up on
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;;; zenburn

(use-package zenburn-theme
  :if (spw--optional-pkg-available-p "zenburn-theme")
  :init
  ;; we enable the theme using an `after-init-hook' because otherwise
  ;; we have to call `package-initialize`.  See README.Debian for
  ;; elpa-zenburn-theme, and Debian bug #847690
  (add-hook 'after-init-hook (lambda () (load-theme 'zenburn))))

;;; I'm in Arizona (this is mainly for using Org-mode on hosts that
;;; have a UTC clock)

(unless (eq system-type 'windows-nt)
  (set-time-zone-rule "/usr/share/zoneinfo/America/Phoenix"))

;;; minimal session management

(use-package recentf
  :init
  (setq
   ;; in an attempt to make TRAMP a bit faster, don't ever keep remote
   ;; files in the recentf list
   recentf-keep '(file-remote-p file-readable-p)

   ;; keep recentf's file out of ~
   recentf-save-file "~/.emacs.d/recentf"))

;; Save my place in buffers, but only with newer Emacs

;; With older Emacs, the additions to `find-file-hook',
;; `kill-emacs-hook' and `kill-buffer-hook' made by `save-place' kept
;; disappearing, unless I enabled save-place using use-package's
;; `:defer' keyword.  Adding the hooks in this init file didn't work
;; either.  See older dotfiles repo commits

(when (version< "25.1" emacs-version)
  ;; if save-place is slowing down quitting Emacs, uncomment this:
  ;; (setq save-place-forget-unreadable-files nil)
  (save-place-mode 1))



;;;; ---- packages ----

;;; sexp management

(show-paren-mode 1)

(use-package elec-pair
  :commands (electric-pair-local-mode electric-pair-mode)
  :init
  ;; insert closing pairs automatically only in programming modes
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)
  :config
  ;; enhance electric-pair-mode with some more pairs
  ;; based on http://emacs.stackexchange.com/a/2554/8610
  (defmacro spw--add-mode-pairs (hook pairs)
    `(add-hook
      ,hook
      (lambda ()
        (setq-local electric-pair-pairs
                    (append electric-pair-pairs ,pairs))
        (setq-local electric-pair-text-pairs electric-pair-pairs))))
  (spw--add-mode-pairs 'emacs-lisp-mode-hook '((?` . ?')))
  (spw--add-mode-pairs 'markdown-mode-hook '((?` . ?`)))
  (spw--add-mode-pairs 'org-mode-hook '((?= . ?=))))

;; make a list of lisp editing modes
(setq lisp-major-mode-hooks '(emacs-lisp-mode-hook
                              lisp-mode-hook
                              lisp-interaction-mode-hook
                              ielm-mode-hook
                              scheme-mode-hook
                              inferior-scheme-mode-hook))

(defmacro spw--activate-in-lisp-modes (minor-mode)
  "Add hooks to activate MINOR-MODE in all the major modes with
hooks listed in `lisp-major-mode-hooks'."
  `(dolist (hook lisp-major-mode-hooks)
    (add-hook hook ,minor-mode)))

(spw--activate-in-lisp-modes 'eldoc-mode)

(defmacro spw--paredit-unsteal (map)
  "Reclaim core Emacs bindings from Paredit-like keymap MAP."
  `(progn
     ;; these are core Emacs text-editing bindings
     (define-key ,map (kbd "M-s") nil)
     (define-key ,map (kbd "M-r") nil)

     ;; check that the current version of paredit/paredit-everywhere
     ;; actually binds these keys before setting our own preferences
     (when (lookup-key ,map (kbd "M-U"))
       (define-key ,map (kbd "M-U") 'paredit-splice-sexp))
     (when (lookup-key ,map (kbd "M-<up>"))
       (define-key ,map (kbd "M-<up>") 'paredit-raise-sexp))
     
     ;; this is to fix IELM
     (define-key ,map (kbd "RET") nil)))

(use-package paredit
  :if (spw--optional-pkg-available-p "paredit")
  :commands paredit-mode
  :init
  (spw--activate-in-lisp-modes 'paredit-mode)
  :config
  (spw--paredit-unsteal paredit-mode-map))

(use-package paredit-everywhere
  :if (spw--optional-pkg-available-p "paredit-everywhere")
  :commands paredit-everywhere-mode
  :init
  (dolist (hook '(prog-mode-hook minibuffer-setup-hook))
    (add-hook hook 'paredit-everywhere-mode))
  :config
  (spw--paredit-unsteal paredit-everywhere-mode-map))

;;; Org

;; disable org-list-allow-alphabetical so that I can start lines with
;; "P. 211 - " to refer to a page and not start a bulleted list.  This
;; has to be set before loading Org, and `use-package' :preface
;; doesn't seem to be early enough
(setq org-list-allow-alphabetical nil)

(use-package org
  ;; init-org.el uses `f-glob'
  :if (spw--optional-pkg-available-p "f")
  :mode (("\\.org" . org-mode)
         ("\\.org_archive" . org-mode))
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o [" . spw/org-agenda-file-to-front)
         ("C-c o ]" . spw/org-remove-file)
         ("C-c o n" . spw/new-philos-notes))
  :commands (org-capture
             org-store-link
             org-agenda
             org-save-all-org-buffers   ; for ~/bin/sync-docs
             spw/org-agenda-file-to-front
             spw/org-remove-file
             spw/new-philos-notes)
  :config (load "~/.emacs.d/init-org.el"))

;;; more useful unique buffer names

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward))

;;; OpenWith

(use-package openwith
  :if (spw--optional-pkg-available-p "openwith")
  :commands openwith-mode
  :demand
  :config (openwith-mode 1))

;; thanks to openwith, the warning for large files can be at a much
;; larger threshold as the chances of hitting it are low (this is
;; about 100MB)

(setq large-file-warning-threshold 100000000)

;;; magit

(use-package magit
  :if (spw--optional-pkg-available-p "magit")
  :demand
  :config
  (setq
   ;; by default, don't pass -f to `git remote add`
   magit-remote-arguments nil

   magit-completing-read-function 'magit-ido-completing-read
   magit-push-always-verify nil
   magit-revert-buffers 'silent)

  (use-package magit-annex
    :if (spw--optional-pkg-available-p "magit-annex")))

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
  :if (spw--optional-pkg-available-p "rainbow-delimiters")
  :commands rainbow-delimiters-mode
  :init
  (setq-default frame-background-mode 'dark)
  (spw--activate-in-lisp-modes 'rainbow-delimiters-mode))

;;; and colour those colours

(use-package rainbow-mode
  :if (spw--optional-pkg-available-p "rainbow-mode")
  :commands rainbow-mode
  :init (dolist (hook '(html-mode-hook css-mode-hook))
          (add-hook hook 'rainbow-mode)))

;;; keep reindenting lisp

(use-package aggressive-indent
  :if (spw--optional-pkg-available-p "aggressive-indent")
  :commands aggressive-indent-mode
  :init (spw--activate-in-lisp-modes 'aggressive-indent-mode))

;;; word count in modeline

(use-package wc-mode
  :if (spw--optional-pkg-available-p "wc-mode")
  :init
  (defun spw--large-buffer-disable-wc-mode (orig-fun &rest args)
    "Disable `wc-mode' if the buffer has a large number of words.

This is a workaround for `wc-mode''s performance issues."
    (unless (> (wc nil nil 1) 4000)
      (apply orig-fun args)))
  (advice-add 'wc-mode :around #'spw--large-buffer-disable-wc-mode)

  (setq wc-modeline-format "%tw words"))

;;; company-mode for smart and easy completion

(use-package company
  :if (spw--optional-pkg-available-p "company")
  :diminish company-mode
  :config
  (defun spw--activate-company ()
    "Setup company mode.

Using this in preference to global-company-mode, with <tab> bound
to `company-complete'.  For another approach, see
https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387"
    (company-mode 1)
    (define-key (current-local-map) (kbd "M-/") 'company-complete))
  (add-hook 'prog-mode-hook 'spw/company-prog-setup)

  ;; retain my C-w binding; move company's C-w binding
  (define-key company-active-map "\C-w" nil)
  (bind-key "M-o" 'company-show-location company-active-map)

  ;; settings

  (setq company-idle-delay nil
        company-minimum-prefix-length 0
        company-echo-delay 0)

  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-transformers 'company-sort-by-occurrence))
;; usage notes:
;;
;; C-o during company isearch narrows to stuff matching that search;
;; mnemonic 'occur'.  C-M-s while outside of search to do the same
;; thing

;;; Randomize the order of lines in a region

(use-package randomize-region
  :commands randomize-region)

;;; Markdown mode

(use-package markdown-mode
  :if (spw--optional-pkg-available-p "markdown-mode")
  :mode "\\.md"

  :init
  ;; (add-hook 'markdown-mode-hook 'turn-on-orgstruct)
  ;; (add-hook 'markdown-mode-hook 'turn-on-orgstruct++)
  (add-hook 'markdown-mode-hook 'wc-mode)

  :config
  (defun spw--pandoc-compile (arg)
    (interactive "P")
    (cond
     ((string= default-directory (expand-file-name "~/doc/papers/"))
      (spw--pandoc-paper-compile arg))
     ((string= default-directory (expand-file-name "~/doc/pres/"))
      (spw--pandoc-presentation-compile arg))))
  (defun spw--pandoc-paper-compile (arg)
    "Compile a paper to PDF with pandoc into ~/tmp.

If ARG, put into my annex instead.

Lightweight alternative to both pandoc-mode and ox-pandoc.el.

Generates calls to pandoc that look like this: pandoc -s --filter pandoc-citeproc --bibliography=$HOME/doc/spw.bib --filter pandoc-citeproc-preamble --template pessay -V documentclass=pessay input.[md|org] -o output.pdf"
    (interactive "P")
    (when (and (string= default-directory (expand-file-name "~/doc/papers/"))
               (or (eq major-mode 'markdown-mode)
                   (eq major-mode 'org-mode)))
      (let ((output-file (f-filename (f-swap-ext (buffer-file-name) "pdf"))))
        (compile (concat "make " output-file
                         ;; We can easily reload the file in evince, and
                         ;; the .view target means that Emacs thinks
                         ;; compilation isn't finished until evince quits.
                         ;; (if window-system ".view" "")
                         )))))
  ;; TODO use a Makefile for this too
  (defun spw--pandoc-presentation-compile ()
    "Compile a presentation to PDF and HTML with pandoc into ~/tmp.

If ARG, put into my annex instead.

Lightweight alternative to both pandoc-mode and ox-pandoc.el.

Generates calls to pandoc that look like this: TODO"
    (interactive)
    (when (and (string= default-directory (expand-file-name "~/doc/pres/"))
               (eq major-mode 'markdown-mode))
      (let* ((pdf-output-file (f-join "~/tmp"
                                      (f-filename (f-swap-ext (buffer-file-name) "pdf"))))
             (html-output-file (f-swap-ext pdf-output-file "html")))

        (call-process-shell-command
         "pandoc" nil "*pandoc pdf output*" nil
         "-s" "-t" "beamer" "-i"
         (shell-quote-argument (buffer-file-name))
         "-o" (shell-quote-argument pdf-output-file))

        (call-process-shell-command
         "pandoc" nil "*pandoc html output*" nil
         "-s" "-t" "slidy"
         "--self-contained" "-i"
         (shell-quote-argument (buffer-file-name))
         "-o" (shell-quote-argument html-output-file))

        )))
  ;; This binding replaces use of `markdown-export'.
  (bind-key "<f9>" 'spw--pandoc-compile markdown-mode-map))

;;; Deft

(use-package deft
  :if (spw--optional-pkg-available-p "deft")
  :commands deft
  :bind ("C-c f" . deft)

  :init
  (setq
   deft-extensions '("org" "mdwn")
   deft-text-mode 'org-mode
   deft-directory "~/doc/org/"
   deft-recursive t
   deft-use-filename-as-title nil
   deft-auto-save-interval 20.0
   deft-incremental-search t
   deft-org-mode-title-prefix t

   ;; snake_case for deft notes; CamelCase for files included in
   ;; main Org agenda
   deft-use-filter-string-for-filename t
   deft-file-naming-rules '((noslash . "_")
                            (nospace . "_")
                            (case-fn . downcase)))

  :config
  ;; restore my C-w binding
  (bind-key "C-w" 'deft-filter-decrement-word deft-mode-map))

;;; TRAMP

(use-package tramp
  :config
  ;; from http://carloerodriguez.com/blog/2015/12/14/effective-ssh-connections-with-emacs/
  (tramp-set-completion-function
   "ssh"
   '((tramp-parse-sconfig "/etc/ssh_config")
     (tramp-parse-sconfig "~/.ssh/config")))
  (setq tramp-default-method "ssh")

  ;; see docstring for `tramp-remote-path'
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; try to disable vc (from TRAMP FAQ)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; clean out remote paths from ~/.emacs.d/ido.last
  ;; from JoeBloggs on the Emacs wiki
  (defun spw--ido-remove-tramp-from-cache nil
    "Remove any TRAMP entries from `ido-dir-file-cache'.
    This stops tramp from trying to connect to remote hosts on emacs startup,
    which can be very annoying."
    (interactive)
    (setq ido-dir-file-cache
	  (cl-remove-if
	   (lambda (x)
	     (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
	   ido-dir-file-cache)))
  (advice-add 'ido-kill-emacs-hook :before #'spw--ido-remove-tramp-from-cache))

;;; ebib for editing BiBTeX databases

(use-package ebib
  :if (spw--optional-pkg-available-p "ebib")
  :bind ("C-c g e" . ebib)
  :init (setq ebib-preload-bib-files '("~/doc/spw.bib")))

;;; dired enhancements

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t)

;; should be able to unzip with Z
(with-eval-after-load "dired-aux"
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(use-package dired-sort-map
  :init (setq dired-listing-switches "--group-directories-first -alh"))

(use-package git-annex
  :if (spw--optional-pkg-available-p "git-annex"))

;;; close old buffers once per day

(use-package midnight
  :init (midnight-delay-set 'midnight-delay "3am"))

;;; simple concept of projects

(use-package projectile
  :if (spw--optional-pkg-available-p "projectile")
  :commands projectile-vc
  :diminish projectile-mode
  :bind (("C-c p" . projectile-command-map)
         ("C-c j" . projectile-find-file)
         ("C-c v" . projectile-vc))
  :demand
  :config
  ;; fix bad interaction between projectile and tramp
  (defun projectile-project-root--tramp-fix (orig-fun &rest args)
    (unless (file-remote-p default-directory)
      (apply orig-fun args)))
  (advice-add 'projectile-project-root :around #'projectile-project-root--tramp-fix)

  (projectile-global-mode 1)
  (setq projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'ido)
  (add-to-list 'projectile-globally-ignored-directories ".stack-work")
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories ".cabal-sandbox")
  (add-to-list 'projectile-globally-ignored-directories "docsets")

  ;; as part of daily cleanup, clean-up projects that no longer exist
  (add-hook 'midnight-hook 'projectile-cleanup-known-projects)

  ;; Find and open projects in ~/src/ that aren't yet known to
  ;; projectile.  Inspired by
  ;; <https://alanpearce.uk/post/opening-projects-with-projectile>.
  ;; (also see projectile() in .zshrc
  (setq programming-projects-dir (expand-file-name "~/src"))
  (defun spw--get-programming-projects (dir)
    "Find all projectile projects in DIR that are presently unknown to projectile."
    (-filter (lambda (d)
               (and (file-directory-p d)
                    (not (-contains?
                          projectile-known-projects
                          (f-slash (replace-regexp-in-string (expand-file-name "~") "~" d))))
                    (-any? (lambda (f) (funcall f d))
                           projectile-project-root-files-functions)))
             (directory-files dir t "^[^.]")))
  (defun spw--open-programming-project (arg)
    "Open a programming project that is presently unknown to projectile.

Passes ARG to `projectile-switch-project-by-name'."
    (interactive "P")
    (let ((project-dir
           (projectile-completing-read "open new project: "
                                       (spw--get-programming-projects programming-projects-dir))))
      (projectile-switch-project-by-name project-dir arg)))
  (bind-key "n" 'spw--open-programming-project projectile-command-map))

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

      ;; Don't invoke TRAMP to complete
      ido-enable-tramp-completion nil

      ;; Have Ido respect completion-ignored-extensions
      ido-ignore-extensions t

      ;; When moving through work directories with M-n/M-p, ignore those
      ;; that don't match the current input
      ido-work-directory-match-only t)

(ido-mode 1)
(ido-everywhere 1)

(use-package flx-ido
  :if (spw--optional-pkg-available-p "flx")
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        flx-ido-threshhold 7500
        gc-cons-threshold 20000000))

(use-package ido-ubiquitous
  :if (spw--optional-pkg-available-p "ido-ubiquitous")
  :config
  ;; enable
  (ido-ubiquitous-mode 1)

  ;; disable during Org capture
  (add-to-list 'ido-ubiquitous-command-overrides
               '(disable prefix "org-capture")))

(use-package smex
  :bind ("C-x C-m" . smex))

;;; snippets

(use-package yasnippet
  :if (spw--optional-pkg-available-p "yasnippet")
  :diminish yas-minor-mode
  :defer 5
  :config
  (yas-global-mode 1)

  ;; kill warnings about snippets that use backquoted lisp to change
  ;; the buffer
  (unless (boundp 'warning-suppress-types)
    (setq warning-suppress-types nil))
  (push '(yasnippet backquote-change) warning-suppress-types))

;;; htmlize for Org HTML export/publishing

(use-package htmlize
  :if (spw--optional-pkg-available-p "htmlize"))

;;; make indentation in python nice and visible

(use-package highlight-indentation
  :if (spw--optional-pkg-available-p "highlight-indentation")
  :init
  (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode))

;;; buffer navigation

;; TODO consider these avy-keys from Endless Parentheses blog
;; (setq avy-keys
;;       '(?c ?a ?s ?d ?e ?f ?h ?w ?y ?j ?k ?l ?n ?m ?v ?r ?u ?p))

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
      (if (bound-and-true-p subword-mode)
          (avy-goto-subword-1 char nil)
        (avy-goto-word-1 char nil))))

  ;; TODO: can this be buffer local?  Then I could use it only for
  ;; variable-pitch-mode.  Disabled because outside of that mode it's
  ;; annoying
  ;; (face-override-variable-pitch 'avy-lead-face-0)
  ;; (face-override-variable-pitch 'avy-lead-face-1)
  ;; (face-override-variable-pitch 'avy-lead-face-2)
  ;; (face-override-variable-pitch 'avy-lead-face)
  )

;; use avy to move between links in *Help* buffers
(use-package ace-link
  :defer 5
  :config
  (ace-link-setup-default))

;;; make dired copy and move asynchronously

(use-package async
  :if (spw--optional-pkg-available-p "async")
  :init (when (require 'dired-aux)
          (require 'dired-async)))

;;; give dired some nice keybindings for browsing images

(use-package image-dired
  :init (image-dired-setup-dired-keybindings))

;;; zap-up-to-char is at least as useful as zap-to-char, so load it
;;; out of misc.el.

(use-package misc
  :commands zap-up-to-char)

;;; make Emacs regexps easier

(use-package visual-regexp
  :if (spw--optional-pkg-available-p "visual-regexp"))

;;; undo/redo window layout changes

(winner-mode 1)

;;; Load up Haskell mode settings if Debian haskell-mode package
;;; installed (and load here, as dependencies of these settings are
;;; earlier in this init file)

(use-package haskell-mode
  :if (spw--optional-pkg-available-p "haskell-mode")
  :mode (("\\.hs" . haskell-mode)
         ("\\.lhs" . literate-haskell-mode)
         ("\\.cabal" . haskell-cabal-mode))
  :config
  ;; TODO polish, and package for Debian
  (use-package haskell-tab-indent
    :load-path "~/.emacs.d/site-lisp")

  (setq
   ;; indentation preferences
   haskell-indentation-layout-offset 4
   haskell-indentation-left-offset 4
   haskell-indentation-show-indentations nil

   ;; we rely on `haskell-mode-goto-loc' for our M-. binding, but still
   ;; generate a TAGS file for completion
   haskell-tags-on-save t

   ;; this tends to get in the way
   haskell-mode-contextual-import-completion nil

   ;; enable standard features from haskell-mode docs
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-log t

   ;; guess whether this is a stack or pure cabal project
   haskell-process-type 'auto)

  ;; Use a local hook to turn on an appropriate indentation mode.  Use
  ;; `haskell-indentation-mode' by default, but if our .dir-locals.el
  ;; specifies `indent-tabs-mode', we should instead use my
  ;; `haskell-tab-indent-mode'
  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-hook 'hack-local-variables-hook
                        (lambda ()
                          (if indent-tabs-mode
                              (haskell-tab-indent-mode 1)
                            (haskell-indentation-mode 1)))
                        nil t)))

  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  (require 'haskell-interactive-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)

  (require 'haskell)
  (diminish 'interactive-haskell-mode)
  (require 'subword)
  (diminish 'subword-mode)

  ;; standard Haskell repl interaction bindings
  (bind-key "C-c C-l" 'haskell-process-load-or-reload  haskell-mode-map)
  (bind-key "C-c C-b" 'haskell-interactive-bring       haskell-mode-map)
  (bind-key "C-c C-i" 'haskell-process-do-info         haskell-mode-map)
  (bind-key "C-c C-c" 'haskell-process-cabal-build     haskell-mode-map)
  (bind-key "C-c C-k" 'haskell-interactive-mode-clear  haskell-mode-map)
  (bind-key "C-c C"   'haskell-process-cabal           haskell-mode-map)

  ;; same again for `haskell-cabal-mode'
  (bind-key "C-c C-b" 'haskell-interactive-bring       haskell-cabal-mode-map)
  (bind-key "C-c C-k" 'haskell-interactive-mode-clear  haskell-cabal-mode-map)
  (bind-key "C-c C-c" 'haskell-process-cabal-build     haskell-cabal-mode-map)
  (bind-key "C-c C"   'haskell-process-cabal           haskell-cabal-mode-map)

;;; these two bindings require GHCi 8 or newer (or GHCi-ng)

  ;; jump asynchronously; no need for a TAGS file
  (bind-key "M-."     'haskell-mode-goto-loc           interactive-haskell-mode-map)

  ;; pass C-u to insert a missing type signature
  (bind-key "C-c C-t" 'haskell-mode-show-type-at       interactive-haskell-mode-map)

  ;; ensure that company falls back to dabbrevs when haskell-mode cannot
  ;; complete, such as in where clauses (this is straight from
  ;; haskell-mode docs)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

;; `key-chord' to save my hands

(use-package key-chord
  :config
  (key-chord-mode 1)
  ;; access the C-c keymap with a comfortable key-chord
  ;; TODO access buffer-local C-c map
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
  :if (spw--optional-pkg-available-p "elisp-slime-nav")
  :commands (turn-on-elisp-slime-nav-mode elisp-slime-nav-mode)
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook 'elisp-slime-nav-mode)))

(use-package world-time-mode
  :if (spw--optional-pkg-available-p "world-time-mode")
  :commands world-time-list
  :bind ("C-c g t" . world-time-list))

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

(use-package debpaste
  :if (spw--optional-pkg-available-p "debpaste")
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

(use-package ws-butler
  :if (spw--optional-pkg-available-p "ws-butler")
  :demand
  :diminish ws-butler-mode
  :init
  ;; message-mode is sensitive to trailing whitespace in sig dashes
  ;; and empty headers.  markdown-mode is sensitive in empty headers
  ;; (e.g. "# " which I use in writing essays) and newlines that
  ;; indicate paragraph flow (obscure Markdown feature)
  (setq ws-butler-global-exempt-modes
        '(markdown-mode message-mode))
  :config (ws-butler-global-mode))

(use-package cycle-quotes
  :if (spw--optional-pkg-available-p "cycle-quotes")
  :bind ("C-c '" . cycle-quotes))

(use-package redtick
  :if (spw--optional-pkg-available-p "redtick")
  :bind ("C-c P" . redtick-mode)
  :init
  (setq redtick-history-file nil)
  :config
  ;; this is needed with no history file
  (remove-hook 'redtick-after-rest-hook #'redtick--save-history))



;;;; ---- functions and bindings ----

;;; for dealing with blocks of lines (inspired by vim, which does a
;;; better job of handling these than Emacs)

(defun spw--mark-whole-lines ()
  (interactive)
  (if (use-region-p)
      (progn
        (when (> (point) (mark))
          (exchange-point-and-mark))
        (beginning-of-line)
        (let ((end (save-excursion
                     (goto-char (mark))
                     (beginning-of-line 2)
                     (point))))
          (push-mark end nil t)))
    (beginning-of-line 2)
    (let ((end (point)))
      (beginning-of-line 0)
      (push-mark end nil t))))
(bind-key "M-i" 'spw--mark-whole-lines)

(defun mwf--narrow-or-widen-dwim (p)
  "Unless P, widen if buffer is narrowed.  Otherwise, narrow intelligently.

Intelligently means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to org-src-block
actually calls `org-edit-src-code'."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; note that `org-edit-src-code' is not a real narrowing command
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))
(bind-key "C-c n" 'mwf--narrow-or-widen-dwim)

;;; backwards and forward deletions of words.  We want to delete, not kill

(defun spw--delete-word (arg)
  "Delete ARG characters forward until encountering the end of a word."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
;;; don't use `bind-key' as we want this to be overridable
;;; TODO do we really want M-d to delete, or should it kill?
(global-set-key "\M-d" 'spw--delete-word)

(defun spw--backward-delete-word (arg)
  "Delete characters ARG backward until encountering the end of a word."
  (interactive "p")
  (spw--delete-word (- arg)))
(bind-key "C-w" 'spw--backward-delete-word)

;; a nicer kill-region binding, and move the keyboard macro bindings
;; somewhere else
(bind-key "C-x C-k" 'kill-region)
;; resettle the previous occupant of C-x C-k
(bind-key "C-c C-x C-k" 'kmacro-keymap)

;;; my buffer save cleanup functions

(defun spw--compact-blank-lines ()
  "Replace multiple empty blank lines in the buffer with single blank lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\n\n\n+" nil "noerror")
      (replace-match "\n\n"))))

(defun spw--clean-lisp-dangling-brackets ()
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

(defun spw--cleanup ()
  "Clean up buffer, or region if mark is active, depending on major mode.

Note that `ws-butler-mode' is also at work."
  (save-restriction
    (when (use-region-p)
      (narrow-to-region))
    (case major-mode
      (haskell-mode
       (spw--compact-blank-lines)
       (haskell-mode-stylish-buffer))
      (python-mode
       (spw--compact-blank-lines))
      (emacs-lisp-mode
       (spw--compact-blank-lines)
       (spw--clean-lisp-dangling-brackets))
      (cc-mode
       (indent-region (point-min) (point-max))
       (whitespace-cleanup))
      (message-mode
       (save-excursion
         (message-goto-body)
         (save-restriction
           (narrow-to-region (point) (point-max))
           ;; (fill-region (point-min) (point-max))
           (whitespace-cleanup)))))))
(bind-key "C-c g c" 'spw--cleanup)

(defun spw--toggle-window-split ()
  "Toggle the orientation of a two-window split.

Author unknown."
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
(bind-key "C-c s" 'spw--toggle-window-split)

(defun magnars--move-beginning-of-line-dwim (arg)
  "Move point back to indentation or beginning of line.

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
(bind-key "C-a" 'magnars--move-beginning-of-line-dwim)

;;; tidy up troublesome unicode

(defun gleitzman--unicode-hunt ()
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
(bind-key "C-c g u" 'gleitzman--unicode-hunt)

(defun spw--kill-ring-save (arg)
  "As `kill-ring-save', but save to end of ARG lines if mark inactive."
  (interactive "P")
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (kill-ring-save (point)
                    (save-excursion
                      (if arg
                          (forward-visible-line (prefix-numeric-value arg))
                        (end-of-visible-line))
                      (point)))))
;; with the region inactive, we have M-i M-w to copy the whole line
;; including its line break, and M-w to copy to the end of the line
;; without moving point
(bind-key "M-w" 'spw--kill-ring-save)

(defun spw--open-term-here ()
  "Open a fresh urxvt terminal in current directory."
  (interactive)
  (call-process "urxvtcd" nil "*errors*" nil
                "-cd" (expand-file-name  default-directory)
                "-e"  "/bin/zsh"))
(bind-key "C-c g g" 'spw--open-term-here)

;;; Make `C-x z' repeat zap-up-to-char without requiring typing the
;;; char again.  Originally from Chris Done's Emacs config, but
;;; modified to always be case-sensitive (previously case-sensitivity
;;; depended on the value of `case-fold-search')

(defvar zap-up-to-char-last-char nil
  "The last char used with zap-up-to-char-repeateable.")
(defvar zap-up-to-char-last-arg 0
  "The last direction used with zap-up-to-char-repeateable.")
(defun zap-up-to-char-repeatable (arg char)
  "As `zap-up-to-char', but repeatable with `repeat'.

Goes backward if ARG is negative; error if CHAR not found."
  (interactive (if (and (eq last-command 'zap-up-to-char-repeatable)
                        (eq 'repeat real-this-command))
                   (list zap-up-to-char-last-arg
                         zap-up-to-char-last-char)
                 (list (prefix-numeric-value current-prefix-arg)
                       (read-char "Zap up to char: " t))))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (let ((case-fold-search nil))
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
        (kill-region start end)))))
  (setq zap-up-to-char-last-char char)
  (setq zap-up-to-char-last-arg arg)
  (setq this-command 'zap-up-to-char-repeatable))
;; note that our C-a binding renders default M-m binding redundant
(bind-key "M-m" 'zap-up-to-char-repeatable)

;; used in init-org.el
(defun spw--strip-text-properties (txt)
  "From http://stackoverflow.com/questions/8372722/print-only-text-discarding-text-properties"
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun spw--dotfiles-rebase ()
  "Rebase & push dotfiles."
  (interactive)
  (let ((default-directory (expand-file-name "~/src/dotfiles/"))
        (buffer (get-buffer-create "*dotfiles rebase*")))
    (display-buffer "*dotfiles rebase*")
    (async-shell-command "git-dotfiles-rebase" "*dotfiles rebase*")))
(bind-key "C-c g d" 'spw--dotfiles-rebase)

;;; defeat variable-pitch-mode for avy and Org tables and source
;;; blocks, per http://stackoverflow.com/a/16819449

;; (defun my-adjoin-to-list-or-symbol (element list-or-symbol)
;;   (let ((list (if (not (listp list-or-symbol))
;;                   (list list-or-symbol)
;;                 list-or-symbol)))
;;     (require 'cl-lib)
;;     (cl-adjoin element list)))

;; (defun face-override-variable-pitch (face)
;;   (set-face-attribute
;;    face nil
;;    :inherit
;;    (my-adjoin-to-list-or-symbol
;;     'fixed-pitch
;;     (face-attribute face :inherit))))



;;;; ---- personal settings ----

;;; no tabs by default

(setq-default indent-tabs-mode nil)

;;; key bindings

;; I don't often want to quit
(bind-key "C-x C-c" 'delete-frame)
(bind-key "C-c g k" 'kill-emacs)

;; `reindent-then-newline-and-indent' tends to get things wrong more
;; often than it gets things right with my typing habits.  I hit <TAB>
;; a lot.
(bind-key "RET" 'newline-and-indent)

;; fixup-whitespace seems to make just-one-space redundant
(bind-key "M-SPC" 'fixup-whitespace)

;; never want to send any e-mail
(unbind-key "C-x m")
(unbind-key "C-x 4 m")

;; fallback expanding
(bind-key "M-/" 'hippie-expand)

(bind-key "C-c ." 'repeat)

;;; mode-specific

(bind-key "C-c u w" 'wdired-change-to-wdired-mode dired-mode-map)

;;; toggling

(bind-key "C-c t e" 'toggle-debug-on-error)
(bind-key "C-c t i" 'org-indent-mode)
(bind-key "C-c t w" 'spw/writing-toggle)

;;; evaluation

(bind-key "C-c e e" 'spw/eval-surrounding-sexp)
(bind-key "C-c e f" 'eval-defun)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e b" 'eval-buffer)

;;; insertion

(bind-key "C-c i h" 'add-file-local-variable-prop-line)

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

;; ;; isearch should leave you at the beginning of the match
;; (add-hook 'isearch-mode-end-hook 'spw/isearch-match-beginning)
;; (defun spw/isearch-match-beginning ()
;;   "Move point to beginning of isearch match."
;;   (when isearch-other-end
;;     (when isearch-forward (goto-char isearch-other-end))))

;; save script files as executable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; always update file contents
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

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

;; ;; disable for python mode where it makes a mess
;; (defun electric-indent-ignore-python ()
;;   "Ignore electric indentation for `python-mode'."
;;   (if (equal major-mode 'python-mode)
;;       `no-indent'
;;     nil))
;; ;;(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; browser
(setq browse-url-generic-program "firefox"
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

(defun spw--save-org-buffers-first (&rest ignore)
  "Save all Org buffers without prompting."
  (when (featurep 'org)
    ;; gotta remove this advice first since `org-save-all-org-buffers'
    ;; calls `save-some-buffers'
    (advice-remove 'save-some-buffers #'spw/save-org-buffers-first)
    (org-save-all-org-buffers)
    (advice-add 'save-some-buffers :before #'spw/save-org-buffers-first)))
(advice-add 'save-some-buffers :before #'spw--save-org-buffers-first)

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
  (defun spw--set-catmail-from ()
    "Set e-mail From: address to CatMail, by looking at other headers."
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

  ;; show trailing whitespace in message-mode (due to empty headers
  ;; and signature dashes, ws-butler disabled)
  (add-hook 'message-mode-hook (lambda ()
                                 (setq-local show-trailing-whitespace t)))

  ;; used in message-mode yasnippets
  (defun spw/recipient-first-name ()
    (save-excursion
      (message-goto-to)
      (message-beginning-of-line)
      ;; handle Microsoft Exchange
      (when (looking-at "\"")
        (forward-word 1)
        (forward-char 2))
      (let* ((beg (point))
             (end (progn (forward-word 1) (point)))
             (name (filter-buffer-substring beg end)))
        (cond
         ;; exceptions for people who have longer forms of their names
         ;; in their From: headers
         ((string= name "Nathaniel") "Nathan")
         ((string= name "Thomas") "Tom")
         ;; default
         (t name)))))

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

  (add-hook 'message-mode-hook
            (lambda ()
              (auto-fill-mode)
              ;; (spw/set-from-address)
              (footnote-mode)
              ;; annoying for WNPP; I want the bug number
              ;; (spw/debbugs-no-ack)
              (message-goto-body)))

  ;; ensure encrypted messages are also encrypted to me, so I can read
  ;; them in my sent mail folder
  (setq mml-secure-openpgp-encrypt-to-self t)

  ;; C-c C-b should skip over mml's sign/encrypt lines (it is a bad
  ;; idea to advise message-goto-body as various functions assume it
  ;; does not skip over sign/encrypt lines
  ;; (e.g. `notmuch-mua-check-no-misplaced-secure-tag')
  (defun spw--message-goto-body--skip-mml-secure ()
    (interactive)
    (message-goto-body)
    (when (looking-at "^<#\\(secure\\|part\\)")
      (forward-line)))
  (bind-key "C-c C-b" 'spw--message-goto-body--skip-mml-secure message-mode-map)

  ;; default C-c C-s binding is not much use, and I keep hitting it
  ;; accidently
  (bind-key "C-c C-s" 'message-goto-subject message-mode-map))

;;; C-c C-c to save-and-exit emacsclient (like <esc>ZZ in vim)

;; hopefully won't interact badly with major mode C-c C-c binding;
;; might need to change the map this gets bound into

(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-c C-c")
                             (lambda ()
                               (interactive)
                               (save-buffer)
                               (server-edit))))))

;;; IELM

;; (setq ielm-dynamic-return nil)

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
        ("^html?$" "." "firefox %o"))))

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

;;; Perl

;; using cperl-mode instead of perl-mode because the former doesn't
;; try to indent lines within a POD

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(setq cperl-indent-level 4)
(setq cperl-indent-wrt-brace t)
(setq cperl-lineup-step 1)
;; (setq cperl-continued-statement-offset 4)

;;; changelogs

(defun spw/change-log-setup ()
  (setq-local indent-tabs-mode nil)
  (setq-local left-margin 2)
  (orgstruct++-mode 1))
(add-hook 'change-log-mode-hook 'spw/change-log-setup)

(setq debian-changelog-mailing-address "spwhitton@spwhitton.name")

;;; cc-mode

;; the built-in 'linux' style doesn't use tabs, but the kernel style
;; guide mandates them, so make a slightly modified style
(c-add-style "linux-tabs" '("linux" (indent-tabs-mode . t)))
(setq c-default-style "linux-tabs")
