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

;; libs in ~/.emacs.d/initlibs are overridden by system packages
;; This is for fallback copies of libraries needed to init Emacs.
(add-to-list 'load-path (concat user-emacs-directory "initlibs") t)

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
;; should be present in ~/.emacs.d/initlibs.

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

(package-initialize)



;;;; ---- basic settings ----

;;; customisation -- must be loaded early so that zenburn theme is
;;; considered safe

(defconst custom-file (concat user-emacs-directory "init-custom.el"))
(load custom-file 'noerror)

;; when I do elisp experimentation I use IELM, so we can make
;; *scratch* easier to use
(setq initial-major-mode 'text-mode)

;;; other startup settings

(setq ;; inhibit-startup-echo-area-message ""
 ;; inhibit-startup-screen t
 ;; initial-buffer-choice t
 initial-scratch-message nil)

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
(setq
 ;; this is more trouble than it is worth because when the window
 ;; layout changes the existing position of the mouse can cause a
 ;; surprise focus change
 mouse-autoselect-window nil

 ;; tell Emacs what my window manager does
 focus-follows-mouse t)

;; note that this works only for self-insert chars, not other
;; bindings, and it comes back after switching away from Emacs.  But
;; mouse-avoidance-mode, which does more than this, is more annoying
;; than helpful for other keypresses
(setq make-pointer-invisible t)

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
;; if want slightly more compact, reduce to Terminus-11
;; TODO conditional causes `emacs --daemon` to crash on stretch
;; (when (find-font (font-spec :name "Terminus-12"))
;;   (add-to-list 'default-frame-alist '(font . "Terminus-12")))
(add-to-list 'default-frame-alist '(font . "Terminus-12"))

;; disable some GUI elements (retain menu bar)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; cursor settings

(setq x-stretch-cursor t)
(setq-default cursor-type 'box)

;; turn off blink-cursor-mode if it ended up on
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))

;;; zenburn

(use-package zenburn-theme
  :if (spw--optional-pkg-available-p "zenburn-theme")
  :init
  (load-theme 'zenburn)
  ;; see description of `frame-background-mode' variable
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list)))

;;; On remote hosts in the UTC timezone, assume I'm in Arizona.  This
;;; is relevant for using Org-mode.  (Hosts in the UK will be in GMT,
;;; not UTC.)

(when (and (not (eq system-type 'windows-nt))
           (string= (car (cdr (current-time-zone))) "UTC"))
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

;;; high garbage collection threshold for ido flx matching

(setq gc-cons-threshold 20000000)



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
                              inferior-scheme-mode-hook
                              clojure-mode-hook))

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

;; docstring says this has to be set before org.el is loaded
(setq org-enforce-todo-checkbox-dependencies t)

(use-package org
  ;; init-org.el uses `f-glob'
  :if (spw--optional-pkg-available-p "f")
  :mode (("\\.org" . org-mode))
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o [" . spw--org-agenda-file-to-front)
         ("C-c o ]" . spw--org-remove-file))
  :commands (org-save-all-org-buffers   ; for ~/bin/save-org-buffers
             orgstruct++-mode)
  :diminish org-indent-mode
  :init
  ;; define this early so that `spw--search-notes' and `spw--new-note'
  ;; can make use of it
  (setq org-directory "~/doc/org")
  :config (load (concat user-emacs-directory "init-org.el")))

;;; more useful unique buffer names

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward))

;;; OpenWith

(use-package openwith
  :if (spw--optional-pkg-available-p "openwith")
  :commands openwith-mode
  :demand
  :config
  (setq
   openwith-associations
   '(("\\.pdf\\'" "evince"
      (file))
     ("\\.\\(ogg\\|mp3\\|flac\\|mkv\\|webm\\|avi\\|mp4\\|wmv\\|flv\\)\\'" "vlc"
      (file))
     ("\\.\\(doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx\\|potx\\)\\'" "soffice"
      (file))
     ("\\.hwp\\'" "hanword"
      (file))
     ("\\.\\(jpg\\|JPG\\|jpeg\\|png\\|gif\\)" "eog"
      (file))))
  (openwith-mode 1))

;; thanks to openwith, the warning for large files can be at a much
;; larger threshold as the chances of hitting it are low (this is
;; about 500MB)

(setq large-file-warning-threshold 500000000)

;;; magit

(use-package magit
  :if (spw--optional-pkg-available-p "magit")
  :demand
  :config
  (setq
   ;; by default, don't pass -f to `git remote add`
   magit-remote-arguments nil

   magit-completing-read-function 'magit-ido-completing-read
   magit-push-always-verify nil)

  (use-package magit-annex
    :if (spw--optional-pkg-available-p "magit-annex")))

;;; colour those parentheses

(use-package rainbow-delimiters
  :if (spw--optional-pkg-available-p "rainbow-delimiters")
  :commands rainbow-delimiters-mode
  :init
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

;;; fill comments

(defun spw--turn-on-comment-filling ()
  "Turn on filling comments."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(spw--activate-in-lisp-modes 'spw--turn-on-comment-filling)

;;; company-mode for smart and easy completion

(use-package company
  :if (spw--optional-pkg-available-p "company")
  :diminish company-mode
  :config
  ;; disable activating company for the moment.  Keep company so
  ;; notmuch can activate it
  ;;   (defun spw--activate-company ()
  ;;     "Setup company mode.

  ;; Using this in preference to global-company-mode, with <tab> bound
  ;; to `company-complete'.  For another approach, see
  ;; https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387"
  ;;     (company-mode 1)
  ;;     (define-key (current-local-map) (kbd "M-/") 'company-complete))
  ;;   (add-hook 'prog-mode-hook 'spw--activate-company)

  ;; retain my C-w binding; move company's C-w binding
  (define-key company-active-map "\C-w" nil)
  (bind-key "M-o" 'company-show-location company-active-map)

  (setq company-idle-delay nil
        company-minimum-prefix-length 0
        company-echo-delay 0)

  ;; do I want these?
  ;; (add-to-list 'company-backends 'company-capf)
  ;; (add-to-list 'company-transformers 'company-sort-by-occurrence)
  )
;; usage notes:
;;
;; C-o during company isearch narrows to stuff matching that search;
;; mnemonic 'occur'.  C-M-s while outside of search to do the same
;; thing

;;; Markdown mode

(use-package markdown-mode
  :if (spw--optional-pkg-available-p "markdown-mode")
  :mode "\\.md"
  :config
  ;; this is called by .dir-locals.el in ~/doc/{pres,papers} and
  ;; relies on the Makefiles in those dirs
  (defun spw--set-pandoc-compile-command (&rest exts)
    (unless exts (setq exts (list "pdf")))
    (setq-local compile-command
                (concat "make "
                        (mapconcat
                         (lambda (ext)
                           (f-filename (f-swap-ext (buffer-file-name) ext)))
                         exts " ")))
    (local-set-key (kbd "<f9>") 'recompile)))

;;; ebib for editing BiBTeX databases

(use-package ebib
  :if (spw--optional-pkg-available-p "ebib")
  :bind ("C-c g e" . ebib)
  :init (setq ebib-preload-bib-files '("~/doc/spw.bib")
              ebib-index-display-fields '(title)
              ebib-save-xrefs-first t)
  :config (delete "translator" ebib-hidden-fields))

;;; dired enhancements

(use-package dired
  :defer
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t
        dired-listing-switches "--group-directories-first -alh")
  :config
  (use-package dired-aux
    :config
    ;; should be able to unzip with Z
    (add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))
  (use-package dired-x
    :config
    (setq-default dired-omit-mode t)
    (setq dired-omit-files "^\\...+$")
    (setq dired-isearch-filenames t)))

(use-package git-annex
  :if (spw--optional-pkg-available-p "git-annex"))

;;; close old buffers once per day

(use-package midnight
  :init (midnight-delay-set 'midnight-delay "3am"))

;;; simple concept of projects

(use-package projectile
  :if (and
       (spw--optional-pkg-available-p "f")
       (spw--optional-pkg-available-p "dash")
       (spw--optional-pkg-available-p "projectile"))
  :diminish projectile-mode
  :bind (("C-c p" . projectile-command-map)
         ("C-c j" . projectile-find-file)
         ("C-c v" . projectile-vc))
  ;; we have to demand because projectile-command-map is not a
  ;; command, so is not autoloaded by the :bind above.  We could drop
  ;; the :demand and use :bind-keymap but together with my keychord
  ;; that seems to mean trouble
  :demand
  :config
  ;; rebind to take advantage of helm-projectile library here
  (bind-key "s s" 'helm-projectile-ag projectile-command-map)

  ;; `spw--get-programming-projects' needs f & dash
  (use-package f)
  (use-package dash)

  ;; ;; fix bad interaction between projectile and tramp
  ;; (defun projectile-project-root--tramp-fix (orig-fun &rest args)
  ;;   (unless (file-remote-p default-directory)
  ;;     (apply orig-fun args)))
  ;; (advice-add 'projectile-project-root :around #'projectile-project-root--tramp-fix)

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
  ;; (also see projectile() in ~/src/dotfiles/archive/.zshrc)
  (defconst programming-projects-dir (expand-file-name "~/src"))
  (defun spw--get-programming-projects (dir)
    "Find all projectile projects in DIR that are presently unknown to projectile."
    (-filter
     (lambda (d)
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

;;; `helm-projectile-ag' for searching through Org notes (replaces
;;; Deft which has become too slow)

(use-package helm-projectile
  :if (and
       (spw--optional-pkg-available-p "helm-ag")
       (spw--optional-pkg-available-p "helm-projectile")
       (boundp 'org-directory))
  :bind (("C-c f" . spw--search-notes)
         ("C-c F" . spw--new-note))
  :commands helm-projectile-ag
  :config
  (use-package helm-ag)
  (defun spw--search-notes ()
    "Invoke ag to incrementally search through my Org notes."
    (interactive)
    (let ((projectile-project-root org-directory))
      (call-interactively 'helm-projectile-ag)))
  (defun spw--new-note (name)
    "Create a new Org note entitled NAME."
    (interactive "sTitle: ")
    (let* ((sanitised1
            (replace-regexp-in-string "\?" "" name))
           (sanitised2
            (replace-regexp-in-string ": " "," sanitised1))
           (sanitised3
            (replace-regexp-in-string ":" "," sanitised2))
           (sanitised
            (replace-regexp-in-string " " "_" sanitised3)))
      (find-file (expand-file-name
                  (concat sanitised ".org")
                  org-directory))
      (insert (concat "#+TITLE: " name))
      (insert "\n"))))

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
(defun spw--ido-ignore-file-at-point ()
  "Disable ido-use-filename-at-point for the current buffer."
  (when (bound-and-true-p ido-use-filename-at-point)
    (setq-local ido-use-filename-at-point nil)))
(add-hook 'dired-mode-hook #'spw--ido-ignore-file-at-point)

(setq ido-use-filename-at-point 'guess
      ido-create-new-buffer 'never
      ;; ido-file-extensions-order '(".org" ".mdwn" ".hs" ".tex" ".py" )
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-max-directory-size 100000
      ido-auto-merge-delay-time 99999 ; only search when I tell you to, M-s
      ido-use-virtual-buffers t
      ido-use-virtual-buffers-automatically t
      ido-enable-regexp nil
      ido-use-url-at-point nil
      ;; ido-max-file-prompt-width 0.1
      ido-save-directory-list-file (expand-file-name "~/.emacs.d/ido.last")

      ;; Don't invoke TRAMP to complete
      ido-enable-tramp-completion nil

      ;; Have Ido respect completion-ignored-extensions
      ido-ignore-extensions t

      ;; When moving through work directories with M-n/M-p, ignore those
      ;; that don't match the current input
      ;; ido-work-directory-match-only t

      ;; ido-enable-tramp-completion t
      ido-confirm-unique-completion nil
      ido-show-dot-for-dired nil

      ido-enable-flex-matching t)

(ido-mode 1)
(ido-everywhere 1)

(use-package flx-ido
  :if (spw--optional-pkg-available-p "flx")
  :config
  (flx-ido-mode 1)
  (setq
   ;; disable ido faces to see flx highlights
   ido-use-faces nil
   flx-ido-threshold 7500))

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

;;; buffer navigation

(use-package avy
  :bind (("M-o" . spw/avy-goto-word)
         ;; if one types M-g g followed by numbers, avy-goto-line will
         ;; switch to Emacs default M-g g behaviour.  So this
         ;; rebinding of a standard key is purely additive
         ("M-g g" . avy-goto-line))
  :config

  (setq
   ;; Make avy overlays look more like ace-jump
   avy-style       'at-full
   ;; Make avy work over all visible frames
   avy-all-windows 'all-frames
   ;; Increase the keys available such that less likely to need to
   ;; type two (list from Endless Parentheses blog)
   avy-keys '(?c ?a ?s ?d ?e ?f ?h ?w ?y ?j ?k ?l ?n ?m ?v ?r ?u ?p))

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

(use-package dired-async
  :if (spw--optional-pkg-available-p "async")
  :commands dired-async-mode
  :defer 5
  :config
  (dired-async-mode 1))

;;; smart C-a binding means we can bind M-m to something more useful

(use-package misc
  :bind ("M-m" . zap-up-to-char))

;;; make Emacs regexps easier

(use-package visual-regexp
  :if (spw--optional-pkg-available-p "visual-regexp"))

;;; undo/redo window layout changes

(winner-mode 1)

;;; subword-mode

(use-package subword
  :diminish subword-mode)

;;; Load up Haskell mode settings if Debian haskell-mode package
;;; installed (and load here, as some dependencies of these settings
;;; earlier in this init file)

(use-package haskell
  :if (spw--optional-pkg-available-p "haskell-mode")
  :mode (("\\.hs" . haskell-mode)
         ("\\.lhs" . literate-haskell-mode)
         ("\\.cabal" . haskell-cabal-mode))
  :diminish interactive-haskell-mode
  :config
  ;; TODO polish, and package for Debian
  (use-package haskell-tab-indent
    :load-path "~/.emacs.d/site-lisp")

  (setq
   ;; indentation preferences
   haskell-indentation-layout-offset 4
   haskell-indentation-left-offset 4
   haskell-indentation-show-indentations nil

   ;; haskell-mode is forever hanging, so enable some logging
   haskell-process-log t

   ;; tidy up the REPL buffer
   haskell-process-show-debug-tips nil

   ;; this tends to get in the way
   haskell-mode-contextual-import-completion nil

   ;; enable standard features from haskell-mode docs
   ;; TODO temporarily commented out to try to figure out why
   ;; haskell-mode hangs so often
   ;; haskell-process-suggest-remove-import-lines t
   ;; haskell-process-auto-import-loaded-modules t
   ;; haskell-process-log t

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
  (use-package haskell-interactive-mode
    :commands interactive-haskell-mode
    :init
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
  (add-hook 'haskell-mode-hook 'subword-mode)

  ;; standard Haskell repl interaction bindings

  ;; first use `C-c C-l' and/oror `C-c C-b' to enable the use of
  ;; bindings like `M-.' and `C-c C-c'.  Note that this is quite slow
  ;; so use sparingly; basically when getting an editing session
  ;; going.  In general `C-c C-b' will be enough to make `C-c C-c'
  ;; work properly and is a bit faster; `C-c C-l' is needed to make
  ;; `M-.' work
  (bind-key "C-c C-l" 'haskell-process-load-or-reload  haskell-mode-map)
  (bind-key "C-c C-b" 'haskell-interactive-bring       haskell-mode-map)

  ;; then, use `C-c C-c' to see whether your code compiles/run a type
  ;; check.  Navigate between errors using `M-g n' and `M-g p'.  This
  ;; is much more performant.  (`haskell-compile' is an alternative
  ;; but it does not autodetect whether the project is cabal or stack)
  (bind-key "C-c C-c" 'haskell-process-cabal-build     haskell-mode-map)

  ;; less useful keys
  ;; (bind-key "C-c C-i" 'haskell-process-do-info         haskell-mode-map)
  ;; (bind-key "C-c C-k" 'haskell-interactive-mode-clear  haskell-mode-map)
  ;; (bind-key "C-c C"   'haskell-process-cabal           haskell-mode-map)

  ;; those same bindings again for `haskell-cabal-mode'

  (bind-key "C-c C-b" 'haskell-interactive-bring       haskell-cabal-mode-map)
  ;; (bind-key "C-c C-k" 'haskell-interactive-mode-clear  haskell-cabal-mode-map)
  (bind-key "C-c C-c" 'haskell-process-cabal-build     haskell-cabal-mode-map)
  ;; (bind-key "C-c C"   'haskell-process-cabal           haskell-cabal-mode-map)

  ;; these two bindings require GHCi 8 or newer (or GHCi-ng)

  ;; jump asynchronously; no need for a TAGS file
  (bind-key "M-."     'haskell-mode-goto-loc           interactive-haskell-mode-map)

  ;; pass C-u to insert a missing type signature
  ;; (bind-key "C-c C-t" 'haskell-mode-show-type-at       interactive-haskell-mode-map)

  ;; ensure that company falls back to dabbrevs when haskell-mode cannot
  ;; complete, such as in where clauses (this is straight from
  ;; haskell-mode docs)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

;; key chords

(use-package key-chord
  :config
  (key-chord-mode 1)
  ;; access the C-c keymap with a comfortable key-chord
  ;; TODO access both mode-specific-map, and major mode's bindings that
  ;; are prefixed with C-c
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

;;; automatic whitespace handling

(use-package ws-butler
  :if (spw--optional-pkg-available-p "ws-butler")
  :diminish ws-butler-mode
  :demand
  :init
  ;; message-mode is sensitive to trailing whitespace in sig dashes
  ;; and empty headers.  markdown-mode is sensitive in empty headers
  ;; (e.g. "# " which I use in writing essays) and newlines that
  ;; indicate paragraph flow (obscure Markdown feature)
  (setq ws-butler-global-exempt-modes
        '(markdown-mode message-mode))
  :config
  (ws-butler-global-mode))

;;; pomodoro timer

(use-package redtick
  :if (spw--optional-pkg-available-p "redtick")
  :bind ("C-c P" . redtick-mode)
  :init
  (setq redtick-history-file nil)
  ;; :config
  ;; this is needed with no history file
  ;; TODO fix upstream code
  ;; (remove-hook 'redtick-after-rest-hook #'redtick--save-history)
  )

;;; notmuch

(use-package notmuch
  :if (spw--optional-pkg-available-p "notmuch")
  :bind (("C-c m" . notmuch-jump-search)
         ("C-c s" . notmuch-search))
  :init
  ;; these let bindings avoid the need to add saved searches to the
  ;; database, so that our database remains recreateable from just my
  ;; Maildirs & mboxes
  (let ((debian
         (concat
          "(to:lists.debian.org or to:lists.alioth.debian.org or to:bugs.debian.org"
          " or from:bugs.debian.org or from:ftp-master.debian.org or from:release.debian.org)"
          " and not to:-announce"))
        (feeds "from:rss@spwhitton.name")
        (ua "to:spwhitton@email.arizona.edu or from:email.arizona.edu"))
    (setq notmuch-saved-searches
          `((:name "weekday unread" :key "u" :search-type nil :sort-order oldest-first
                   :query ,(concat
                            "tag:unread and not (" debian ") and not (" feeds ")"))
            (:name "weekend unread" :key "w" :search-type nil :sort-order oldest-first
                   :query ,(concat "tag:unread and not (" ua ")"))
            (:name "personal unread" :key "p" :search-type nil :sort-order oldest-first
                   :query ,(concat
                            "tag:unread and not ("
                            ua
                            ") and not ("
                            debian
                            ") and not ("
                            feeds
                            ")"))
            (:name "UA unread" :key "W" :search-type nil :sort-order oldest-first
                   :query ,(concat "tag:unread and (" ua ")"))
            (:name "Debian unread" :key "d" :search-type nil :sort-order oldest-first
                   :query ,(concat "tag:unread and (" debian ")"))
            (:name "feeds unread" :key "f" :search-type nil :sort-order oldest-first
                   :query ,(concat "tag:unread and (" feeds ")"))
            (:name "flagged" :key "F" :search-type tree
                   :query "tag:flagged" )
            (:name "sent" :key "s" :search-type nil :sort-order newest-first
                   :query "from:spwhitton@spwhitton.name or from:spwhitton@email.arizona.edu")
            (:name "drafts" :key "D" :search-type nil :sort-order newest-first
                   :query "tag:draft")
            (:name "all unread" :key "U" :search-type nil :sort-order oldest-first
                   :query "tag:unread")
            (:name "all mail" :key "a" :search-type nil :sort-order newest-first
                   :query "*"))))

  (defun spw--notmuch-import-gpg ()
    (interactive)
    (when (get-buffer "*notmuch-pipe*")
      (with-current-buffer "*notmuch-pipe*"
        (let ((buffer-read-only nil)) (erase-buffer))))
    (notmuch-show-pipe-message t "gpg --decrypt | gpg --import")
    (display-buffer "*notmuch-pipe*"))

  (use-package notmuch-message
    :config
    (bind-key "C-c C-s" 'message-goto-subject notmuch-message-mode-map))

  (setq notmuch-tagging-keys
        '(("u" ("+unread") "Mark as unread")
          ("s" ("-unread" "+spam") "Mark as spam")
          ("m" ("-unread" "+killed") "Kill thread") ; 'm' for 'mute'
          ("d" ("-unread" "+deleted") "Send to trash")))

  ;; default is t, but given that notmuch searches run to the
  ;; beginning of time, and we are likely to want recent mail, we want
  ;; newer e-mails at the top
  (setq notmuch-search-oldest-first nil)

  ;; this ensures that hitting C-x m right after Emacs starts yields a
  ;; message with the correct From: address and User-Agent header, etc.
  (defun compose-mail--load-notmuch (&rest ignore)
    (require 'notmuch))
  (advice-add 'compose-mail :before #'compose-mail--load-notmuch)

  (setq send-mail-function 'sendmail-send-it)

  ;; always decrypt & verify PGP parts
  (setq notmuch-crypto-process-mime t)
  ;; have Emacs set envelope-from to be on the safe side
  (setq mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)

  (setq notmuch-archive-tags '("-unread"))
  (setq notmuch-maildir-use-notmuch-insert t
        notmuch-fcc-dirs "sent -unread")

  ;; when 'unread' is being used as an inbox, want manual resolution
  ;; of messages
  (setq notmuch-show-mark-read-function (lambda (beg end)))
  ;; but always resolve when I write a reply
  (setq notmuch-message-replied-tags '("-unread" "+replied"))

  (setq notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)

  ;; TODO upstream?
  (defun message-newline-and-reformat--delete-superfluous-newlines (&rest ignore)
    "Have `message-newline-and-reformat' get rid of some more superflous blank quoted lines."
    (save-excursion
      (forward-line -2)
      (when (looking-at ">[[:space:]]*$")
        (kill-line 1)))
    (save-excursion
      (forward-line 2)
      (when (looking-at ">[[:space:]]*$")
        (kill-line 1))))
  (advice-add 'message-newline-and-reformat
	      :after #'message-newline-and-reformat--delete-superfluous-newlines)

  ;; TODO generalise the following hack into something that can be
  ;; upstreamed

  ;; Harald Hanche-Olsen <https://emacs.stackexchange.com/a/3339>
  (defmacro add-hook-run-once (hook function &optional append local)
    "Like add-hook, but remove the hook after it is called"
    (let ((sym (make-symbol "#once")))
      `(progn
         (defun ,sym ()
           (remove-hook ,hook ',sym ,local)
           (funcall ,function))
         (add-hook ,hook ',sym ,append ,local))))

  (defun spw--notmuch-next-command-kills--remove ()
    (setq notmuch-archive-tags '("-unread")))
  (defun spw--notmuch-next-command-kills--add ()
    (setq notmuch-archive-tags '("-unread" "+killed"))
    (add-hook-run-once
     'post-command-hook
     'spw--notmuch-next-command-kills--remove))
  (defun spw--notmuch-next-command-kills ()
    (interactive)
    (message "Next archive command will also kill")
    (add-hook-run-once
     'pre-command-hook
     'spw--notmuch-next-command-kills--add))

  (defun spw--notmuch-next-command-unread--remove ()
    (setq notmuch-archive-tags '("-unread")))
  (defun spw--notmuch-next-command-unread--add ()
    (setq notmuch-archive-tags '("+unread"))
    (add-hook-run-once
     'post-command-hook
     'spw--notmuch-next-command-unread--remove))
  (defun spw--notmuch-next-command-unread ()
    (interactive)
    (message "Next archive command will instead mark as unread")
    (add-hook-run-once
     'pre-command-hook
     'spw--notmuch-next-command-unread--add))

  (defun spw--notmuch-next-command-spam--remove ()
    (setq notmuch-archive-tags '("-unread")))
  (defun spw--notmuch-next-command-spam--add ()
    (setq notmuch-archive-tags '("-unread" "+spam"))
    (add-hook-run-once
     'post-command-hook
     'spw--notmuch-next-command-spam--remove))
  (defun spw--notmuch-next-command-spam ()
    (interactive)
    (message "Next archive command will also mark as spam")
    (add-hook-run-once
     'pre-command-hook
     'spw--notmuch-next-command-spam--add))

  ;; (defun spw--notmuch-next-command-kills ()
  ;;   (interactive)
  ;;   (let ((old notmuch-archive-tags))
  ;;     (message "Next archive command will also kill")
  ;;     (add-to-list 'notmuch-archive-tags "+killed")
  ;;     (set-transient-map nil nil
  ;;                        (lambda ()
  ;;                          (setq notmuch-archive-tags old)))))

  :config
  ;; some bindings
  (bind-key "S-SPC" 'notmuch-tree-scroll-message-window-back notmuch-tree-mode-map)
  (bind-key "g" (notmuch-tree-close-message-pane-and #'notmuch-show-reply) notmuch-tree-mode-map)
  (bind-key "C-c |" 'spw--notmuch-import-gpg notmuch-show-mode-map)

  (bind-key "K" 'spw--notmuch-next-command-kills notmuch-tree-mode-map)
  (bind-key "K" 'spw--notmuch-next-command-kills notmuch-search-mode-map)
  (bind-key "K" 'spw--notmuch-next-command-kills notmuch-show-mode-map)

  (bind-key "U" 'spw--notmuch-next-command-unread notmuch-tree-mode-map)
  (bind-key "U" 'spw--notmuch-next-command-unread notmuch-search-mode-map)
  (bind-key "U" 'spw--notmuch-next-command-unread notmuch-show-mode-map)

  (bind-key "S" 'spw--notmuch-next-command-spam notmuch-tree-mode-map)
  (bind-key "S" 'spw--notmuch-next-command-spam notmuch-search-mode-map)
  (bind-key "S" 'spw--notmuch-next-command-spam notmuch-show-mode-map))

;;; my d20 roleplaying functions

(use-package spwd20 :commands spwd20-mode)

;;; epubs

(use-package nov
  :if (spw--optional-pkg-available-p "nov")
  :mode ("\\.epub" . nov-mode)
  :init (setq nov-text-width 80))

;;; CIDER

(use-package cider
  :if (spw--optional-pkg-available-p "cider")
  :init
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

;;; find lines violating 80 cols rule

;; package is no longer maintained because author suggests using
;; `whitespace-mode'; however, customising whitespace-mode to display
;; only long lines, and not all the stuff it usually displays, means
;; it can't be toggled on and off to quickly show other whitespace,
;; which can be useful

;; note that highlight-80+ has the advantage over the likes of
;; fill-column-indicator of not using overlays, which easily conflict
;; with other packages

(use-package highlight-80+
  :diminish highlight-80+-mode
  :commands highlight-80+-mode
  :init (dolist (hook '(prog-mode-hook message-mode-hook))
          (add-hook hook 'highlight-80+-mode)))



;;;; ---- functions and bindings ----

;;; for dealing with blocks of lines (inspired by vim, which does a
;;; better job of handling these than Emacs)

;; possible enhancement: M-i goes into a transient mode where C-n and
;; C-p are like 'j' and 'k' in vim's mode accessed with 'V' (I think
;; "visual line mode")

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
(put 'narrow-to-region 'disabled nil)

;;; killing of words and regions

;; Possible enhancement: C-w deletes back to the previous space
;; character, while M-backspace does what spw--backward-delete-word
;; does.  This is useful in bash
(bind-key "C-w" 'backward-kill-word)

;; a nicer kill-region binding
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
        (forward-line -1)
        (beginning-of-line)
        (when (not (looking-at ".*;.*"))
          (forward-line 1)
          (delete-indentation))))))

(defun spw--cleanup ()
  "Clean up buffer, or region if mark is active, depending on major mode.

Note that `ws-butler-mode' is also at work."
  (save-restriction
    (when (use-region-p)
      (narrow-to-region (region-beginning) (region-end)))
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

;;; more functions

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
(bind-key "s" 'spw--toggle-window-split ctl-x-4-map)

;; there are many variations on this online
(defun spw--rotate-windows (arg)
  "Rotate your windows, reversing direction if ARG.

By Robert Bost, based on work by Steve Yegge, Colin Doering and others."
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))
(bind-key "t" 'spw--rotate-windows ctl-x-4-map)

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
            (while (re-search-forward key nil t)
              (replace-match value))))))
(bind-key "C-c g u" 'gleitzman--unicode-hunt)

;;; saving lines

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

;;; opening terminals

(defun spw--open-term-here ()
  "Open a fresh xfce4 terminal in current directory."
  (interactive)
  (call-process "xfce4-terminal" nil 0 nil
                (concat "--working-directory="
                        (expand-file-name  default-directory))
                "-e"  "/bin/bash"))
(bind-key "C-c g g" 'spw--open-term-here)

;;; rebasing dotfiles

(defun spw--dotfiles-rebase ()
  "Rebase & push dotfiles."
  (interactive)
  (let ((default-directory (expand-file-name "~/src/dotfiles/"))
        (buffer (get-buffer-create "*dotfiles rebase*")))
    (display-buffer "*dotfiles rebase*")
    (async-shell-command "git-dotfiles-rebase" "*dotfiles rebase*")))
(bind-key "C-c g d" 'spw--dotfiles-rebase)

;;; message-mode functions

;; I had my own version of these two functions but Michael
;; Stapelberg's were better, taking better advantage of built-in
;; functions, so these are from his config

(defun spw--recipient-first-name ()
  "Attempt to extract the first name of the recipient of a `message-mode' message.

Used in my `message-mode' yasnippets."
  (let ((to (message-fetch-field "To")))
    (if to
        (spw--extract-first-name (nth 0 (mail-extract-address-components to)))
      "")))

(defun spw--extract-first-name (full-name)
  (if (stringp full-name)
      (if (string-match "\\([^ ]+\\)" full-name)
          (let ((first-name (match-string 0 full-name)))
            (cond
             ;; exceptions for people who have longer forms of their names
             ;; in their From: headers
             ((string= first-name "Nathaniel") "Nathan")
             ((string= first-name "Thomas") "Tom")
             ;; default
             (t first-name)))
        ;; no spaces: assume whole thing is an alias and use it
        full-name)
    ""))



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

;; fallback expanding
(bind-key "M-/" 'hippie-expand)

(bind-key "C-c ." 'repeat)

;;; mode-specific

(bind-key "C-c u w" 'wdired-change-to-wdired-mode dired-mode-map)

;;; toggling

(bind-key "C-c t e" 'toggle-debug-on-error)
(bind-key "C-c t i" 'org-indent-mode)

;;; evaluation

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
  ;; TODO fix this upstream
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
      set-mark-command-repeat-pop nil)

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
      select-enable-primary nil
      select-enable-clipboard t
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

;; dabbrev should be case-insensitive
(setq dabbrev-case-fold-search t)

;; view mode should be read-only
(setq view-read-only t)

;;; don't ask me before following symlinks to files in version control
(setq vc-follow-symlinks t)

;;; colours in comint modes

(ansi-color-for-comint-mode-on)

;;; show column numbers as well as line numbers in the mode line

(setq column-number-mode t)



;;;; ---- major modes configuration ----

;;; mail mode for mutt & notmuch

(use-package sendmail :commands mail-add-attachment)

(use-package message
  :mode ("/mutt-.*$" . message-mode)
  :init
  ;; mutt uses a blank line to separate the headers from the message
  ;; body; to ensure proper font locking and other behaviour, tell
  ;; Emacs about that
  (defun spw--mutt-mail-header-separator ()
    (when (string-match-p "^mutt-" (buffer-name (current-buffer)))
      (setq-local mail-header-separator "")))
  (add-hook 'message-mode-hook 'spw--mutt-mail-header-separator)

  ;; automatic formatting/templating of messages
  (use-package message-templ
    :if (spw--optional-pkg-available-p "message-templ")
    :commands message-templ-config-exec
    :init
    (setq message-templ-alist
          '(("default"
             ("From" . "Sean Whitton <spwhitton@spwhitton.name>"))
            ("UA"
             ("From" . "Sean Whitton <spwhitton@email.arizona.edu>"))
            ("Debian"
             ("From" . "Sean Whitton <spwhitton@debian.org>"))))
    (setq message-templ-config-alist
          '(("^\\(To\\|Cc\\|Bcc\\):.*@.*\\(\.edu\\|\.ac\.uk\\)"
             (lambda ()
               (message-templ-apply "UA")
               (mml-unsecure-message))))))

  (add-hook 'message-mode-hook 'footnote-mode)
  (add-hook 'message-mode-hook 'message-goto-body)
  ;; need orgstruct++ rather than just orgstruct so that filling
  ;; doesn't break lists
  (add-hook 'message-mode-hook 'orgstruct++-mode)

  :config
  ;; code to automatically format a message

  (make-variable-buffer-local
   (defvar spw--message-normalised nil
     "Whether `spw--message-normalise' has been run in this buffer."))

  (defun notmuch-mua-send-and-exit--check-normalised (orig-fun &rest args)
    "Prompt before sending a message if `spw--normalise-message' not yet called."
    (when (or (bound-and-true-p spw--message-normalised)
              (y-or-n-p "Send message without having invoked `spw-message-normalise'?"))
      (apply orig-fun args)))
  (advice-add 'notmuch-mua-send-and-exit :around
              #'notmuch-mua-send-and-exit--check-normalised)

  (defun spw--normalise-message ()
    "Autoformat a message before sending.

The state after this function has been called is meant to be like
mutt's review view after exiting EDITOR."
    (interactive)
    (setq spw--message-normalised t)
    ;; sign messages by default, though avoid clobbering a
    ;; 'signencrypt' tag added when replying to an encrypted message
    (if (mml-secure-is-encrypted-p)
        (mml-secure-message-sign-encrypt)
      (mml-secure-message-sign-pgpmime))
    ;; set up From address, etc.; this also undoes the PGP signature
    ;; tag where necessary
    (message-templ-config-exec)
    (spw--compact-blank-lines)
    (save-excursion
      (spw--message-goto-body--skip-mml-secure)
      ;; also skip over Debian BTS control lines, which shouldn't be
      ;; wrapped
      (when (looking-at "^[cC]ontrol: .+$")
        (while (looking-at "^[cC]ontrol: .+$")
          (forward-line 1))
        (if (looking-at "\n")
            (forward-line 1)
          (newline)))
      (let ((body (point)))
        ;; ensure there is at least a basic salutation
        (unless (looking-at "^[A-Z].+,\n\n")
          (insert "Hello,\n\n")
          (when (looking-at "\n")
            (delete-blank-lines)))
        (message-goto-signature)
        (unless (eobp)
          (end-of-line -1))
        ;; delete trailing whitespace in message body, when that
        ;; message body exists (this protects signature dashes and
        ;; empty headers)
        (when (< body (point))
          (delete-trailing-whitespace body (point)))
        ;; make any remaining trailing whitespace visible to the user
        (setq-local show-trailing-whitespace t)
        ;; ensure there is a newline before the signature dashes
        (unless (bolp)
          (insert "\n"))
        (undo-boundary)
        (save-restriction
          (narrow-to-region body (point))
          (message-fill-yanked-message))
        (message "Hit undo if the quoted message was too aggressively wrapped"))))
  ;; I do not need a key to insert the Newsgroups: header
  (bind-key "C-c C-n" 'spw--normalise-message message-mode-map)

  ;; a convenient macro for something I find myself often doing by hand
  (defun spw--message-delete-and-normalise ()
    (interactive)
    (newline)
    (message-kill-to-signature)
    (spw--normalise-message))
  (bind-key "<f9>" 'spw--message-delete-and-normalise message-mode-map)

  (defun spw--notmuch-decrypt-inline ()
    (interactive)
    (epa-decrypt-armor-in-region (point-min) (point-max)))

  ;; bindings

  ;; C-c C-b should skip over mml's sign/encrypt lines (it is a bad
  ;; idea to advise `message-goto-body' as various functions assume it
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
  (bind-key "C-c C-s" 'message-goto-subject message-mode-map)

  ;; this attachment function has sensible defaults so requires less
  ;; typing than the default binding to C-c C-a.  From Michael
  ;; Stapelberg's config
  (define-key message-mode-map (kbd "C-c C-a") 'mail-add-attachment)
  (define-key mml-mode-map [menu-bar Attachments Attach\ File...]
    '("Attach File..." . mail-add-attachment))

  ;; miscellaneous preferences

  ;; follow the rest of the world
  (setq message-forward-before-signature nil)

  ;; try to strip signatures when citing
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)

  ;; with defaults, this gets us "On X, Y wrote:" lines
  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  ;; default dir for saving attachments
  (setq mm-default-directory "~/tmp/")

  ;; ensure encrypted messages are also encrypted to me, so I can read
  ;; them in my sent mail folder
  (setq mml-secure-openpgp-encrypt-to-self t)

  ;; disable openwith-mode when sending mail (i.e. attach the PDF,
  ;; rather than opening it in evince and aborting the send)
  (require 'mm-util)
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

  ;; don't let sent messages hang around
  (setq message-kill-buffer-on-exit t))

;;; C-c C-c to save-and-exit emacsclient (like <esc>ZZ in vim)

;; this overrides the major mode's C-c C-c binding (this is important
;; for message-mode, so that the message doesn't get sent by notmuch,
;; bypassing mutt's invocation of emacsclient)

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

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(diminish 'auto-fill-function)

;;; LaTeX

;; (setq TeX-auto-save t
;;       TeX-parse-self t
;;       LaTeX-indent-level 4
;;       LaTeX-item-indent -2
;;       TeX-newline-function 'reindent-then-newline-and-indent)

;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince %o")
        ("^html?$" "." "firefox %o"))))

;;; fixes for exporting from Org-mode

;; ;; (setq TeX-auto-save t)
;; ;; (setq TeX-parse-self t)
;; (setq-default TeX-master t)
;; (make-variable-buffer-local 'TeX-master)

;;; the Emacs calculator

;; disable line numbering primarily so that killing and copying stack
;; entries puts the number alone in the kill-ring
(setq calc-line-numbering nil)

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

(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode))
  :init
  ;; (setq cperl-continued-statement-offset 4)
  (setq cperl-indent-level 4)
  (setq cperl-indent-wrt-brace t)
  (setq cperl-lineup-step 1))

;;; changelogs

(defun spw--change-log-setup ()
  (setq-local indent-tabs-mode nil)
  (setq-local left-margin 2)
  (orgstruct++-mode 1))
(add-hook 'change-log-mode-hook 'spw--change-log-setup)

(setq debian-changelog-mailing-address "spwhitton@spwhitton.name")

;;; cc-mode

;; the built-in 'linux' style doesn't explicitly include tabs, so with
;; indent-tabs-mode set to nil, cc-mode will not use tabs.  But the
;; kernel style guide mandates tabs, so make a slightly modified style
;; TODO fix in the 'linux' style in upstream Emacs
(c-add-style "linux-tabs" '("linux" (indent-tabs-mode . t)))
(setq c-default-style "linux-tabs")

;; following setting also part of Linux kernel style, but it's from
;; newcomment.el, not cc-mode, so must be set in addition to
;; `c-default-style'
(setq comment-style 'extra-line)
