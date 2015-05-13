;;; init --- Sean's Emacs configuration

;;; Commentary:

;;; Code:

;;;; ---- package management ----

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'use-package)
(require 'package)

(setq
 package-user-dir "~/local/src/elpa"
 package-archives
 '(("melpa" . "http://melpa.org/packages/")
   ("melpa-stable" . "http://stable.melpa.org/packages/")
   ;; ("marmalade" . "http://marmalade-repo.org/packages/")
   ("org" . "http://orgmode.org/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/"))
 package-pinned-packages
 '((org-plus-contrib . "org")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-enable-at-startup nil)

;;;; ---- basic settings ----

;;; customisation -- must be loaded early so that zenburn theme is
;;; considered safe

(defconst custom-file (concat user-emacs-directory "init-custom.el"))
(load custom-file 'noerror)

;; when I try do elisp experimentation I use IELM so make scratch
;; buffer a little more co-operative
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
      focus-follows-mouse t)

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

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

;; kill the fringes, if we have window system support compiled in
(if (fboundp 'set-fringe-mode)
    (set-fringe-mode 0))

;; Terminus
(if (member "Terminus-11" (font-family-list))
    (set-default-font "Terminus-11"))
(add-to-list 'default-frame-alist '(font . "Terminus-11"))

;; disable GUI elements
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; winner mode: undo window configuration changes

(winner-mode 1)

;;; cursor settings

(setq x-stretch-cursor t)
(setq-default cursor-type 'box)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0)) ; turns off blink-cursor-mode if it ended up on

;; get the mouse out of the way
(mouse-avoidance-mode 'exile)

;;; zenburn

(use-package zenburn-theme
  :ensure
  :init (load-theme 'zenburn))

;;; sexy mode line

(use-package smart-mode-line
  :ensure
  :init (progn
          (use-package powerline :ensure)
          (use-package smart-mode-line-powerline-theme :ensure)
          (sml/setup)
          (sml/apply-theme 'powerline)
          (setq sml/shorten-directory nil
                sml/shorten-modes t
                sml/mode-width 'right
                sml/vc-mode-show-backend t)))

;;; I'm in Korea

(if (not (eq system-type 'windows-nt))
    (set-time-zone-rule "/usr/share/zoneinfo/Asia/Seoul"))

;;; be sure to start the server

(require 'server)
(unless (server-running-p)
  (server-start))



;;;; ---- packages ----

;;; clean up the mode line

(use-package diminish :ensure)

;;; libraries of useful lisp functions

(use-package f :ensure) (use-package s :ensure) (use-package dash :ensure)

;;; instead of vim text objects

(use-package expand-region
  :ensure
  :bind ("M-i" . er/expand-region)
  :init (progn
          (setq expand-region-contract-fast-key (kbd "o"))
          ;; fill out the region to the beginning and ends of the
          ;; lines at either end of it when we're not using
          ;; expand-region but we've activated the mark (but only do
          ;; this once)
          (defadvice er/expand-region (around fill-out-region activate)
            (if (or (not (region-active-p))
                    (eq last-command 'er/expand-region))
                ad-do-it
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
                  (forward-char 1)))))))

;;; keep parentheses under control: modern replacement for the mighty paredit

(use-package smartparens
  :ensure
  :bind (("M-J" . sp-join-sexp)

         ;; for when I use Emacs via PuTTY
         ("M-<right>" . sp-forward-slurp-sexp)
         ("M-<left>" . sp-forward-barf-sexp))
  :commands (smartparens-strict-mode smartparens-mode show-smartparens-global-mode)
  :init (dolist (hook '(emacs-lisp-mode-hook
                        lisp-mode-hook
                        lisp-interaction-mode-hook
                        ielm-mode-hook
                        scheme-mode-hook
                        inferior-scheme-mode-hook
                        python-mode-hook
                        minibuffer-setup-hook
                        haskell-mode-hook))
          (add-hook hook
                    (lambda ()
                      (smartparens-strict-mode))))
  :idle (progn
          (show-smartparens-global-mode)

          ;; non-strict mode the default, and strict mode in some
          ;; programming language major modes
          ;; (smartparens-global-mode)
          )
  :config (progn
            (require 'smartparens-config)
            (setq sp-navigate-consider-symbols t)

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

            (defadvice sp-backward-kill-word (after sp-backward-kill-word-fix-punctuation activate)
              ;; when killing the first word of a sentence, leave the
              ;; two spaces after the previous sentence's terminal
              ;; period
              (save-excursion
                (backward-char 2)
                (if (and
                     (or
                      (looking-at "\\. ")
                      (looking-at   "! ")
                      (looking-at "\\? "))
                     (not (looking-back "^[1-9]+")))
                    (progn
                      (forward-char 1)
                      (insert " ")))))

            ;; fix cursor position after M-d at the beginning of a line
            (defadvice sp-kill-word (after sp-kill-word-beg-of-line-fix activate)
              (if (looking-back "^[[:space:]]")
                  (backward-char 1)))

            (defadvice sp-backward-delete-char (around sp-backward-delete-char-remove-indentation activate)
              ;; when after whitespace at the beginning of a line or
              ;; an Org bullet or heading, delete it all
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
                ad-do-it))

            (defadvice sp-backward-kill-word (around sp-backward-delete-word-remove-indentation activate)
              ;; when after whitespace at the beginning of a line or
              ;; an Org bullet or heading, delete it all.  This is
              ;; more intuitive when C-w is one's main way to delete
              ;; stuff
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
                ad-do-it))

            ;; define some additional pairings for Org-mode
            (sp-local-pair 'org-mode "=" "=") ; verbatim
            ;; (sp-local-pair 'org-mode "*" "*")
            ;; (sp-local-pair 'org-mode "/" "/")
            ;; (sp-local-pair 'org-mode "~" "~") ; code
            ;; (sp-local-pair 'org-mode "+" "+")
            ;; (sp-local-pair 'org-mode "_" "_")

            (defadvice sp--cleanup-after-kill (after haskell-sp-unindent activate)
              (when hi2-mode
                (hi2-indent-backwards)))

            ))

;;; Org

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org" . org-mode)
         ("\\.org_archive" . org-mode))
  :commands (org-capture
             org-store-link
             org-agenda
             spw/org-agenda-file-to-front
             spw/org-remove-file)
  :diminish org-indent-mode
  :config (load "~/.emacs.d/init-org.el"))

;;; keep pop up windows under control

(use-package popwin
  :ensure
  :commands popwin-mode
  :idle (popwin-mode 1))

;;; save my places in buffers; this is all the session management I need

(setq recentf-save-file "~/.emacs.d/recentf")

;; the three hooks added by the idle progn below don't stay set when
;; set by (require 'saveplace), nor do they remain in place if simply
;; added in this config file or even in 'after-init-hook.  So have
;; use-package add them a few seconds after Emacs starts

(use-package saveplace
  :init (setq-default save-place t
                      save-place-file "~/.emacs.d/saveplace")
  :idle (progn
          (add-hook 'find-file-hook 'save-place-find-file-hook t)
          (add-hook 'kill-emacs-hook 'save-place-kill-emacs-hook)
          (add-hook 'kill-buffer-hook 'save-place-to-alist)))

;;; fix up whitespace around kill and yanking (package seems to be
;;; unavailable for download)

(use-package smart-whitespace-comment-fixup :disabled t :ensure
  ;; the advice that this package applies to indent-for-tab-command
  ;; uses a function (line-matches) which doesn't seem to exist
  :config (ad-deactivate 'indent-for-tab-command))

;;; more useful unique buffer names

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward))

;;; OpenWith

(use-package openwith
  :ensure
  :commands openwith-mode
  :idle (openwith-mode t))

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
  :ensure
  :diminish magit-auto-revert-mode
  :config (progn
            ;; C-c C-a to amend without any prompt
            (defun magit-just-amend ()
              (interactive)
              (save-window-excursion
                (magit-with-refresh
                 (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
            (bind-key "C-c C-a" 'magit-just-amend magit-status-mode-map)

            (use-package magit-annex :ensure)

            (use-package magit-wip
              :diminish magit-wip-save-mode
              :config (global-magit-wip-save-mode 1))))

;;; pointback mode: make sure that point is back where I left it when
;;; switching between buffers where at least one buffer is displayed
;;; in more than one window

(use-package pointback
  :ensure
  :commands global-pointback-mode
  :idle (global-pointback-mode 1))

;;; colour those parentheses

(setq-default frame-background-mode 'dark)
(use-package rainbow-delimiters
  :ensure
  :commands rainbow-delimiters-mode)

;;; and colour those colours

(use-package rainbow-mode
  :ensure
  :commands rainbow-mode
  :init (progn
          (add-hook 'html-mode-hook 'rainbow-mode)
          (add-hook 'css-mode-hook 'rainbow-mode)))

;;; keep reindenting lisp

(use-package aggressive-indent
  :ensure
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
              (turn-on-eldoc-mode)
              ;; (diminish 'eldoc-mode)
              (aggressive-indent-mode)
              (rainbow-delimiters-mode t))))

;;; boxquotes

(use-package boxquote
  :ensure
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

(use-package wc-mode :ensure)

;;; company-mode for smart and easy completion

(use-package company
  :ensure
  ;; :commands global-company-mode
  ;; :bind ("<tab>" . company-complete)
  ;; :idle (global-company-mode)
  :diminish company-mode
  :config (progn
            ;;; startup company

            (defun spw/company-prog-setup ()
              "Setup company mode carefully when its needed, rather than using the brash global-company-mode"
              (company-mode 1)
              (define-key (current-local-map) (kbd "M-/") 'company-complete)
              )
            (add-hook 'prog-mode-hook 'spw/company-prog-setup)
            ;; alternative approach: https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387

            ;; I like my C-w binding so move one of company's bindings
            (define-key company-active-map "\C-w" nil)
            (bind-key "M-o" 'company-show-location company-active-map)

            ;;; settings

            (setq company-idle-delay nil
                  company-minimum-prefix-length 0
                  company-echo-delay 0)

            (add-to-list 'company-backends 'company-capf)
            (add-to-list 'company-transformers 'company-sort-by-occurrence)

            ;;; python code completion

            (use-package anaconda-mode
              :disabled t
              :ensure
              :config (progn
                        (add-hook 'python-mode-hook 'anaconda-mode)
                        (add-to-list 'company-backends 'company-anaconda)
                        (add-hook 'python-mode-hook 'anaconda-eldoc)))))
;; C-o during company isearch narrows to stuff matching that search;
;; mnemonic 'occur'.  C-M-s while outside of search to do the same
;; thing

;;; Randomize the order of lines in a region

(use-package randomize-region :commands randomize-region)

;;; Markdown mode

(use-package markdown-mode
  :ensure
  :init (progn
          (add-hook 'markdown-mode-hook 'turn-on-orgstruct)
          (add-hook 'markdown-mode-hook 'turn-on-orgstruct++))
  :mode "\\.md")

;;; PHP mode

(use-package php-mode :ensure :mode (("\\.php" .  php-mode)))

;;; Deft

(use-package deft
  :ensure
  :init (setq deft-extension "org"
              deft-text-mode 'org-mode
              deft-directory "~/doc/org/"
              deft-use-filename-as-title nil
              deft-auto-save-interval 20.0
              deft-incremental-search t
              ;; don't just strip the leading hash but the whole #+TITLE:
              deft-strip-title-regexp "\\(?:\\#\\+TITLE\\: \\|\\#\\+FILETAGS\\: \\|^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)")
  :config (progn
            (bind-key "C-w" 'deft-filter-decrement-word deft-mode-map)
            ;; (bind-key "C-h" 'deft-filter-decrement deft-mode-map)

            (defadvice deft (before persp-deft activate)
              (projectile-persp-switch-project "~/doc"))

            (defadvice deft-new-file (after insert-org-TITLE activate)
              (save-excursion
                (goto-char (point-min))
                (insert "#+TITLE: ")))))

;;; The following two python packages seem to be unavailable for
;;; download.  Disable them for now.

;;; allow lisp to interact with python

(use-package pymacs :disabled t :ensure)

;;; Get Python documentation as info files

(use-package python-info :disabled t :ensure)
;;(use-package pydoc-info :ensure)

;;; flycheck

(use-package flycheck
  :ensure
  :idle (global-flycheck-mode)
  ;; disable flymake; having both running at the same time is annoying
  :init (setq flymake-allowed-file-name-masks nil))

;;; TRAMP

(use-package tramp
  :config (progn
            (add-to-list 'tramp-default-user-alist '(nil "sdf" "spw"))
            (add-to-list 'tramp-default-user-alist '("sudo" "localhost" "root"))
            (add-to-list 'tramp-default-user-alist '(nil nil "swhitton") t)
            (add-to-list 'tramp-default-user-alist '(nil "ma" "spw"))
            (add-to-list 'tramp-default-user-alist '(nil "sage" "spwhitton"))

            ;; TRAMP and zsh are not friends so might as well switch
            ;; over here
            (setenv "SHELL" "/bin/bash")))

;;; ebib for editing BiBTeX databases

(use-package ebib
  :ensure
  :bind ("C-c g e" . ebib)
  :init (progn
          (defadvice ebib (before spw/persp-ebib activate)
            (persp-switch "ebib"))
          (setq ebib-preload-bib-files '("~/doc/spw.bib"))))

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

(use-package git-annex :ensure)

;;; simple concept of projects

(use-package projectile
  :ensure
  :commands projectile-vc
  :init (projectile-global-mode)
  :diminish 'projectile-mode
  :config (progn
            (setq projectile-switch-project-action 'projectile-dired
                  projectile-completion-system 'helm)
            (diminish 'projectile-mode)))

(use-package persp-projectile :ensure)

(use-package perspective
  :ensure
  :commands (persp-toggle persp-switch)
  :init (progn
          (setq persp-modestring-dividers '("" "" "|"))

          ;; activate persp mode, but don't activate it if it's
          ;; already active cos this removes all existing perspectives
          ;; which is annoying
          (unless persp-mode
            (persp-mode))

          (defun persp-toggle (arg)
            (interactive "P")
            (if arg (call-interactively 'persp-switch)
              (persp-switch (persp-find-some))))

          ;; save and restore a base window configuration ala
          ;; workgroups.el.  Designed to handle the perennial Emacs
          ;; problem of Emacs totally screwing up your windows in the
          ;; middle of your work
          (setq persp-basewcs nil)
          (defun persp-basewc-save ()
            (interactive)
            (let* ((name (persp-name persp-curr))
                   (wc (current-window-configuration))
                   (pair (cons name wc)))
              (setq persp-basewcs
                    (remove* name persp-basewcs
                             :test 'equal :key 'car))
              (add-to-list 'persp-basewcs pair)))
          ;; Save when opening a perspective.
          (add-hook 'persp-created-hook 'persp-basewc-save)
          ;; Also do a save of the basewc after
          ;; `projectile-persp-switch-project' (I never use
          ;; `projectile-switch-project' directly) switches to dired
          ;; as this is a more sensible initial basewc to go back to.
          (add-hook 'projectile-switch-project-hook 'persp-basewc-save)
          (defun persp-basewc-restore ()
            (interactive)
            (let* ((name (persp-name persp-curr))
                   (pair (assoc name persp-basewcs))
                   (wc (cdr pair)))
              (set-window-configuration wc)))))

;;; powerful completion everywhere, especially for jumping between files

(use-package helm
  :ensure
  :bind (("C-x C-f" . helm-find-files)
         ("M-s o" . helm-occur))
  :diminish helm-mode
  :init (progn
          (require 'helm-config)

          ;; extra sources
          (use-package helm-projectile :ensure)
          (use-package helm-dired-recent-dirs :ensure)
          (use-package imenu-anywhere :ensure)

          ;; no fuzzy matching and other settings
          (setq helm-buffers-fuzzy-matching nil
                helm-time-zone-home-location "Seoul"
                helm-quick-update t
                helm-split-window-in-side-p t
                helm-ff-search-library-in-sexp t
                helm-ff-file-name-history-use-recentf t
                helm-ff-newfile-prompt-p nil
                helm-tramp-verbose 3)

          (helm-mode 1)
          (helm-autoresize-mode 1)

          ;; helm-mode adds an two arguments to the end of the normal
          ;; completing-read argument list, the first of which is
          ;; `cands-in-buffer'.  This makes completion faster because
          ;; if we type 'mast' to match 'master', we can then hit
          ;; <enter> to choose master.  Without `cands-in-buffer',
          ;; hitting enter will select 'mast' (which is intended
          ;; (https://github.com/emacs-helm/helm/issues/376#issuecomment-30872692),
          ;; so that helm-mode is consistent with standard Emacs
          ;; completing-read), and to select master requires an
          ;; additional C-n keypress which is inconvenient
          (defadvice helm-completing-read-default-1
              (around spw/helm-completing-read-cands-in-buffer activate)
            (interactive)
            ;; unfortunately this doesn't work if require-match is
            ;; nil, so block that case
            (let ((require-match (if (not require-match)
                                     'confirm require-match))
                  (cands-in-buffer t))
              ad-do-it))

          (add-to-list 'helm-completing-read-handlers-alist
                       '(org-capture . completing-read-default))

          ;; rebind some keys
          (bind-key "C-w" 'spw/backward-delete-word helm-map)
          (bind-key "C-o" 'helm-select-action helm-map)
          (bind-key "M-i" 'helm-next-source helm-map)

          (defun helm-choose-last ()
            (interactive)
            (helm-end-of-buffer)
            (helm-exit-minibuffer))
          (bind-key "M-RET" 'helm-choose-last helm-map)

          ;; swap <tab> and C-z in helm since use persistent action
          ;; much more frequently
          (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
          (bind-key "C-i" 'helm-execute-persistent-action helm-map)
          (bind-key "C-z" 'helm-select-action helm-map)

          ;; eshell history -- let Helm handle eshell completion
          (add-hook 'eshell-mode-hook
                    #'(lambda ()
                        (define-key eshell-mode-map [remap company-complete] 'helm-esh-pcomplete)))

          ;; My helm mini: add and remove some sources depending
          ;; on context it is launched from.  Also default to "" to
          ;; avoid `thing-at-point'
          (defun spw/helm-mini ()
            (interactive)
            (require 'helm-files)
            (let ((sources '())
                  (excluded-projects '("annex")))

              (add-to-ordered-list 'sources 'helm-source-buffers-list 2)
              (add-to-ordered-list 'sources 'helm-source-imenu-anywhere 4)
              (add-to-ordered-list 'sources 'helm-source-bookmarks 5)
              (add-to-ordered-list 'sources 'helm-source-dired-recent-dirs 7)
              (add-to-ordered-list 'sources 'helm-source-recentf 9)

              ;; Enable projectile stuff if we're in a project and not
              ;; in its eshell buffer, since this doesn't work well
              ;; when TRAMP'd to a remote host.  Disable it for
              ;; certain projects and if we're in the default
              ;; perspective.
              (when (and (not (equal major-mode 'eshell-mode))
                         (projectile-project-p)
                         (not (memq (projectile-project-name) excluded-projects))
                         (not (equal (persp-name persp-curr) "main")))
                (add-to-ordered-list 'sources 'helm-source-projectile-files-list 3)
                (delete 'helm-source-dired-recent-dirs sources))

              ;; Org headlines source: imenu covers all open org
              ;; buffers so better
              ;; (when (or
              ;;        (f-child-of? default-directory org-directory)
              ;;        (f-same? default-directory org-directory))
              ;;   (add-to-ordered-list 'sources 'helm-source-org-headline 1.5))

              ;; begin code from original helm-mini
              (unless helm-source-buffers-list
                (setq helm-source-buffers-list
                      (helm-make-source "Buffers" 'helm-source-buffers)))
              (let ((helm-ff-transformer-show-only-basename nil))
                (helm :default "" :sources sources :buffer "*spw helm mini*"))
              ;; end code from original helm-mini
              (helm-adaptive-mode)))

          (use-package helm-descbinds
            :ensure
            :idle (helm-descbinds-mode))

          ;; jump around current buffer, and if necessary currently
          ;; open buffers
          (use-package helm-swoop
            :disabled t
            :commands (helm-swoop
                       helm-swoop-back-to-last-point
                       helm-multi-swoop
                       helm-multi-swoop-all)
            :config (progn
                      (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)

                      ;; it's better to swoop for the symbol at point
                      ;; explicitly rather than automatically.  Just
                      ;; hit "vao" to select the symbol and then call helm-swoop
                      (setq helm-swoop-pre-input-function (lambda () nil))))

          ;; redefine this Helm function to work nicely with
          ;; perspectives: just replace its code to create or switch
          ;; to the shell buffer with a call to my function
          (defun helm-ff-switch-to-eshell (_candidate)
            "Switch to eshell and cd to `helm-ff-default-directory'."
            (let ((cd-eshell #'(lambda ()
                                 (eshell-kill-input)
                                 (goto-char (point-max))
                                 (insert
                                  (format "cd '%s'" helm-ff-default-directory))
                                 (eshell-send-input))))
              (spw/persp-eshell)
              (unless (get-buffer-process (current-buffer))
                (funcall cd-eshell))))))

;;; snippets

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :idle (yas-global-mode))

;;; htmlize for Org HTML export/publishing

(use-package htmlize :ensure)

;;; close old buffers once per day

(use-package midnight
  :init (midnight-delay-set 'midnight-delay "3am"))

;;; make indentation in python nice and visible

(use-package highlight-indentation
  :ensure
  :init (progn
          (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
          ;; (add-hook 'haskell-mode-hook 'highlight-indentation-current-column-mode)
          ))

;;; jump around what's visible

(use-package ace-jump-mode
  :ensure
  :bind ("M-o" . ace-jump-mode)
  ;; make `ace-jump-mode' respect dired-isearch-filenames
  ;; from https://www.reddit.com/r/emacs/comments/2x3mke/making_acejump_play_nice_with_dired/
  :config (add-hook 'dired-mode-hook
                    (lambda ()
                      (setq-local ace-jump-search-filter
                                  (lambda ()
                                    (get-text-property (point) 'dired-filename))))))

;;; use ace-jump-mode to move between links in help file

(use-package ace-link
  :ensure
  :idle (ace-link-setup-default))

;;; chat on Jabber

(use-package jabber
  :disabled t
  :ensure
  :config (progn (when (f-exists? "~/.emacs.d/init-jabber.el")
                   (load-file "~/.emacs.d/init-jabber.el")
                   (jabber-connect-all))
                 (defun spw/persp-jabber ()
                   (interactive)
                   (persp-switch "xmpp"))
                 ;; TODO: when have Emacs 24.4 everywhere, switch this
                 ;; to nadvice.el macros
                 (defadvice jabber-activity-switch-to (before spw/persp-jabber activate)
                   (spw/persp-jabber))
                 (defadvice jabber-switch-to-roster-buffer (before spw/persp-jabber activate)
                   (spw/persp-jabber))
                 (defadvice jabber-chat-with (before spw/persp-jabber activate)
                   (spw/persp-jabber))))

;;; make dired copy and move asynchronously

(use-package async
  :ensure
  :init (when (require 'dired-aux)
          (require 'dired-async)))

;;; give dired some nice keybindings for browsing images

(use-package image-dired
  :init (image-dired-setup-dired-keybindings))

;;; allow Emacs to resize images: keybinding S f

(use-package eimp
  :ensure
  :init (add-hook 'image-mode-hook 'eimp-mode))

;;; alternative to my old `spw/centre-window'

(use-package centered-window-mode :commands centered-window-mode)

;;; IRC client, for when I need it

(use-package rcirc
  :config (progn
            ;; basic settings
            (setq rcirc-default-nick "seanw"
                  rcirc-default-user-name user-login-name
                  rcirc-default-full-name user-full-name)

            ;; networks and channels
            (setq rcirc-server-alist nil)
            (add-to-list 'rcirc-server-alist
                         '("chat.freenode.net" :port 6697 :encryption tls
                           :channels ("#freenode-social")))

            ;; authentication data
            (when (f-exists? "~/.emacs.d/init-rcirc.el")
              (load-file "~/.emacs.d/init-rcirc.el"))))

;;; zap-up-to-char is at least as useful as zap-to-char, so load it
;;; out of misc.el.  Don't need M-m binding as have smarter C-a

(use-package misc
  :bind ("M-m" . zap-up-to-char))

;;; make Emacs regexps easier

(use-package visual-regexp :ensure)

;;; toggle quotes for when I fail to follow the style guidelines

(use-package toggle-quotes
  :ensure
  :bind ("C-c t '" . toggle-quotes))

;;; have M-w and C-xC-k work on the whole line when region is inactive

(use-package whole-line-or-region
  :ensure
  :diminish whole-line-or-region-mode
  :idle (whole-line-or-region-mode 1))

(use-package hydra
  :ensure
  :init (progn
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
            ("3" split-window-right "vert" :color red))))

;;; aligning rule for Haskell adapted from Haskell mode wiki

(use-package align
  :init (progn
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
                         (modes quote (haskell-mode literate-haskell-mode))))))

;;; edit .nix files

(use-package nix-mode :ensure)

;;; Haskell (load after packages that it depends on)

(load "~/.emacs.d/init-haskell.el")

;; key-chord to save my hands

(use-package key-chord
  :ensure
  :init (key-chord-mode 1))

;;; god-mode to maybe save my hands

(use-package god-mode
  :ensure
  :config (progn
            ;; (unless god-global-mode (god-mode))

            ;; activate with a key-chord to avoid having to have
            ;; capslock bound to both escape and control
            (key-chord-define-global "hj" 'god-mode-all)

            ;; just a little vi in god-mode
            (define-key god-local-mode-map (kbd ".") 'repeat)
            (define-key god-local-mode-map (kbd "i") 'god-mode-all)

            ;; change a part of the modeline depending on whether in god-mode
            (defun spw/god-mode-update-modeline ()
              (set-face-background 'sml/filename (if god-local-mode
                                                     "red"
                                                   ;; following from smart-mode-line-powerline-theme.el
                                                   (or (face-background 'powerline-active1) "Grey30"))))

            (add-hook 'god-mode-enabled-hook 'spw/god-mode-update-modeline)
            (add-hook 'god-mode-disabled-hook 'spw/god-mode-update-modeline)))

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
  :ensure
  :commands turn-on-elisp-slime-nav-mode
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook 'elisp-slime-nav-mode)))

(use-package world-time-mode)

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
    (centered-window-mode 1)
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
    (centered-window-mode 0)
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

(defun spw/backward-delete-word (arg)
  "Delete characters ARG backward until encountering the end of a word."
  (interactive "p")
  (spw/delete-word (- arg)))

(bind-key "C-w" 'spw/backward-delete-word)
(bind-key "C-x C-k" 'kill-region)
(global-set-key "\M-d" 'spw/delete-word)
(bind-key "C-x C-m" 'helm-M-x)

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

(add-hook 'before-save-hook 'spw/auto-cleanup)

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

(defun magnars/copy-to-end-of-line ()
  "Save from point to end of line to keyring."
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))
(defun magnars/copy-whole-lines (arg)
  "Copy ARG lines to the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(defun magnars/copy-line (arg)
  "Copy to end of line.

If ARG, just copy that many lines."
  (interactive "P")
  (if (null arg)
      (magnars/copy-to-end-of-line)
    (magnars/copy-whole-lines (prefix-numeric-value arg))))
(defun magnars/kill-ring-save-dwim (arg)
  "If region active, save it.  Otherwise, save to end of line.

If ARG and region inactive, save that many lines."
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (magnars/copy-line arg)))

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
  (if (region-active-p)
      (let ((align-region-separate 'entire))
        (call-interactively 'align))
    (save-excursion
      (mark-paragraph)
      (call-interactively 'align))))

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
  "Set e-mail from address correctly by looking at other headers."
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
(bind-key "M-w" 'magnars/kill-ring-save-dwim)

;;; bind all up into the C-c keymap

(setq spw/personal-map (make-sparse-keymap))
(key-chord-define-global "jk" spw/personal-map)

(defmacro spw/C-c-bind (key bindee)
  "Bind BINDEE to C-c KEY with `bind-key' macro.

BINDEE may be a command or another keymap, but whatever it is, it should not be quoted."
  `(if (keymapp ,bindee)
       (progn
         (bind-key (concat "C-c " ,key) ,bindee)
         (define-key spw/personal-map (kbd ,key) ,bindee))
     (bind-key (concat "C-c " ,key) bindee)
     (define-key spw/personal-map (kbd ,key) bindee)))

(setq spw/personal-bindings
      '(("p" . projectile-command-map)
        ("j" . spw/helm-mini)
        ("v" . projectile-vc)
        ("n" . mwf/narrow-or-widen-dwim)
        ("s" . spw/persp-eshell)
        ("d" . spw/dired-jump)
        ("f" . deft)
        ("a" . spw/align-dwim)
        ("A" . align-regexp)
        ("l" . persp-toggle)
        ("L" . persp-switch)

        ;; perspectives map
        ("q s" . persp-switch)
        ("q k" . persp-remove-buffer)
        ("q c" . persp-kill)
        ("q r" . persp-rename)
        ("q a" . persp-add-buffer)
        ("q A" . persp-set-buffer)
        ("q i" . persp-import)
        ("q n" . persp-next)
        ("q <right>" . persp-next)
        ("q p" . persp-prev)
        ("q <left>" . persp-prev)
        ("q u" . persp-basewc-save)
        ("q q" . persp-basewc-restore)
        ("q C" . spw/persp-clone)
        ("q o" . spw/open-programming-project)

        ;; Org-mode map
        ("o c" . org-capture)
        ("o l" . org-store-link)
        ("o a" . org-agenda)
        ("o [" . spw/org-agenda-file-to-front)
        ("o ]" . spw/org-remove-file)

        ;; launcher map
        ("g k" . kill-emacs)
        ("g c" . spw/manual-cleanup)
        ("g l" . spw/tblesson)
        ("g r" . (lambda ()
                   (interactive)
                   (projectile-persp-switch-project "~/src/dotfiles")
                   (find-file "~/src/dotfiles/.emacs.d/init.el")
                   (eval-buffer)))

        ("g t l" . world-time-list)
        ("g t h" . helm-world-time)
        ("g t t" . display-time-world)

        ;; Sariul launcher map
        ("S l" . spw/tblesson)
        ("S S" . spw/auto-textbook)
        ("S s" . spw/textbook)
        ("S t" . spw/auto-teachers-book)

        ;; toggle map
        ("t e" . toggle-debug-on-error)
        ("t i" . org-indent-mode)
        ("t w" . spw/writing-toggle)

        ;; insertion map
        ("i h" . add-file-local-variable-prop-line)

        ;; evaluation map
        ("e e" . spw/eval-surrounding-sexp)
        ("e f" . eval-defun)
        ("e r" . eval-region)
        ("e b" . eval-buffer)
        ("E" . prelude/eval-and-replace)))

(defun spw/personal-bindings ()
  (interactive)
  (dolist (pair spw/personal-bindings)
    (let ((key (car pair))
          (bindee (cdr pair)))
      (spw/C-c-bind key bindee))))

(spw/personal-bindings)

;;; abbreviations

(setq abbrev-file-name "~/doc/emacs-abbrevs")

;; turn on for all buffers, if our abbrevs file is checked out
(if (file-exists-p abbrev-file-name)
    (progn
      (setq save-abbrevs t)
      (setq-default abbrev-mode t)
      (diminish 'abbrev-mode)))

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
  (defadvice bookmark-write-file (before save-bookmarks-buffer activate)
    (if (get-buffer "emacs-bookmarks")
        (with-current-buffer (get-buffer "emacs-bookmarks")
          (save-buffer))))
  (defadvice kill-buffer (before kill-buffer-clear-modified activate)
    (if (get-buffer "emacs-bookmarks")
        (with-current-buffer (get-buffer "emacs-bookmarks")
          (set-buffer-modified-p nil)))))

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
;(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

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

(defun spw/save-org-buffers-first (&optional arg pred)
  "Save all Org buffers without prompting.

ARG, PRED ignored."
  (when (featurep 'org)
    ;; gotta remove ourselves since `org-save-all-org-buffers' calls
    ;; `save-some-buffers'
    (advice-remove 'save-some-buffers #'spw/save-org-buffers-first)
    (org-save-all-org-buffers)
    (advice-add 'save-some-buffers :before #'spw/save-org-buffers-first)))

;; no `advice-add' on Emcas 24.3 on ma
(if (fboundp 'advice-add)
    (advice-add 'save-some-buffers :before #'spw/save-org-buffers-first))



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
  :init (progn
          (defadvice message-newline-and-reformat (before spw/delete-superflous-newlines activate)
            "Have `message-newline-and-reformat' get rid of some more superflous blank quoted lines."
            (save-excursion
              (beginning-of-line)
              (when (looking-at ">[[:space:]]*$")
                (kill-line 1))))
          (setq mail-header-separator "")
          (add-hook 'message-mode-hook (lambda ()
                                         (auto-fill-mode)
                                         (spw/set-from-address)
                                         (footnote-mode)
                                         (message-goto-body)))))

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
(add-hook 'text-mode 'refill-mode)

;;; dired

(use-package dired-x)
(setq-default dired-omit-mode t)
(setq dired-omit-files "^\\...+$")
(setq dired-isearch-filenames 'dwim)

;; dired omit mode mapping conflicts with my ace jump mode
;; binding
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

;;; javascript

;; don't insert a newline after a semicolon
(add-hook 'js-mode-hook (lambda ()
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

(provide 'init)
;;; init.el ends here
