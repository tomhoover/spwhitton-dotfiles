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

(defconst user-custom-file (concat user-emacs-directory "init-custom.el"))
(load user-custom-file 'noerror)

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

(use-package f :ensure) (use-package s :ensure)

;;; instead of vim text objects

(use-package expand-region
  :ensure
  :bind ("M-i" . er/expand-region)
  :init (setq expand-region-contract-fast-key (kbd "o")))

;;; keep parentheses under control

(use-package smartparens
  :ensure
  :commands (smartparens-strict-mode)
  :init (dolist (hook '(emacs-lisp-mode-hook
                        lisp-mode-hook
                        lisp-interaction-mode-hook
                        ielm-mode-hook
                        scheme-mode-hook
                        inferior-scheme-mode-hook
                        python-mode-hook))
          (add-hook hook
                    (lambda ()
                      (smartparens-strict-mode))))
  :idle (show-smartparens-global-mode)
  :config (progn
            (require 'smartparens-config)
            (setq sp-navigate-consider-symbols t)
            (sp-use-smartparens-bindings)
            (bind-key "C-w" 'sp-backward-kill-word emacs-lisp-mode-map)
            (bind-key "C-k" 'sp-kill-hybrid-sexp emacs-lisp-mode-map)))

;;; Org

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org" . org-mode)
         ("\\.org_archive" . org-mode))
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda))
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

;;; fix up whitespace around kill and yanking

(use-package smart-whitespace-comment-fixup :ensure
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
              (define-key (current-local-map) (kbd "TAB") 'company-complete))
            (add-hook 'prog-mode-hook 'spw/company-prog-setup)

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
  :commands deft
  :init (setq deft-extension "org"
              deft-text-mode 'org-mode
              deft-directory "~/doc/org/"
              deft-use-filename-as-title nil
              deft-auto-save-interval 20.0
              deft-incremental-search nil
              ;; don't just strip the leading hash but the whole #+TITLE:
              deft-strip-title-regexp "\\(?:\\#\\+TITLE\\: \\|\\#\\+FILETAGS\\: \\|^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)")
  :config (progn
            (bind-key "C-w" 'deft-filter-decrement-word deft-mode-map)
            ;; (bind-key "C-h" 'deft-filter-decrement deft-mode-map)

            (defadvice deft (before persp-deft activate)
              (projectile-persp-switch-project "~/doc"))))

;;; allow lisp to interact with python

(use-package pymacs :ensure)

;;; Get Python documentation as info files

(use-package python-info :ensure)
(use-package pydoc-info :ensure)

;;; flycheck

(use-package flycheck :ensure :idle (global-flycheck-mode))

;;; TRAMP

(use-package tramp
  :config (progn
            (add-to-list 'tramp-default-user-alist '(nil "sdf" "spw"))
            (add-to-list 'tramp-default-user-alist '("sudo" "localhost" "root"))
            (add-to-list 'tramp-default-user-alist '(nil nil "swhitton") t)
            (add-to-list 'tramp-default-user-alist '(nil "ma" "spw"))

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
  :bind (("C-c l" . persp-toggle)
         ("C-c L" . persp-switch))
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
              (persp-switch (persp-name persp-last))))

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
          (add-hook 'persp-created-hook 'persp-basewc-save)
          (defun persp-basewc-restore ()
            (interactive)
            (let* ((name (persp-name persp-curr))
                   (pair (assoc name persp-basewcs))
                   (wc (cdr pair)))
              (set-window-configuration wc)))))

;;; powerful completion everywhere, especially for jumping between files

(use-package helm
  :ensure
  :bind ("C-x C-f" . helm-find-files)
  :diminish helm-mode
  :init (progn
          (require 'helm-config)

          ;; extra sources
          (use-package helm-projectile :ensure)
          (use-package helm-dired-recent-dirs :ensure)
          (use-package imenu-anywhere :ensure)

          ;; new fuzzy matching and other settings
          (setq helm-buffers-fuzzy-matching t
                helm-time-zone-home-location "Seoul"
                helm-quick-update t
                helm-split-window-in-side-p t
                helm-ff-search-library-in-sexp t
                helm-ff-file-name-history-use-recentf t
                helm-ff-newfile-prompt-p nil)

          (helm-mode)

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

              (add-to-ordered-list 'sources 'helm-source-buffers-list 1)
              (add-to-ordered-list 'sources 'helm-source-imenu-anywhere 3)
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
                (add-to-ordered-list 'sources 'helm-source-projectile-files-list 4)
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
            :ensure
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
  :init (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode))

;;; jump around what's visible

(use-package ace-jump-mode
  :ensure
  :bind ("M-o" . ace-jump-mode))

;;; use ace-jump-mode to move between links in help file

(use-package ace-link
  :ensure
  :idle (ace-link-setup-default))

;;; chat on Jabber

(use-package jabber
  :ensure
  :config (when (f-exists? "~/.emacs.d/init-jabber.el")
            (load-file "~/.emacs.d/init-jabber.el")
            (jabber-connect-all)))

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
  (unless (eq system-type 'windows-nt)
    (centered-window-mode 1)
    ;; indent mode need only be turned off if we're using centered
    ;; window mode
    (when (eq major-mode 'org-mode)
      (org-indent-mode 0)))
  (if (eq system-type 'windows-nt) (spw/centre-window nil)))

(defun spw/writing-off ()
  "Deactivate my prose writing features."
  (wc-mode 0)
  (variable-pitch-mode 0)
  (unless (eq system-type 'windows-nt)
    (centered-window-mode 0)
    ;; indent mode need only be turned off if we're using centered
    ;; window mode
    (when (eq major-mode 'org-mode)
      ;; TODO: finesse this.  don't turn it on if it wouldn't be on by default
      (org-indent-mode 1)))
  (if (eq system-type 'windows-nt) (delete-other-windows)))

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
  (unless aggressive-indent-mode
    (indent-region (point-min) (point-max)))
  (case major-mode
    (emacs-lisp-mode
     (spw/clean-lisp-dangling-brackets))))

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

(bind-key "S-SPC" 'spw/toggle-language-environment)

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

(defun magnars/smarter-move-beginning-of-line (arg)
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
              (helm-switch-to-buffer correct-eshell-name)
            (eshell "a")
            (rename-buffer correct-eshell-name))
        ;; we're not in a project: don't
        (if (get-buffer "*eshell*")
            (helm-switch-to-buffer "*eshell*")
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

(defun tblesson (grade lesson period)
  "Start a textbook-based lesson plan for grade GRADE, lesson LESSON, period PERIOD."
  (interactive "sGrade: \nsGrade %s, lesson: \nsGrade %s, lesson %s, period: ")
  (projectile-persp-switch-project "~/Documents/Teaching")
  (let* ((parent (f-join "~/Documents/Teaching" grade lesson))
         (path (f-join "~/Documents/Teaching" grade lesson period))
         (name (f-join path (concat grade "-" lesson "-" period ".org"))))
    (when (not (f-exists? name))
      (f-mkdir parent)
      (f-mkdir path))
    (find-file name)
    (when (not (f-exists? name))
      (insert "tblesson")
      (if evil-mode (evil-append-line 1))
      (yas-expand))))

;;;; ---- personal settings ----

;;; no tabs please

(setq-default indent-tabs-mode nil)

;;; key bindings

;; I don't often want to quit
(bind-key "C-x C-c" 'delete-frame)

;; opening new lines below
(bind-key "M-RET" 'magnars/new-line-dwim)

;; C-m and RET should reindent the current line only for languages
;; that don't use semantic indentation
(bind-key "RET" 'reindent-then-newline-and-indent)
(add-hook 'python-mode-hook (lambda ()
                              (bind-key "RET" 'newline-and-indent python-mode-map)))
(add-hook 'haskell-mode-hook (lambda ()
                               (bind-key "RET" 'newline-and-indent haskell-mode-map)))

;; fixup-whitespace seems to make just-one-space redundant
(bind-key "M-SPC" 'fixup-whitespace)

;; never want to send any e-mail
(unbind-key "C-x m")

;; fallback expanding
(bind-key "M-/" 'hippie-expand)

;; remap C-a to `magnars/smarter-move-beginning-of-line'
(bind-key "C-a" 'magnars/smarter-move-beginning-of-line)

;;; bind all up into the C-c keymap

(defmacro spw/C-c-bind (key bindee)
  "Bind BINDEE to C-c KEY with `bind-key' macro.

BINDEE may be a command or another keymap, but whatever it is, it should not be quoted."
  `(if (keymapp ,bindee)
       (bind-key (concat "C-c " ,key) ,bindee)
     (bind-key (concat "C-c " ,key) bindee)))
(dolist (pair '(("p" . projectile-command-map)
                ("j" . spw/helm-mini)
                ("v" . projectile-vc)
                ("n" . mwf/narrow-or-widen-dwim)
                ("gk" . kill-emacs)
                ("s" . spw/persp-eshell)
                ("d" . spw/dired-jump)
                ("ih" . add-file-local-variable-prop-line)))
  (let ((key (car pair))
        (bindee (cdr pair)))
    (spw/C-c-bind key bindee)))

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
(setq global-auto-revert-mode t)

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
(auto-insert-mode 1)

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

;; return to Emacs 23 selection/clipboard behaviour
(setq select-active-regions nil
      mouse-drag-copy-region t
      x-select-enable-primary t
      x-select-enable-clipboard t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

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
          (setq mail-header-separator "")
          (add-hook 'message-mode-hook (lambda ()
                                         (auto-fill-mode)
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

(provide 'init)
;;; init.el ends here
