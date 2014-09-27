;; when --debug-init isn't enough
;;(setq debug-on-error t
;;      debug-on-quit t)

;;;; ---- package management ----

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'use-package)
(require 'package)
(require 'package-filter)
(setq
 package-user-dir "~/local/src/elpa"
 package-archives
 '(("melpa" . "http://melpa.milkbox.net/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("org" . "http://orgmode.org/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/"))
 package-archive-exclude-alist
 '(("melpa" org)
   ("marmalade" org)
   ("gnu" org)
   ("org" org)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-enable-at-startup nil)

;;;; ---- basic settings ----

;;; customisation -- must be loaded early so that zenburn theme is
;;; considered safe

(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file 'noerror)

;;; From the tmux FAQ: should allow some modified keys to pass through
;;; to Emacs

(defadvice terminal-init-screen
  ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
  (before tmux activate)
  ;; Docstring.  This describes the advice and is made available inside emacs;
  ;; for example when doing C-h f terminal-init-screen RET
  "Apply xterm keymap, allowing use of keys passed through tmux."
  ;; This is the elisp code that is run before `terminal-init-screen'.
  (if (getenv "TMUX")
      (let ((map (copy-keymap xterm-function-map)))
        (set-keymap-parent map (keymap-parent input-decode-map))
            (set-keymap-parent input-decode-map map))))

;;; From the Emacswiki: should allow some more modified keys to work
;;; in Emacs over ssh via PuTTY

(if (eq system-uses-terminfo t)
    (progn                              ;; PuTTY hack - needs to be in SCO mode
      (define-key key-translation-map [\e] [\M])
      (define-key input-decode-map "\e[H" [home])
      (define-key input-decode-map "\e[F" [end])
      (define-key input-decode-map "\e[D" [S-left])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[A" [S-up])
      (define-key input-decode-map "\e[B" [S-down])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[I" [prior])
      (define-key input-decode-map "\e[G" [next])
      (define-key input-decode-map "\e[M" [f1])
      (define-key input-decode-map "\e[Y" [S-f1])
      (define-key input-decode-map "\e[k" [C-f1])
      (define-key input-decode-map "\e\e[M" [M-f1])
      (define-key input-decode-map "\e[N" [f2])
      (define-key input-decode-map "\e[Z" [S-f2])
      (define-key input-decode-map "\e[l" [C-f2])
      (define-key input-decode-map "\e\e[N" [M-f2])
      (define-key input-decode-map "\e[O" [f3])
      (define-key input-decode-map "\e[a" [S-f3])
      (define-key input-decode-map "\e[m" [C-f3])
      (define-key input-decode-map "\e\e[O" [M-f3])
      (define-key input-decode-map "\e[P" [f4])
      (define-key input-decode-map "\e[b" [S-f4])
      (define-key input-decode-map "\e[n" [C-f4])
      (define-key input-decode-map "\e\e[P" [M-f4])
      (define-key input-decode-map "\e[Q" [f5])
      (define-key input-decode-map "\e[c" [S-f5])
      (define-key input-decode-map "\e[o" [C-f5])
      (define-key input-decode-map "\e\e[Q" [M-f5])
      (define-key input-decode-map "\e[R" [f6])
      (define-key input-decode-map "\e[d" [S-f6])
      (define-key input-decode-map "\e[p" [C-f6])
      (define-key input-decode-map "\e\e[R" [M-f6])
      (define-key input-decode-map "\e[S" [f7])
      (define-key input-decode-map "\e[e" [S-f7])
      (define-key input-decode-map "\e[q" [C-f7])
      (define-key input-decode-map "\e\e[S" [M-f7])
      (define-key input-decode-map "\e[T" [f8])
      (define-key input-decode-map "\e[f" [S-f8])
      (define-key input-decode-map "\e[r" [C-f8])
      (define-key input-decode-map "\e\e[T" [M-f8])
      (define-key input-decode-map "\e[U" [f9])
      (define-key input-decode-map "\e[g" [S-f9])
      (define-key input-decode-map "\e[s" [C-f9])
      (define-key input-decode-map "\e\e[U" [M-f9])
      (define-key input-decode-map "\e[V" [f10])
      (define-key input-decode-map "\e[h" [S-f10])
      (define-key input-decode-map "\e[_" [C-f10])
      (define-key input-decode-map "\e\e[V" [M-f10])
      (define-key input-decode-map "\e[W" [f11])
      (define-key input-decode-map "\e[i" [S-f11])
      (define-key input-decode-map "\e[u" [C-f11])
      (define-key input-decode-map "\e\e[W" [M-f11])
      (define-key input-decode-map "\e[X" [f12])
      (define-key input-decode-map "\e[j" [S-f12])
      (define-key input-decode-map "\e[v" [C-f12])
      (define-key input-decode-map "\e\e[X" [M-f12])))

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

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; don't handle keyboard events before redrawing
(setq redisplay-dont-pause t)

;; show trailing whitespace ...
(setq-default show-trailing-whitespace t)
;; ... but not in terminals
(add-hook 'term-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)
                            (goto-address-mode)))
(add-hook 'eshell-mode-hook (lambda ()
                              (setq show-trailing-whitespace nil)))

;; don't prompt to create scratch buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; initial frame size
(if window-system (progn (set-frame-width (selected-frame) 80)
                         (set-frame-height (selected-frame) ; fails atm
                                           (round
                                            (/ (- (x-display-pixel-height) 28)
                                                       (frame-char-height))))))

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

;;; cursor settings

(setq x-stretch-cursor t)
(setq-default cursor-type 'box)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0)) ; turns off blink-cursor-mode if it ended up on

;; get the mouse out of the way
(mouse-avoidance-mode 'cat-and-mouse)

;;; zenburn

(use-package zenburn-theme :ensure)
(load-theme 'zenburn)

;;; I'm in Korea

(if (not (eq system-type 'windows-nt))
    (set-time-zone-rule "/usr/share/zoneinfo/Asia/Seoul"))

;;; be sure to start the server

(server-start)

;; bind C-x k to end edit (C-x C-c seems to be already bound to this)
;; (add-hook 'server-switch-hook
;;           (lambda ()
;;             (when (current-local-map)
;;               (use-local-map (copy-keymap (current-local-map))))
;;             (when server-buffer-clients
;;               (local-set-key (kbd "C-x k") 'server-edit))))

;;;; ---- packages ----

(use-package diminish :ensure)

;;; Org

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org" . org-mode)
         ("\\.org_archive" . org-mode))
  :bind (("C-M-c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :diminish org-indent-mode
  :config (load "~/.emacs.d/init-org.el"))

;;; keep pop up windows under control

(use-package popwin
  :ensure
  :commands popwin-mode
  :idle (popwin-mode 1))

;;; modern replacement for the mighty paredit

(use-package smartparens
  :ensure
  :commands (smartparens-global-strict-mode show-smartparens-global-mode)
  :bind (("C-w" . kill-region-or-backward-word)
         ("M-d" . sp-kill-word)         ; ideally these would delete, not kill

         ;; for when I use Emacs via PuTTY
         ("M-<right>" . sp-forward-slurp-sexp)
         ("M-<left>" . sp-forward-barf-sexp)

         ("M-j" . sp-join-sexp))
  :idle (progn
          ;; non-strict mode the default, and strict mode in some
          ;; programming language major modes
          (smartparens-global-mode)
          (dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                python-mode-hook))
            (add-hook hook
            (lambda ()
              (smartparens-strict-mode))))
          (show-smartparens-global-mode))
  ;; :diminish smartparens-mode
  :config (progn
            (require 'smartparens-config)
            (setq sp-navigate-consider-symbols t)
            (sp-use-smartparens-bindings)

            ;; override smartparens binding for C-k outside of lisp,
            ;; since sp-kill-hybrid-sexp isn't very smart in comint
            ;; and I like using C-u C-k
            (define-key smartparens-strict-mode-map [remap kill-line] 'kill-line)
            (bind-key "C-k" 'sp-kill-hybrid-sexp emacs-lisp-mode-map)

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
            (sp-local-pair 'org-mode "~" "~") ; code
            ;; (sp-local-pair 'org-mode "+" "+")
            ;; (sp-local-pair 'org-mode "_" "_")
            ))

;;; save my places in buffers.  ido and recentf save recently opened
;;; files, and these two things together are enough session management
;;; for me

(setq recentf-save-file "~/.emacs.d/recentf")

;; the three hooks added by the idle progn below don't stay set when
;; set by (require 'saveplace), nor do they remain in place if simply
;; added in this config file or even in 'after-init-hook.  So have
;; use-package add them a few seconds after Emacs starts

(use-package saveplace
  :init (progn
          (setq-default save-place t)
          (setq save-place-file "~/.emacs.d/saveplace"))
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

;;; bbdb

(use-package bbdb
  :ensure
  :commands bbdb
  :config (progn
            (setq bbdb-complete-name-full-completion t
                  bbdb-completion-type 'primary-or-name
                  bbdb-complete-name-allow-cycling t
                  bbdb-dwim-net-address-allow-redundancy t
                  bbdb-offer-save 1
                  bbdb-use-pop-up t
                  bbdb-electric-p t
                  bbdb-popup-target-lines  1
                  bbdb-file "~/.bbdb")
            (bbdb-initialize 'message)))

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
            (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)

            (use-package magit-annex :ensure)

            (use-package magit-wip
              :diminish magit-wip-save-mode
              :config (global-magit-wip-save-mode 1))))

;;; winner mode

(use-package winner-mode
  :disabled t
  :commands winner-mode
  :idle (winner-mode 1))

;;; better window switching

(use-package switch-window
  :ensure
  ;; ace-jump-mode is better
  :disabled t
  :bind ("C-x o" . switch-window))

;;; pointback mode

(use-package pointback
  :ensure
  :commands pointback-mode
  :idle (pointback-mode))

;;; Ido

(use-package ido
  :ensure
  :init (progn
          (setq ido-enable-flex-matching t
                ido-everywhere t
                ido-use-filename-at-point 'guess
                ido-create-new-buffer 'always
                ido-file-extensions-order '(".org" ".tex" ".py" )
                ido-default-file-method 'selected-window
                ido-max-directory-size 100000
                ido-auto-merge-delay-time 99999 ; only search when I tell you to M-s
                ido-use-virtual-buffers t
                ido-use-virtual-buffers-automatically t
                ido-enable-regexp nil
                ido-use-url-at-point nil
                ido-max-file-prompt-width 0.1
                ido-save-directory-list-file "~/.emacs.d/ido.last")
          (add-hook 'ido-setup-hook (lambda ()
                                      (define-key ido-completion-map "\C-w" 'ido-delete-backward-word-updir)
                                      (define-key ido-completion-map " \C-h" 'ido-delete-backward-updir)))

          (use-package flx-ido
            :ensure
            :init (flx-ido-mode 1)
            :config (progn
                      (setq ido-use-faces nil
                            flx-ido-threshhold 7500
                            gc-cons-threshold 20000000)))
          (use-package ido-ubiquitous
            :ensure
            :init (ido-ubiquitous))
          (use-package ido-vertical-mode
            :ensure
            :init (ido-vertical-mode 1))

          (ido-mode 1)))

;;; use ido to jump to imenu tags in all open files of the same mode

(use-package imenu-anywhere
  :ensure
  ;; :bind ("M-o" . imenu-anywhere)
  )

;;; ibuffer

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (progn
            (add-hook 'ibuffer-hook
                      (lambda ()
                        (ibuffer-vc-set-filter-groups-by-vc-root)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))))

(use-package ibuffer-vc
  :ensure
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :config (setq ibuffer-formats
                '((mark modified read-only vc-status-mini " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        (vc-status 16 16 :left)
                        " "
                        filename-and-process))))

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

;;; ElDoc and rainbow delimiters activation

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (turn-on-eldoc-mode)
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

(use-package wc-mode
  :ensure
  :bind ("C-c w" . wc-mode))

;;; company-mode for smart and easy completion

(use-package company
  :ensure
  :commands global-company-mode
  :idle (global-company-mode)
  :diminish company-mode
  :config (progn
            ;; I like my C-w binding so move one of company's bindings
            (define-key company-active-map "\C-w" nil)
            (define-key company-active-map "\C-j" 'company-show-location)

            (setq company-idle-delay 0.15)
            (add-to-list 'company-backends 'company-capf)
            (add-to-list 'company-transformers 'company-sort-by-occurrence)

            ;;; python code completion

            (use-package anaconda-mode
              :ensure
              :config (progn
                        (add-hook 'python-mode-hook 'anaconda-mode)
                        (add-to-list 'company-backends 'company-anaconda)
                        (add-hook 'python-mode-hook 'anaconda-eldoc)))

            ;; company completion in eshell buffers gums up TRAMP
            (add-hook 'eshell-mode-hook (lambda ()
                                          (company-mode 0)))
            ;; company in Org-mode more often annoying than useful
            (add-hook 'org-mode-hook (lambda ()
                                          (company-mode 0)))))
;; C-o during company isearch narrows to stuff matching that search;
;; mnemonic 'occur'.  C-M-s while outside of search to do the same
;; thing

;;; auto indent mode inc. smart yanking

(use-package auto-indent-mode :ensure
  :disabled t)

;;; smart tabs - tabs AND spaces (but just spaces by default)

(setq-default indent-tabs-mode nil)
(use-package smart-tabs-mode
  :ensure
  :disabled t
  :commands smart-tabs-insinuate
  :idle (smart-tabs-insinuate 'c 'javascript))

;;; guess a file's indentation style

(use-package dtrt-indent
  :ensure
  :disabled t
  :commands dtrt-indent-mode
  :idle (dtrt-indent-mode 1))

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
  :bind ("<f9>" . deft)
  :init (progn
          (setq deft-extension "org"
                deft-text-mode 'org-mode
                deft-directory "~/doc/org/"
                deft-use-filename-as-title nil
                deft-auto-save-interval 20.0
                deft-incremental-search nil

                ;; don't just strip the leading hash but the whole #+TITLE:
                deft-strip-title-regexp "\\(?:\\#\\+TITLE\\: \\|\\#\\+FILETAGS\\: \\|^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"))
  :config (progn
            (bind-key "C-w" 'deft-filter-decrement-word deft-mode-map)
            (bind-key "C-h" 'deft-filter-decrement deft-mode-map)))

;;; fast region expanding

(use-package expand-region
  :ensure
  :disabled t
  :bind ("M-m" . er/expand-region)
  :init (progn
          (defun er/add-text-mode-expansions ()
            (make-variable-buffer-local 'er/try-expand-list)
            (setq er/try-expand-list (append
                                      er/try-expand-list
                                      '(mark-paragraph
                                        mark-page))))

          (add-hook 'text-mode-hook 'er/add-text-mode-expansions)))

;;; allow lisp to interact with python

(use-package pymacs :ensure)

;;; Get Python documentation as info files

(use-package python-info :ensure)
(use-package pydoc-info :ensure)

;;; fix binding in python-mode

(use-package python
  :config (define-key python-mode-map (kbd "C-h") 'python-indent-dedent-line-backspace))

;;; flycheck

(use-package flycheck :ensure :idle (global-flycheck-mode))

;;; edit minibuffer in a proper buffer

(use-package miniedit
  :ensure
  :commands miniedit
  :init (progn
          (define-key minibuffer-local-map "\M-\C-e" 'miniedit)
          (define-key minibuffer-local-ns-map "\M-\C-e" 'miniedit)
          (define-key minibuffer-local-completion-map "\M-\C-e" 'miniedit)
          (define-key minibuffer-local-must-match-map "\M-\C-e" 'miniedit)))

;;; smex

(use-package smex
  :ensure
  :disabled t
  :commands smex-initialize
  :bind (("C-x C-m" . smex)
         ("C-x C-," . smex-major-mode-commands))
  :idle (smex-initialize)
  :config (progn
            (setq smex-save-file "~/.emacs.d/smex-items")

            ;; restore space bar for hyphens (should be faster)
            (defadvice smex (around space-inserts-hyphen activate compile)
              (let ((ido-cannot-complete-command
                     `(lambda ()
                        (interactive)
                        (if (string= " " (this-command-keys))
                            (insert ?-)
                          (funcall ,ido-cannot-complete-command)))))
                ad-do-it))))

;;; anchored transpose

(use-package anchored-transpose
  :bind ("C-x t" . anchored-transpose))

;;; let's make it possible to toggle editing as root
;;; http://atomized.org/2011/01/toggle-between-root-non-root-in-emacs-with-tramp/

(use-package toggle-root
  :bind ("C-c C-M-x C-M-q" . toggle-alternate-file-as-root))

;;; TRAMP

(use-package tramp
  :config (progn
            (setq tramp-default-method "scp")

            (add-to-list 'tramp-default-user-alist '(nil "athena" "swhitton"))
            (add-to-list 'tramp-default-user-alist '(nil "sdf" "spw"))
            (add-to-list 'tramp-default-user-alist '("sudo" "localhost" "root"))
            (add-to-list 'tramp-default-user-alist '(nil nil "swhitton") t)
            (add-to-list 'tramp-default-user-alist '(nil "ma" "spw"))

            ;; from the TRAMP manual: For all hosts except my local one connect via
            ;; ssh first, and apply sudo -u root afterwards
            (add-to-list 'tramp-default-proxies-alist
                         '(nil "\\`root\\'" "/scpc:%h:"))
            (add-to-list 'tramp-default-proxies-alist
                         '("localhost" nil nil))

            (setq tramp-verbose 0)

            ;; TRAMP and zsh are not friends so might as well switch
            ;; over here
            (setenv "SHELL" "/bin/bash")))

;;; ebib for editing BiBTeX databases

(use-package ebib
  :ensure
  :init (setq ebib-preload-bib-files '("~/doc/spw.bib"))
  :commands ebib)

;;; dired enhancements

(use-package dired-details
  :ensure
  :init (setq dired-recursive-deletes 'always
              dired-recursive-copies 'always
              dired-dwim-target t)
  :idle (dired-details-install))

(use-package dired-sort-map
  :init (setq dired-listing-switches "--group-directories-first -alh"))

(use-package git-annex :ensure)

;;; easily run the odd shell command in a real shell

(use-package emamux
  :disabled t
  :ensure
  :bind (("C-c t t" . emamux:run-command)
         ("C-c t z" . emamux:zoom-runner)
         ("C-c t [" . emamux:inspect-runner)
         ("C-c t ]" . emamux:copy-kill-ring)
         ("C-c t k" . emamux:close-runner-pane)))

;;; loads of visual help for working with Emacs regexps

(use-package visual-regexp
  :ensure
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;;; simple projects

(use-package projectile
  :ensure
  :bind ("C-c g" .  projectile-vc)
  :idle (projectile-global-mode))

;;; move around my commonly used stuff fast

(use-package helm
  :ensure
  :bind (("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-m" . helm-M-x))
  :diminish helm-mode
  :init (progn
          ;; autoloads aren't enough to make the above bindings work
          (require 'helm-config)

          ;; extra sources
          (use-package helm-projectile :ensure)
          (use-package helm-dired-recent-dirs :ensure)

          ;; new fuzzy matching
          (setq helm-buffers-fuzzy-matching t
                helm-time-zone-home-location "Seoul"
                helm-quick-update t
                helm-split-window-in-side-p t
                helm-ff-search-library-in-sexp t
                helm-ff-file-name-history-use-recentf t
                helm-ff-newfile-prompt-p nil)

          ;; rebind some keys
          (bind-key "C-w" 'backward-delete-word helm-map)
          (bind-key "C-h" 'backward-delete-char helm-map)
          (bind-key "C-h" 'backward-delete-char helm-find-files-map)
          (bind-key "C-o" 'helm-select-action helm-map)
          (bind-key "M-i" 'helm-next-source helm-map)

          ;; swap <tab> and C-z in helm since use persistent action
          ;; much more frequently
          (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
          (bind-key "C-i" 'helm-execute-persistent-action helm-map)
          (bind-key "C-z" 'helm-select-action helm-map)

          ;; eshell history -- let Helm handle eshell completion
          (add-hook 'eshell-mode-hook
                    #'(lambda ()
                        (define-key eshell-mode-map [remap yas-expand] 'helm-esh-pcomplete)))

          ;; helm-mini shouldn't stop working just because we're not
          ;; in a projectile project.  And it's way too slow to use
          ;; from an eshell buffer that's TRAMP'd to a remote host
          (defadvice helm-mini (around spw/helm-mini activate)
            "Remove projectile stuff if not in a project, unless it's my massive annex"
            (if (equal major-mode 'eshell-mode)
                (ido-switch-buffer)
              (if (and (projectile-project-p)
                       (not (equal (projectile-project-name) "annex")))
                  (setq helm-mini-default-sources '(helm-source-buffers-list
                                                    ;; this one is pointless since everything in it is contained in the one above.  Only useful if they are switched over but I don't think it's worth the processing time
                                                    ;; helm-source-projectile-buffers-list
                                                    helm-source-ido-virtual-buffers
                                                    helm-source-projectile-files-list
                                                    helm-source-imenu-anywhere
                                                    helm-source-bookmarks
                                                    helm-source-dired-recent-dirs
                                                    helm-source-recentf
                                                    helm-source-buffer-not-found))
                (setq helm-mini-default-sources '(helm-source-buffers-list
                                                  helm-source-ido-virtual-buffers
                                                  helm-source-imenu-anywhere
                                                  helm-source-bookmarks
                                                  helm-source-dired-recent-dirs
                                                  helm-source-recentf)))
              ad-do-it)

            ;; once Org is loaded, can add Org headline source
            (if (featurep 'org)
                (add-to-list 'helm-mini-default-sources 'helm-source-org-headline t))

            (helm-adaptative-mode))

          (use-package helm-descbinds
            :ensure
            :idle (helm-descbinds-mode))

          (helm-mode)))

(use-package yasnippet
  :ensure
  :diminish yas-global-mode
  :idle (yas-global-mode))

;;; eshell's plan9-style smart shell

(use-package em-smart
  :init (progn
          (setq eshell-where-to-jump 'begin
                eshell-review-quick-commands nil
                eshell-smart-space-goes-to-end t)))

;;; easily switch between eshells

(use-package shell-switcher
  :ensure
  :init (progn (setq shell-switcher-mode t
                     shell-switcher-ask-before-creating-new nil)

               (define-key shell-switcher-mode-map (kbd "C-c s")
                 'shell-switcher-switch-buffer)
               (define-key shell-switcher-mode-map (kbd "C-c 4 s")
                 'shell-switcher-switch-buffer-other-window)

               (defadvice shell-switcher-switch-buffer (around shell-switcher-switch-buffer-add-arg (arg) activate)
                 "Use the universal argument to create new
                 shells, rather than binding another key
                 to (shell-switcher-new-shell)"
                 (interactive "P")
                 (if arg (shell-switcher-new-shell) ad-do-it))))

;;; htmlize for Org HTML export/publishing

(use-package htmlize :ensure)

;;; close old buffers once per day

(use-package midnight
  :init (progn
          (midnight-delay-set 'midnight-delay "3am")))

;;; electric spacing around mathematical operators

(use-package smart-operator
  ;; :ensure                               ; MELPA version six years out of date
  :init (progn
          ;; I've trained my fingers well to double-space manually
          (setq smart-operator-double-space-docs nil)

          (add-hook 'python-mode-hook 'smart-operator-mode)))

;;; make indentation in python nice and visible

(use-package highlight-indentation
  :ensure
  :init (progn
          (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)))

;;; jump around what's visible

(use-package ace-jump-mode
  :ensure
  :bind ("M-o" . ace-jump-mode))

(use-package jump-char
  :ensure
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

(use-package ace-link
  :ensure
  :idle (ace-link-setup-default))

(use-package pomodoro)

(use-package flex-isearch
  :idle (flex-isearch-mode)
  :config (setq flex-isearch-auto t))

(use-package expand-region
  :ensure
  :bind ("M-r" . er/expand-region))

(use-package change-inner
  :ensure
  :bind (("M-I" . change-inner)
         ("M-O" . change-outer)
         ;; ("M-I" . copy-inner)
         ;; ("C-O" . copy-outer)
         ))

(use-package god-mode :ensure)

(use-package key-chord
  :ensure
  :init (progn
          (key-chord-define-global "jk" 'god-mode-all) 
          (key-chord-mode 1)))

;;;; ---- functions ----

;; backwards and forward deletions of words

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; disabled in favour of smartparens versions, though these are delete
;; and the ones I'm currently using are kill
;;(global-set-key "\C-w" 'backward-delete-word)
;;(global-set-key "\M-d" 'delete-word)

;;; my buffer save cleanup functions

;; http://stackoverflow.com/questions/3533703/emacs-delete-trailing-whitespace-except-current-line
(defun delete-trailing-whitespace-except-current-line ()
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

(defun compact-blank-lines ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\n\n\n+" nil "noerror")
      (replace-match "\n\n"))))

(defun spw/cleanup ()
  (interactive)
  (case major-mode
    (haskell-mode
     (delete-trailing-whitespace-except-current-line)
     (compact-blank-lines))
    (python-mode
     (delete-trailing-whitespace-except-current-line)
     (compact-blank-lines))
    (message-mode
     (whitespace-cleanup))
    (emacs-lisp-mode
     (delete-trailing-whitespace-except-current-line))))

(add-hook 'before-save-hook 'spw/cleanup)

;;; Typing Hangul

(defun my-korean-setup ()
  "Set up my Korean environment."
  (if (equal current-language-environment "Korean")
    (set-input-method "korean-hangul")))

(defun my-english-setup ()
  "Set up my English environment."
  (if (equal current-language-environment "English")
    (set-input-method nil)))

(add-hook 'set-language-environment-hook 'my-korean-setup)
(add-hook 'set-language-environment-hook 'my-english-setup)

(defun my-toggle-lang-env ()
  (interactive)
  (set-language-environment
   (if (equal current-language-environment "English")
       "Korean" "English")))

;;; centralise window for easier viewing

(defun swhitton/centralise-current-window (arg)
  "Make editing window 95 cols wide and centre it in the frame
for easier reading and writing"
  (interactive "P")
  (delete-other-windows)
  (split-window-horizontally)
  (if arg
      (split-window-horizontally))
  (shrink-window-horizontally (- (window-width) (/ (- (frame-width) 97) 2)))
  (switch-to-buffer "*blank*")
  (toggle-read-only 1)
  (setq mode-line-format nil)
  (other-window 1)
  (if arg (progn
              (shrink-window-horizontally (- (window-width) 95))
              (other-window 1)
              (switch-to-buffer "*blank*")
              (other-window -1)))
  )

;;; run my DnD helper script in an appropriately sized window

(defun spwd20 ()
  (interactive)
  (find-file "~/doc/org/pathfinder2013.org")
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (shrink-window (- (window-height) 6))
  (switch-to-buffer
   (make-comint "spwd20" "/home/swhitton/bin/spwd20")))

;;; toggle orientation of a two window split

(defun toggle-window-split ()
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

;;; swap/rotate windows

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun ergoemacs-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?")))

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) myFileList))))))

(defun ergoemacs-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

(defun join-setqs ()
  "Interactively join a series of setq forms into a single definition"
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

(defun smarter-move-beginning-of-line (arg)
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

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun prelude-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
        (goto-char (+ origin (* (length region) arg) arg))))

;; from http://blog.gleitzman.com/post/35416335505/hunting-for-unicode-in-emacs
(defun unicode-hunt ()
  "Tidy up a buffer by replacing all special Unicode characters
   (smart quotes, etc.) with their more sane cousins"
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

;;; functions from magnars's emacs config

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (sp-backward-kill-word 1)))

;;;; ---- personal settings ----

;;; key bindings

;; movement
(bind-key "C-c t p" 'transpose-params)

;; dwim alternatives to standard bindings
(bind-key "M-w" 'save-region-or-current-line)

;; I almost never want to quit and if I do there is Alt-F4
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; my function to fix my published blog
(bind-key "C-c x p" (lambda () (interactive) (swhitton/pyblosxom-fixups)))

;; sometimes need to forcefully access the system clipboard,
;; especially on Windows
(bind-key "C-c x C-y" 'clipboard-yank)
(bind-key "C-c x M-w" 'clipboard-kill-ring-save)
(bind-key "C-c x C-x C-k" 'clipboard-kill-region)

;; get a tmux terminal in current dir
;; (define-key global-map (kbd "C-c t") '(lambda ()
;;                                         (interactive)
;;                                        (shell-command (concat "tmux " "split-window -c " default-directory))))

(bind-key "S-<menu>" 'my-toggle-lang-env)
(bind-key "S-<Multi_key>" 'my-toggle-lang-env) ; need this on Apple keyboard

(bind-key "C-c c" 'swhitton/centralise-current-window)
(bind-key "C-c d" 'spwd20)
(bind-key "C-c S" 'toggle-window-split)
(bind-key "C-c R" 'rotate-windows)
(bind-key "C-x C-k" 'kill-region)
(bind-key "C-h" 'delete-backward-char) ; overriden by smartparens
(bind-key "C-x M-k" 'backward-kill-sentence)
(bind-key "C-c u" 'unicode-hunt)

;; C-m and RET should reindent the current line only for languages
;; that don't use semantic indentation
(bind-key "RET" 'reindent-then-newline-and-indent)
(bind-key "C-m" 'reindent-then-newline-and-indent)
(bind-key "RET" 'newline-and-indent python-mode-map)
(bind-key "C-m" 'newline-and-indent python-mode-map)
(add-hook 'haskell-mode-hook (lambda()
                               (bind-key "RET" 'newline-and-indent haskell-mode-map)
                               (bind-key "C-m" 'newline-and-indent haskell-mode-map)))

(bind-key "C-x M-t" 'transpose-paragraphs)

;; fixup-whitespace seems to make just-one-space redundant
(bind-key "M-SPC" 'fixup-whitespace)

(unbind-key "C-x m")
;; (bind-key "C-c s" 'join-setqs emacs-lisp-mode-map)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-c d" 'prelude-duplicate-current-line-or-region)
(bind-key "C-c M-d" 'prelude-duplicate-and-comment-current-line-or-region)
(bind-key "C-c P" 'proced)

;; remap C-a to `smarter-move-beginning-of-line'
(bind-key "C-a" 'smarter-move-beginning-of-line)

(bind-key "C-c SPC" 'fixup-whitespace)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; abbreviations

(setq abbrev-file-name "~/doc/emacs-abbrevs")

;; turn on for all buffers, if our abbrevs file is checked out
(if (file-exists-p abbrev-file-name)
    (progn
      (setq save-abbrevs t)
      (setq-default abbrev-mode t)))

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
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
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
(electric-layout-mode 1)
(electric-indent-mode 1)

;; disable for python mode where it makes a mess
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; browser
(setq browse-url-generic-program "iceweasel"
      browse-url-browser-function 'browse-url-generic)

;; return to Emacs 23 selection/clipboard behaviour
(setq select-active-regions nil
      mouse-drag-copy-region t
      x-select-enable-primary t
      x-select-enable-clipboard t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; always add a new line
(setq require-final-newline t)

;; require a buffer to have a final newline
(setq require-final-newline 'visit-save)

;; scrolling
(setq scroll-preserve-screen-position t)
;;;(setq scroll-margin 5)
;;;(setq scroll-conservatively 4)
;;;(setq scroll-up-aggressively 0.3) ; don't try and just slap all these three on at once!
;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html

(setq switch-to-visible-buffer nil)
(setq ido-default-buffer-method 'selected-window)

;; dabbrev should be case-insensitive
(setq dabbrev-case-fold-search t)

;; view mode should be read-only
(setq view-read-only t)

;;; command aliases

(defalias 'rb 'revert-buffer)
(defalias 'er 'eval-region)

;; I often want to toggle this
(defalias 'oim 'org-indent-mode)

;;; don't ask me before following symlinks to files in version control
(setq vc-follow-symlinks t)

;;; make it easy to revert window configuratin

(winner-mode 1)

;;; colours in comint modes

(ansi-color-for-comint-mode-on)

;;;; ---- modes configuration ----

;;; mail mode for mutt

(add-to-list 'auto-mode-alist '("/mutt" . message-mode))
(add-hook 'message-mode-hook 'message-goto-body)

(defun djcb-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]"
                    (if (= 0 (length summ)) "" (concat ": " summ))
                    n
                    (if (= 1 n) "" "s")))))

(setq mail-header-separator "")
(add-hook 'message-mode-hook (lambda ()
                               (auto-fill-mode)
                               (footnote-mode)
                               (define-key message-mode-map (kbd "C-c C-s") 'djcb-snip)
                               (message-goto-body)
                               (orgstruct++-mode) ; must go last for some reason
                               ))

;;; IELM

(setq ielm-dynamic-return nil)

;;; text mode

(toggle-text-mode-auto-fill)

;;; dired

;; dired omit mode mapping conflicts with my ace jump mode
;; binding
(define-key dired-mode-map (kbd "M-o") 'nil)
(define-key dired-mode-map (kbd "C-c M-o") 'dired-omit-mode)

(use-package dired-x)
(setq-default dired-omit-mode t)
(setq dired-omit-files "^\\...+$")

;; bind two useful functions from ergoemacs
(define-key dired-mode-map (kbd "C-c C-o") 'ergoemacs-open-in-external-app)
(define-key dired-mode-map (kbd "C-c C-d") 'ergoemacs-open-in-desktop)

;;; LaTeX

(setq TeX-auto-save t
      TeX-parse-self t
      ;; reftex-plug-into-AUCTeX t
      LaTeX-indent-level 4
      LaTeX-item-indent -2
      TeX-newline-function 'reindent-then-newline-and-indent)

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map [backtab] 'LaTeX-indent-line)))
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

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

;;; finally, maximise the window if we're on Windows.  Seems to
;;; require Emacs to have been launched with -mm option in order for
;;; this to work
(if (eq system-type 'windows-nt)
    (w32-send-sys-command 61488))
