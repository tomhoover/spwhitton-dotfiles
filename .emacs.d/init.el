;;;; ---- package management ----

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
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

;; focus follow mouse
(setq mouse-autoselect-window nil
      focus-follows-mouse t)

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; don't handle keyboard events before redrawing
(setq redisplay-dont-pause t)

;; don't prompt to create scratch buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; initial frame width
(if window-system (set-frame-width (selected-frame) 80))

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

;;; sexy mode line

(use-package smart-mode-line
  :ensure
  :init (progn
          (use-package powerline :ensure) ; dependency
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

;;; the zen of vim from my youth

(use-package evil
  :ensure
  :config (progn
            ;;; settings

            (setq evil-cross-lines t)

            ;;; initial states

            (evil-set-initial-state 'deft-mode 'insert)
            (evil-set-initial-state 'git-commit-mode 'insert)

            ;;; bindings

            (define-key evil-window-map (kbd "u") 'winner-undo)

            (evil-global-set-key 'emacs (kbd "C-w") 'evil-window-map)
            (evil-global-set-key 'emacs (kbd ",") 'evil-execute-in-god-state)
            (evil-global-set-key 'normal (kbd ",") 'evil-execute-in-god-state)
            (evil-global-set-key 'visual (kbd ",") 'evil-execute-in-god-state)
            (evil-global-set-key 'insert (kbd "M-,") 'evil-execute-in-god-state)

            (evil-define-key 'normal emacs-lisp-mode-map (kbd ")") 'paredit-forward-up)
            (evil-define-key 'normal emacs-lisp-mode-map (kbd "(") 'paredit-backward-up)

            ;; I can use the arrow keys if I need to move by char in
            ;; Emacs state buffers.  I need to be able to scroll easily
            (evil-global-set-key 'emacs (kbd "C-f") 'scroll-up-command)
            (evil-global-set-key 'emacs (kbd "C-b") 'scroll-down-command)

            (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)

            ;; escape quits
            (bind-key "<escape>" 'isearch-cancel isearch-mode-map)
            (define-key minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)
            (define-key minibuffer-local-ns-map (kbd "ESC") 'abort-recursive-edit)
            (define-key minibuffer-local-completion-map (kbd "ESC") 'abort-recursive-edit)
            (define-key minibuffer-local-must-match-map (kbd "ESC") 'abort-recursive-edit)
            (define-key minibuffer-local-isearch-map (kbd "ESC") 'abort-recursive-edit)

            ;;; text objects

            ;; TODO: make the following work well with counts i.e. selecting more than one subtree or list item

            ;; TODO: item text objects should not include subitems

            (evil-define-text-object evil-org-outer-subtree (count &optional beg end type)
              "An Org subtree.  Uses code from `org-mark-subtree`"
              (save-excursion
                ;; get to the top of the tree
                (org-with-limited-levels
                 (cond ((org-at-heading-p) (beginning-of-line))
                       ((org-before-first-heading-p) (user-error "Not in a subtree"))
                       (t (outline-previous-visible-heading 1))))

                (decf count)
                (when count (while (and (> count 0) (org-up-heading-safe)) (decf count)))

                ;; extract the beginning and end of the tree
                (let ((element (org-element-at-point)))
                  (list (org-element-property :end element)
                        (org-element-property :begin element)))))

            (evil-define-text-object evil-org-inner-subtree (count &optional beg end type)
              "An Org subtree, minus its header and concluding line break.  Uses code from `org-mark-subtree`"
              (save-excursion
                ;; get to the top of the tree
                (org-with-limited-levels
                 (cond ((org-at-heading-p) (beginning-of-line))
                       ((org-before-first-heading-p) (user-error "Not in a subtree"))
                       (t (outline-previous-visible-heading 1))))

                (decf count)
                (when count (while (and (> count 0) (org-up-heading-safe)) (decf count)))

                ;; extract the beginning and end of the tree
                (let* ((element (org-element-at-point))
                       (begin (save-excursion
                                (goto-char (org-element-property :begin element))
                                (next-line)
                                (point)))
                       (end (save-excursion
                              (goto-char (org-element-property :end element))
                              (backward-char 1)
                              (point))))
                  (list end begin))))

            (define-key evil-outer-text-objects-map "S" 'evil-org-outer-subtree)
            (define-key evil-inner-text-objects-map "S" 'evil-org-inner-subtree)

            (evil-define-text-object evil-org-outer-item (count &optional beg end type)
              (let* ((regionp (org-region-active-p))
                     (struct (progn
                               (re-search-backward "^[[:space:]]*- ")
                               (org-list-struct)))
                     (begin (org-list-get-item-begin))
                     (end (org-list-get-item-end (point-at-bol) struct)))
                (if (or (not begin) (not end))
                    nil
                  (list begin end))))

            (evil-define-text-object evil-org-inner-item (count &optional beg end type)
              (let* ((regionp (org-region-active-p))
                     (struct (progn
                               (re-search-backward "^[[:space:]]*- ")
                               (org-list-struct)))
                     (begin (progn (goto-char (org-list-get-item-begin))
                                   (forward-char 2)
                                   (point)))
                     (end (org-list-get-item-end-before-blank (point-at-bol) struct)))
                (if (or (not begin) (not end))
                    nil
                  (list begin end))))

            (define-key evil-inner-text-objects-map "I" 'evil-org-inner-item)
            (define-key evil-outer-text-objects-map "I" 'evil-org-outer-item)

            ;;; Advice

            ;; make Evil respect the eshell prompt
            (defadvice evil-insert-line (after spw/evil-eshell-bol activate)
              (if (eq major-mode 'eshell-mode)
                  (call-interactively 'eshell-bol)))
            (defadvice evil-first-non-blank (after spw/evil-eshell-bol activate)
              (if (eq major-mode 'eshell-mode)
                  (call-interactively 'eshell-bol)))

            ;; make Evil respect Org line prefixes when inserting at
            ;; the beginning of the line
            (defadvice evil-insert-line (after spw/evil-org-bol activate)
              (if (eq major-mode 'org-mode)
                  (call-interactively 'org-beginning-of-line)))))

;; evil support packages

(use-package evil-god-state :ensure
  :config (progn
            (evil-global-set-key 'god (kbd "ESC") 'evil-god-state-bail)
            (add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
            (add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))

            ;; god-mode doesn't work well in special-mode buffers such as
            ;; the Org agenda.  These buffers bind functions
            ;; other than `self-insert-command` to letter and number
            ;; keys, and god-local-mode intercepts key presses by
            ;; remapping `self-insert-command` to its own function.
            ;; So it fails to get a grip in special-mode buffers.

            ;; This is okay when one isn't using Evil
            ;; because in such modes bindings aren't generally
            ;; prefixed with C- and M-: there's little reason to use god-mode.

            ;; However, I want to use evil-god-state from the Emacs
            ;; state in Org agenda buffers.  So manually remap all the keys.
            (let ((map god-local-mode-map)
                  (keys '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                          "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
                          "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G"
                          "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                          "S" "T" "U" "V" "W" "X" "Y" "Z" "0" "1" "2"
                          "3" "4" "5" "6" "7" "8" "9" "0")))
              (dolist (key keys)
                (define-key map (kbd key) 'god-mode-self-insert))

              (defadvice evil-execute-in-god-state
                  (before evil-execute-in-god-state-from-visual-state activate)
                "When in visual state, get out of visual state
and restore the selection before firing up god-mode.  This avoids
visual state bindings conflicting with god-mode"
                (when (evil-visual-state-p)
                  (if (< (point) (mark))
                      (exchange-point-and-mark))
                  (forward-char 1)
                  (evil-exit-visual-state)
                  (let ((activate-mark-hook nil)) ; stop visual mode from firing
                    (activate-mark))))

              (evil-define-key 'normal emacs-lisp-mode-map (kbd ")") 'paredit-forward-up))))

(use-package evil-surround
  :ensure
  :config (progn
            ;; make sure that evil-surround works when paredit is
            ;; active (from evil-surround's readme on github)
            (add-to-list 'evil-surround-operator-alist
                         '(evil-paredit-change . change))
            (add-to-list 'evil-surround-operator-alist
                         '(evil-paredit-delete . delete))))

(use-package evil-args :ensure)
(use-package evil-matchit :ensure)
;; (use-package evil-jumper :ensure)

(use-package paredit
  :ensure
  :commands paredit-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode)))

(use-package evil-paredit :ensure)
(use-package evil-indent-textobject :ensure)
(setq evil-operator-comment-key "Q")
(use-package evil-operator-comment)

;; evil-leader

(use-package evil-leader
  :ensure
  :init (progn
          ;; leader binding
          (evil-leader/set-leader "<SPC>")
          (evil-leader/set-key
            ;; core map
            "<SPC>" 'evil-ace-jump-word-mode
            "f" 'helm-find-files
            "j" 'helm-mini
            "x" 'helm-M-x
            "r" 'helm-swoop
            "k" 'kill-buffer
            "l" 'persp-toggle
            "L" 'persp-switch
            "v" 'projectile-vc
            "C-w" 'evil-window-map
            "w" 'evil-window-mru
            "b" 'switch-to-buffer
            "s" (lambda ()
                  (interactive)
                  ;; save the current buffer first (if it's visiting a
                  ;; file) to avoid a y/n prompt
                  (if (buffer-file-name) (save-buffer))
                  ;; save all Org buffers to avoid some more prompts
                  (if (featurep 'org)
                      (org-save-all-org-buffers))
                  (save-some-buffers))

            ;; narrowing map
            "ns" 'org-narrow-to-subtree
            "nr" 'narrow-to-region
            "nf" 'narrow-to-defun
            "nn" 'widen

            ;; Org-mode map
            "oc" 'org-capture
            "ol" 'org-store-link
            "oa" 'org-agenda
            "o[" 'spw/org-agenda-file-to-front
            "o]" 'spw/org-remove-file

            ;; launcher map
            "gs" 'persp-eshell
            "gc" 'spw/manual-cleanup
            "gd" 'deft
            "gl" 'tblesson
            "gk" 'kill-emacs
            "gr" (lambda ()
                   (interactive)
                   (projectile-persp-switch-project "~/src/dotfiles")
                   (find-file "~/src/dotfiles/.emacs.d/init.el")
                   (eval-buffer))
            "*" 'calc-dispatch

            ;; toggle map
            "te" 'toggle-debug-on-error
            "ti" 'org-indent-mode
            "tw" 'spw/writing-toggle

            ;; Jabber map ('c' for 'chat')
            "cj" 'jabber-activity-switch-to
            "cc" 'jabber-chat-with)

          (evil-leader/set-key-for-mode 'emacs-lisp-mode
            ;; paredit map
            "dj" 'paredit-join-sexps
            "ds" 'paredit-split-sexp
            "dp" 'paredit-splice-sexp
            "dl" 'paredit-forward-slurp-sexp
            "dh" 'paredit-forward-barf-sexp
            "dL" 'paredit-backward-slurp-sexp
            "dH" 'paredit-backward-barf-sexp
            "dk" 'paredit-kill

            ;; evaluation map
            "ee" 'eval-surrounding-sexp
            "ef" 'eval-defun
            "er" 'eval-region
            "eb" 'eval-buffer
            "E"  'eval-and-replace)

          ;; `evil-leader/in-all-states' binds in insert state, but we
          ;; want it emacs state only.  So do this with a hook
          (setq evil-leader/in-all-states nil)
          (setq evil-leader/non-normal-prefix "")
          (defun evil-leader/add-to-emacs-state ()
            (let* ((prefixed (read-kbd-macro (concat evil-leader/non-normal-prefix evil-leader/leader)))
                   (no-prefix (read-kbd-macro evil-leader/leader))
                   (mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
                   (map (or mode-map evil-leader--default-map)))
              (if evil-leader-mode
                  (progn
                    (evil-normalize-keymaps)
                    (define-key evil-emacs-state-local-map prefixed map)
                    ;; (define-key evil-emacs-state-local-map (kbd "<escape> <escape>") 'keyboard-quit)
                    ))))))

;; fire up Evil and associated packages

(global-evil-leader-mode 1)
(add-hook 'evil-local-mode-hook 'evil-leader/add-to-emacs-state)
(remove-hook 'evil-local-mode-hook 'evil-turn-on-undo-tree-mode)
(evil-mode)
;; (require 'evil-jumper)
(global-evil-surround-mode t)
(global-evil-matchit-mode t)
(global-evil-operator-comment-mode 1)
(global-undo-tree-mode 0)

;;; Org

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org" . org-mode)
         ("\\.org_archive" . org-mode))
  :commands (org-capture
             org-store-link
             org-agenda)
  ;; :bind (("C-M-c" . org-capture)
  ;;        ("C-c l" . org-store-link)
  ;;        ("C-c a" . org-agenda))
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

;;; magit

(use-package magit
  :ensure
  :diminish magit-auto-revert-mode
  :config (progn
            (bind-key (kbd "'") 'magit-section-jump-map magit-status-mode-map)
            (bind-key (kbd "j") 'magit-goto-next-section magit-status-mode-map)
            (bind-key (kbd "M-j") 'magit-goto-next-sibling-section magit-status-mode-map)
            (bind-key (kbd "k") 'magit-goto-previous-section magit-status-mode-map)
            (bind-key (kbd "K") 'magit-discard-item magit-status-mode-map)
            (bind-key (kbd "M-k") 'magit-goto-previous-sibling-section magit-status-mode-map)

            ;; restore usual magit spacebar command to M-SPC
            (bind-key (kbd "M-SPC") 'magit-show-item-or-scroll-up)

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

;;; winner mode: undo window configuration changes

(winner-mode 1)

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
            ;; this technique from
            ;; https://github.com/bradleywright/emacs.d/blob/master/setup-company.el
            (defun spw/company-complete-lambda (arg)
              "Ignores passed in arg like a lambda and runs company-complete"
              (company-complete))

            (defun spw/company-prog-setup ()
              "Setup company mode carefully when its needed, rather than using the brash global-company-mode"
              (company-mode 1)
              (setq-local evil-complete-next-func 'spw/company-complete-lambda)
              (setq-local evil-complete-previous-func 'spw/company-complete-lambda)
              ;; Make sure emacs does the right thing with completion command
              ;; from https://github.com/bradleywright/emacs.d/blob/master/setup-company.el
              (define-key (current-local-map) [remap hippie-expand] 'company-complete))

            (add-hook 'prog-mode-hook 'spw/company-prog-setup)

            ;; I like my C-w binding so move one of company's bindings
            (define-key company-active-map "\C-w" nil)
            (define-key company-active-map "\C-j" 'company-show-location)

            ;; vim-like completion selection
            (define-key company-active-map "\C-n" 'company-select-next)
            (define-key company-active-map "\C-p" 'company-select-previous)

            (setq company-idle-delay nil
                  company-minimum-prefix-length 0
                  company-echo-delay 0)

            (add-to-list 'company-backends 'company-capf)
            (add-to-list 'company-transformers 'company-sort-by-occurrence)

            ;; python code completion

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
            ;; (bind-key "C-h" 'deft-filter-decrement deft-mode-map)

            (defadvice deft (before persp-deft activate)
              (projectile-persp-switch-project "~/doc"))))

;;; allow lisp to interact with python

(use-package pymacs :ensure)

;;; Get Python documentation as info files

(use-package python-info :ensure)
(use-package pydoc-info :ensure)

;;; fix binding in python-mode

;; (use-package python
;;   :config (define-key python-mode-map (kbd "C-h") 'python-indent-dedent-line-backspace))

;;; flycheck

(use-package flycheck :ensure :idle (global-flycheck-mode))

;;; edit minibuffer in a proper buffer

;; (use-package miniedit
;;   :ensure
;;   :commands miniedit
;;   :init (progn
;;           (define-key minibuffer-local-map "\M-\C-e" 'miniedit)
;;           (define-key minibuffer-local-ns-map "\M-\C-e" 'miniedit)
;;           (define-key minibuffer-local-completion-map "\M-\C-e" 'miniedit)
;;           (define-key minibuffer-local-must-match-map "\M-\C-e" 'miniedit)))

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
  :init (setq ebib-preload-bib-files '("~/doc/spw.bib"))
  :commands ebib)

;;; dired enhancements

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t)

(use-package dired-sort-map
  :init (setq dired-listing-switches "--group-directories-first -alh"))

(use-package git-annex :ensure)

;;; loads of visual help for working with Emacs regexps

(use-package visual-regexp
  :ensure
  :commands (vr/replace vr/query-replace))

;;; simple concept of projects

(use-package projectile
  :ensure
  :commands projectile-vc
  :init (projectile-global-mode)
  :diminish 'projectile-mode
  :config (progn
            (setq projectile-switch-project-action 'projectile-dired
                  projectile-completion-system 'helm)
            ;; access projectile thorugh evil leader
            (define-key evil-leader--default-map (kbd "p") projectile-command-map)
            (diminish 'projectile-mode)))

(use-package persp-projectile :ensure)

(use-package perspective
  :ensure
  :commands (persp-toggle persp-switch)
  :init (progn
          (setq persp-modestring-dividers '("" "" "|"))
          (persp-mode)
          ;; access perspectives thorugh evil leader
          (define-key evil-leader--default-map (kbd "q") perspective-map)
          (defun persp-toggle (arg)
            (interactive "P")
            (if arg (call-interactively 'persp-switch)
              (persp-switch (persp-name persp-last))))))

;;; powerful completion everywhere, especially for jumping between files

(use-package helm
  :ensure
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

          ;; rebind some keys
          (bind-key "C-w" 'backward-delete-word helm-map)
          (bind-key "C-o" 'helm-select-action helm-map)
          (bind-key "M-i" 'helm-next-source helm-map)
          (bind-key "<escape>" 'helm-keyboard-quit helm-map)
          (bind-key "<escape>" 'helm-keyboard-quit helm-comp-read-map)
          (bind-key "M-j" 'helm-next-line helm-map)
          (bind-key "M-k" 'helm-previous-line helm-map)
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

          ;; helm-mini shouldn't stop working just because we're not
          ;; in a projectile project.  And it's way too slow to use
          ;; from an eshell buffer that's TRAMP'd to a remote host
          (defadvice helm-mini (around spw/helm-mini activate)
            "Remove projectile stuff if not in a project, unless it's my massive annex"
            (when (equal major-mode 'eshell-mode)
              (setq helm-mini-default-sources '(helm-source-buffers-list
                                                helm-source-bookmarks
                                                helm-source-dired-recent-dirs
                                                helm-source-recentf)))

            (if (and (projectile-project-p)
                     (not (equal (projectile-project-name) "annex"))
                     (not (equal (persp-name persp-curr) "main")))

                (setq helm-mini-default-sources '(helm-source-buffers-list
                                                  helm-source-projectile-files-list
                                                  helm-source-imenu-anywhere
                                                  helm-source-bookmarks
                                                  helm-source-recentf))
              (setq helm-mini-default-sources '(helm-source-buffers-list
                                                ;; helm-source-ido-virtual-buffers
                                                helm-source-imenu-anywhere
                                                helm-source-bookmarks
                                                helm-source-dired-recent-dirs
                                                helm-source-recentf)))
            ad-do-it

            ;; ;; once Org is loaded, can add Org headline source
            ;; (eval-after-load 'org
            ;;   (add-to-list 'helm-mini-default-sources 'helm-source-org-headline t))

            (helm-adaptative-mode))

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
                      (setq helm-swoop-pre-input-function (lambda () nil))

                      ;; make sure that we can jump back from where
                      ;; helm swoop took us
                      (defadvice helm-swoop (after helm-swoop-set-evil-marker activate)
                        (evil-set-jump))
                      (defadvice helm-multi-swoop-all (after helm-swoop-set-evil-marker activate)
                        (evil-set-jump))))

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
              (persp-eshell)
              (unless (get-buffer-process (current-buffer))
                (funcall cd-eshell))))))

;;; snippets

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :idle (yas-global-mode))

;;; eshell's plan9-style smart shell

(use-package em-smart
  :init (progn
          (setq eshell-where-to-jump 'begin
                eshell-review-quick-commands nil
                eshell-smart-space-goes-to-end t)))

;;; htmlize for Org HTML export/publishing

(use-package htmlize :ensure)

;;; close old buffers once per day

(use-package midnight
  :init (progn
          (midnight-delay-set 'midnight-delay "3am")))

;;; make indentation in python nice and visible

(use-package highlight-indentation
  :ensure
  :init (progn
          (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)))

;;; jump around what's visible

(use-package ace-jump-mode
  :ensure
  :commands ace-jump-mode)

;;; use ace-jump-mode to move between links in help file

(use-package ace-link
  :ensure
  :idle (ace-link-setup-default))

;;; simple pomodoro timer

(use-package pomodoro)

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

;;; alternative to my old `swhitton/centralise-current-window'

(use-package centered-window-mode
  :commands centered-window-mode)

;;;; ---- functions ----

;;; toggle some features on and off to make Emacs better at prose editing

(defun org-fill-buffer ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (when (not (string= "gpg" (f-ext (f-this-file))))
      (save-excursion
        (goto-char (point-min))
        (while (not (eq (point) (point-max)))
          (org-forward-paragraph)
          (org-fill-paragraph))))))

(defun spw/writing-on ()
  (add-hook 'before-save-hook 'org-fill-buffer nil t)
  (wc-mode 1)
  (variable-pitch-mode 1)
  (unless (eq system-type 'windows-nt)
    (centered-window-mode 1))
  (when (eq major-mode 'org-mode)
    (org-indent-mode 0))
  (add-hook 'evil-insert-state-exit-hook 'fill-paragraph nil t))

(defun spw/writing-off ()
  (remove-hook 'before-save-hook 'org-fill-buffer t)
  (wc-mode 0)
  (variable-pitch-mode 0)
  (unless (eq system-type 'windows-nt)
    (centered-window-mode 0))
  (when (eq major-mode 'org-mode)
    ;; TODO: finesse this.  don't turn it on if it wouldn't be on by default
    (org-indent-mode 1))
  (remove-hook 'evil-insert-state-exit-hook 'fill-paragraph t))

(defun spw/writing-toggle ()
  (interactive)
  (let ((activate (if (boundp 'buffer-face-mode) buffer-face-mode)))
    (if activate
        (spw/writing-off)
      (spw/writing-on))))

;;; eval the surrounding sexp (https://stackoverflow.com/posts/2172827/revisions)

(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (if (looking-at "(")
        (progn (forward-char 1) (eval-surrounding-sexp 1))
      (up-list (abs levels))
      (eval-last-sexp nil))))

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

(global-set-key "\C-w" 'backward-delete-word)
(global-set-key "\M-d" 'delete-word)

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

(defun spw/clean-lisp-dangling-brackets ()
  "Clean up dangling brackets"
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
  (interactive)
  (case major-mode
    (haskell-mode
     (delete-trailing-whitespace-except-current-line)
     (compact-blank-lines))
    (python-mode
     (delete-trailing-whitespace-except-current-line)
     (compact-blank-lines))
    (message-mode
     (save-excursion
       (message-goto-body)
       (save-restriction
         (narrow-to-region (point) (point-max))
         (fill-region (point-min) (point-max))
         (whitespace-cleanup))))
    (emacs-lisp-mode
     (delete-trailing-whitespace-except-current-line)
     (compact-blank-lines))))

(defun spw/manual-cleanup ()
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
            (other-window -1))))

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
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

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

;; from Emacs Prelude/Redux author

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; from magnars

(defun new-line-dwim ()
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
    (indent-for-tab-command)
    (evil-insert 1)))

(defun terminal-emulator (arg exec)
  (interactive "P\nsexec: ")
  (async-start-process "term" "/usr/bin/urxvt" 'ignore
                       "++iso14755"
                       "++iso14755_52"
                       "-title"
                       (shell-quote-argument exec)
                       "-e"
                       "/bin/sh"
                       "-c"
                       (shell-quote-argument exec)))

(defun persp-eshell (arg)
  "Switch to perspective's eshell or create it.  If already in the eshell, move to the last prompt and clear it, ready for input"
  (interactive "P")
  (if (equal major-mode 'eshell-mode)
      (progn
        (evil-goto-line)
        (eshell-bol)
        (ignore-errors (kill-line))
        (call-interactively 'evil-insert))
    (when arg (split-window-right) (other-window 1))
    (if (and (projectile-project-p)
             (not (equal (persp-name persp-curr) "main")))
        ;; we're in a project: name buffer carefully
        (if (get-buffer (concat "*eshell* (" (persp-name persp-curr) ")"))
            (helm-switch-to-buffer (concat "*eshell* (" (persp-name persp-curr) ")"))
          (eshell "a")
          (rename-buffer (concat "*eshell* (" (persp-name persp-curr) ")")))
      ;; we're not in a project: don't
      (if (get-buffer "*eshell*")
          (helm-switch-to-buffer "*eshell*")
        (call-interactively 'eshell)))))

(defun tblesson (grade lesson period)
  "Start writing a lesson plan for a standard textbook-based lesson"
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

;;; show parens

(setq show-paren-delay 0)
(show-paren-mode 1)

;;; no tabs please

(setq-default indent-tabs-mode nil)

;;; key bindings

;; I almost never want to quit and if I do there is Alt-F4
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; vim navigation
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)

;; my function to fix my published blog
(bind-key "C-c x p" (lambda () (interactive) (swhitton/pyblosxom-fixups)))

;; sometimes need to forcefully access the system clipboard,
;; especially on Windows
(bind-key "C-c x C-y" 'clipboard-yank)
(bind-key "C-c x M-w" 'clipboard-kill-ring-save)
(bind-key "C-c x C-x C-k" 'clipboard-kill-region)

(bind-key "S-<menu>" 'my-toggle-lang-env)
(bind-key "S-<Multi_key>" 'my-toggle-lang-env) ; need this on Apple keyboard

(bind-key "C-c c" 'swhitton/centralise-current-window)
(bind-key "C-c S" 'toggle-window-split)
(bind-key "C-c R" 'rotate-windows)
(bind-key "C-c u" 'unicode-hunt)
(bind-key "M-o" 'new-line-dwim)
(bind-key "M-RET" 'new-line-dwim)

;; C-m and RET should reindent the current line only for languages
;; that don't use semantic indentation
(bind-key "RET" 'reindent-then-newline-and-indent)
(add-hook 'python-mode-map (lambda ()
                             (bind-key "RET" 'newline-and-indent python-mode-map)))
(add-hook 'haskell-mode-hook (lambda ()
                               (bind-key "RET" 'newline-and-indent haskell-mode-map)))

;; automatically rebalance windows
(defadvice split-window-below (after rebalance-windows activate)
  (balance-windows))
(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows))

;; fixup-whitespace seems to make just-one-space redundant
(bind-key "M-SPC" 'fixup-whitespace)

(unbind-key "C-x m")
(bind-key "M-/" 'hippie-expand)

;; remap C-a to `smarter-move-beginning-of-line'
(bind-key "C-a" 'smarter-move-beginning-of-line)

;; insert mode headers
(bind-key "C-c i h" 'add-file-local-variable-prop-line)

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
;; (electric-layout-mode 1)
(electric-indent-mode 1)

;; templates when creating new files
(auto-insert-mode 1)

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

(defun turn-on-comment-filling ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'emacs-lisp-mode-hook 'turn-on-comment-filling)

;;; mail mode for mutt

(use-package message
  :mode ("/mutt-.*$" . message-mode)
  :init (add-hook 'message-mode-hook 'message-goto-body))

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
                               ;; (orgstruct++-mode) ; must go last for some reason
                               ))

;;; IELM

(setq ielm-dynamic-return nil)

;;; text mode

(add-hook 'text-mode 'turn-on-auto-fill)
(add-hook 'text-mode 'refill-mode)

;;; dired

(use-package dired-x)
(setq-default dired-omit-mode t)
(setq dired-omit-files "^\\...+$")

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

;;; programming modes

;; automatically insert some pairs ...
(electric-pair-mode 1)
(setq electric-pair-pairs nil)
;; ... but not in text modes (this is a hack)
(add-hook 'text-mode-hook (lambda ()
                            (make-local-variable 'post-self-insert-hook)
                            (remove-hook 'post-self-insert-hook 'electric-pair-post-self-insert-function t)))

;;; javascript

;; don't insert a newline after a semicolon
(add-hook 'js-mode-hook (lambda ()
                          (setq-local electric-layout-rules
                                      (remove (quote (?\; . after)) electric-layout-rules))))
