;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;; There are a lot of choices to make in setting up Emacs for writing
;; Haskell.  In this file, choices made that I think are a good
;; balance of current best practices and being conservative:

;; - flycheck over flymake
;; - ghc-mod over ghci-ng
;; - hi2 for indentation
;; - stylish-haskell over hindent
;; - company over AC

;;; Installation:

;; Needs (use-package flycheck) in init.el.

;; Needs exes from Hackage: structured-haskell-mode, ghc-mod, stylish-haskell

;;; Code:

(require 'use-package)
(require 'bind-key)
(require 'haskell-tab-indent)

;;; haskell mode does most of our work

(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/pkg/haskell-mode/")

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'" . haskell-core-mode))

  :config

  ;; Turn on an appropriate indentation mode.  Use
  ;; `haskell-indentation-mode' by default, but if our .dir-locals.el
  ;; specifies `indent-tabs-mode' we should use my
  ;; `haskell-tab-indent-mode'.
  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-hook 'hack-local-variables-hook
                        (lambda ()
                          (if indent-tabs-mode
                              (haskell-tab-indent-mode)
                            (haskell-indentation-mode)))
                        nil t))) ; local hook

  ;; Start up all my usual minor modes and bindings.
  (add-hook 'haskell-mode-hook 'spw/haskell-mode-hook)
  (add-hook 'after-save-hook 'spw/haskell-cabal-mode-save-hook)

  (setq haskell-tags-on-save t
        haskell-process-suggest-remove-import-lines t
        haskell-mode-contextual-import-completion nil))

;;; Try to kill off flymake since init.el is starting flycheck.  Also
;;; remove a call to flymake-mode (add the call to `ghc-init' back in
;;; later).

;; Needs emacs 24.4 for with-eval-after-load

(when (and (=  emacs-major-version 24)
           (>= emacs-minor-version 4))
  (with-eval-after-load "haskell-mode"
    ;; Disable haskell-mode's default snippets for now.  We take the
    ;; car of this list because that should be the latest version of
    ;; haskell-mode.
    (delete (car (f-glob (f-join package-user-dir "haskell-mode-*/snippets"))) yas-snippet-dirs)
    (setq flymake-allowed-file-name-masks nil)
    (remove-hook 'haskell-mode-hook (lambda ()
                                      (ghc-init)
                                      (flymake-mode)))))

;;; startup hook

(defun spw/haskell-mode-hook ()
  "Haskell mode startup stuff."
  (interactive)

  ;; When I've created a stack.yaml in the project root, use stack to
  ;; run ghci.
  ;; (when (f-exists? (projectile-expand-root "stack.yaml"))
  ;;   (setq-local haskell-process-type 'ghci)
  ;;   (setq-local haskell-process-path-ghci "stack")
  ;;   (setq-local haskell-process-args-ghci '("ghci"))
  ;;   (setq-local flycheck-haskell-ghc-executable "stack")
  ;;   (setq-local flycheck-ghc-args '("ghc")))

  ;; When I've created a shell.nix in the project root, use nix-shell
  ;; to run ghci and cabal.
  ;; (when (f-exists? (projectile-expand-root "shell.nix"))
  ;;   (setq-local haskell-process-type 'cabal-repl)
  ;;   (setq-local
  ;;    haskell-process-wrapper-function
  ;;    (lambda (argv) (append (list "nix-shell" "-I" "." "--command" )
  ;;                           (list (mapconcat 'identity argv " "))))))

  ;;; basic minor modes

  (turn-on-haskell-doc)
  (capitalized-words-mode)
  (diminish 'capitalized-words-mode)
  (interactive-haskell-mode)
  (diminish 'interactive-haskell-mode)
  (flymake-mode 0)
  (smartparens-mode 0)

  ;;; make sure haskell-flycheck checker being used?

  ;; (when (fboundp 'flycheck-disable-checker)
  ;;   (flycheck-disable-checker 'haskell-ghc))

  ;; until we get ghc-mod working, use dabbrev
  (company-mode 0)
  (bind-key "M-/" 'dabbrev-expand interactive-haskell-mode-map)

  (bind-key "C-c C-b" 'haskell-interactive-bring interactive-haskell-mode-map)
  ;; (bind-key "C-x C-s" 'haskell-mode-save-buffer interactive-haskell-mode-map)

  ;; suggested bindings from Chris Done

  ;; should be moved into use-package declaration above (requires some
  ;; care)

  ;; (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
  ;; (define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
  ;; (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

  (bind-key "C-c C-t" 'haskell-process-do-type interactive-haskell-mode-map)
  (bind-key "C-c C-i" 'haskell-process-do-info interactive-haskell-mode-map)
  (bind-key "C-c C-r" 'haskell-process-restart interactive-haskell-mode-map)
  (bind-key "SPC"     'haskell-mode-contextual-space haskell-mode-map))

;; save hook

(defun spw/haskell-cabal-mode-save-hook ()
  "Regenerate shell.nix if it exists."
  (interactive)
  (when (and (eq major-mode 'haskell-cabal-mode)
             (f-exists? (f-join default-directory "shell.nix")))
    (call-process "/bin/sh" nil t nil "-c" "cabal2nix --shell . > shell.nix")))

(provide 'init-haskell)
;;; init-haskell.el ends here
