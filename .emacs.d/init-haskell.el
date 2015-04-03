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

(require 'flycheck)
(require 'use-package)
(require 'bind-key)

;;; haskell mode does most of our work

(use-package haskell-mode
  :ensure
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'" . haskell-core-mode))
  ;; Start up all my usual minor modes and bindings.
  :init (add-hook 'haskell-mode-hook 'spw/haskell-mode-hook)
  :config (setq haskell-tags-on-save t))

;; load haskell-flycheck only once haskell-mode is loaded

(require 'haskell-flycheck)

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

  ;;; basic minor modes

  (turn-on-haskell-doc)
  (capitalized-words-mode)
  (diminish 'capitalized-words-mode)
  (interactive-haskell-mode)
  (diminish 'interactive-haskell-mode)
  (flymake-mode 0)

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

  ;;; make sure haskell-flycheck checker being used?

  (when (fboundp 'flycheck-disable-checker)
    (flycheck-disable-checker 'haskell-ghc))

  ;; When I've created a shell.nix in the project root, use nix-shell
  ;; to run ghci and cabal.
  (when (f-exists? (projectile-expand-root "shell.nix"))
    (setq-local haskell-process-type 'cabal-repl)
    (setq-local
     haskell-process-wrapper-function
     (lambda (argv) (append (list "nix-shell" "-I" "." "--command" )
                            (list (mapconcat 'identity argv " ")))))))

;;; hindent for reformatting code

;; can't make this work so using stylish-haskell instead

(use-package hindent
  :disabled t
  :ensure
  :diminish hindent-mode
  :init (progn (setq hindent-style "johan-tibell")
               (add-hook 'haskell-mode-hook #'hindent-mode)))

;;; shm^Whi2 for indentation

(use-package hi2
  :ensure
  :diminish hi2-mode
  :init (progn (setq hi2-layout-offset 4
                     hi2-left-offset 4
                     hi2-show-indentations nil)

               (defun spw/hi2-pipe ()
                 "Newline, pipe char and indent"
                 (interactive)
                 (hi2-newline-and-indent)
                 (insert "|")
                 (indent-for-tab-command)
                 (insert " "))

               (add-hook 'haskell-mode-hook 'turn-on-hi2)
               (bind-key "C-c |" 'spw/hi2-pipe hi2-mode-map)))

(use-package shm
  :disabled t
  :ensure
  :init (progn (setq shm-indent-spaces 4)
               (set-face-background 'shm-current-face "#4F4F4F")
               (add-hook 'haskell-mode-hook 'structured-haskell-mode))
  :config (progn
            (bind-key "C-w" 'spw/backward-delete-word shm-map)
            (bind-key "C-x C-k" 'shm/kill-region shm-map)))

;;; ghc-mod doesn't work with cabal 1.22 and ghc 7.8.4 atm

(use-package ghc
  :disabled t
  :ensure
  :init (progn
          (add-hook 'haskell-mode-hook 'ghc-init)

          ;; completion

          (use-package company-ghc
            :ensure
            :config (progn
                      (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
                      (setq company-ghc-show-info t)))))

(provide 'init-haskell)
;;; init-haskell.el ends here
