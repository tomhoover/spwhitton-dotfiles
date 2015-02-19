;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;; There are a lot of choices to make in setting up Emacs for writing
;; Haskell.  In this file, choices made that I think are a good
;; balance of current best practices and being conservative:

;; - flycheck over flymake
;; - ghc-mod over ghci-ng
;; - structured haskell mode for indentation
;; - stylish-haskell over hindent
;; - company over AC

;;; Installation:

;; Needs (use-package flycheck) in init.el.

;; For the required haskell packages, see ~/doc/org/comproc.org

;;; Code:

(require 'flymake)
(require 'use-package)
(require 'bind-key)
(require 'haskell-flycheck)

;;; haskell mode does most of our work

(use-package haskell-mode
  :ensure
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'" . haskell-core-mode))
  :init (progn ;; (setq ;; haskell-process-args-cabal-repl
               ;;  ;; '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")
               ;;  ;; haskell-process-path-ghci "ghci-ng"
               ;;  ;; haskell-process-arg-ghci "-ferror-spans"
               ;;  )
               (add-hook 'haskell-mode-hook 'spw/haskell-mode-hook)))

;;; Try to kill off flymake since init.el is starting flycheck.  Also
;;; remove a call to `ghc-init' since we're not using ghc-mod.

(with-eval-after-load "haskell-mode"
  (setq flymake-allowed-file-name-masks nil)
  (remove-hook 'haskell-mode-hook (lambda ()
                                    (ghc-init)
                                    (flymake-mode))))

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

  ;; suggested bindings from Chris Done

  ;; should be moved into use-package declaration above (requires some
  ;; care)

  (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
  (define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
  (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

  ;;; make sure haskell-flycheck checker being used?

  (when (fboundp 'flycheck-disable-checker)
    (flycheck-disable-checker 'haskell-ghc)))

;;; hindent for reformatting code

;; can't make this work so using stylish-haskell instead

(use-package hindent
  :disabled t
  :ensure
  :diminish hindent-mode
  :init (progn (setq hindent-style "johan-tibell")
               (add-hook 'haskell-mode-hook #'hindent-mode)))

;;; hi2^Wshm for indentation

(use-package hi2
  :disabled t
  :ensure
  :diminish hi2-mode
  :init (progn (setq hi2-layout-offset 4
                     hi2-left-offset 4)
               (add-hook 'haskell-mode-hook 'turn-on-hi2)))

(use-package shm
  :ensure
  :init (progn (setq shm-indent-spaces 4)
               (set-face-background 'shm-current-face "#4F4F4F")
               (add-hook 'haskell-mode-hook 'structured-haskell-mode))
  :config (progn
            (bind-key "C-w" 'spw/backward-delete-word shm-map)
            (bind-key "C-x C-k" 'shm/kill-region shm-map)))

(provide 'init-haskell)
;;; init-haskell.el ends here
