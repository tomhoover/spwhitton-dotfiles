;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;;; Installation:

;; Needs (use-package flycheck) in init.el.

;; Needs exes from Hackage: stylish-haskell

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

  (setq haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-show-indentations nil
        haskell-tags-on-save t
        haskell-process-suggest-remove-import-lines t
        haskell-mode-contextual-import-completion nil)

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

  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  (add-hook 'haskell-mode-hook 'capitalized-words-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (diminish 'interactive-haskell-mode)

  ;; SmartParens interacts badly with haskell-indentation-mode so turn
  ;; it off for now
  (add-hook 'haskell-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'haskell-mode-hook 'electric-pair-mode))

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

(provide 'init-haskell)
;;; init-haskell.el ends here
