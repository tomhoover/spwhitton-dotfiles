;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;; There are a lot of choices to make in setting up Emacs for writing
;; Haskell.  In this file, choices made that I think are the most
;; current best practices:

;; - flycheck over flymake
;; - ghci-ng over ghc-mod
;; - Chris Done's haskell-flycheck instead of flycheck-haskell

;; and choices made out of personal taste/what I could make work:

;; - hi2 indentation style
;; - stylish-haskell over hindent
;; - company over AC

;;; Sources/links:

;; https://www.reddit.com/r/haskell/comments/2uspan/modern_emacs_haskellmode/
;; http://blog.hoersten.co/post/110096363794/modern-emacs-haskell-mode
;; https://github.com/LukeHoersten/emacs.d/blob/master/elisp/haskell-init.el (updates since his blog post)

;; https://github.com/chrisdone/emacs-haskell-config
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;; http://robots.thoughtbot.com/building-haskell-projects-with-halcyon

;;; Installation (see ~/doc/org/comproc.org):

;; Needs (use-package flycheck) in init.el.

;; Needs cabal install haskell-docs stylish-haskell.

;; Needs manual install of ghci-ng: https://github.com/chrisdone/ghci-ng

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
  :init (progn (setq ;; haskell-process-args-cabal-repl
                ;; '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")
                haskell-process-path-ghci "ghci-ng"
                haskell-process-arg-ghci "-ferror-spans")
               (add-hook 'haskell-mode-hook 'spw/haskell-mode-hook)))

;;; try to kill off flymake since init.el is starting flycheck

(with-eval-after-load "haskell-mode"
  (setq flymake-allowed-file-name-masks nil))

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

;;; hi2 for indentation

(use-package hi2
  :ensure
  :diminish hi2-mode
  :init (progn (setq hi2-layout-offset 4
                     hi2-left-offset 4)
               (add-hook 'haskell-mode-hook 'turn-on-hi2)))

(provide 'init-haskell)
;;; init-haskell.el ends here
