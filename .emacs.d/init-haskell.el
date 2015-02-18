;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;; There are a lot of choices to make in setting up Emacs for writing
;; Haskell.  In this file, choices made that I think are the most
;; current best practices:
;;

;; - flycheck over flymake
;; - ghci-ng over ghc-mod
;; - Chris Done's haskell-flycheck instead of flycheck-haskell

;; and choices made out of personal taste:

;; - hi2 indentation style
;; - hindent over stylish-haskell
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

;; Needs cabal install haskell-docs.

;; Needs manual install of ghci-ng: https://github.com/chrisdone/ghci-ng

;;; Code:

(require 'flymake)
(require 'use-package)
(require 'bind-key)
(require 'haskell-flycheck)

;;; kill flymake setup done by haskell-mode

(eval-after-load "haskell-mode"
  (setq flymake-allowed-file-name-masks
        (delete '("\\.l?hs\\'" haskell-flymake-init) flymake-allowed-file-name-masks)))

;;; haskell mode does most of our work

(use-package haskell-mode
  :ensure
  :init (add-hook 'haskell-mode-hook 'spw/haskell-mode-hook))

(defun spw/haskell-mode-hook ()
  "Haskell mode startup stuff."
  (interactive)
  (turn-on-haskell-doc)
  (capitalized-words-mode)
  (interactive-haskell-mode)
  (flycheck-disable-checker 'haskell-ghc))

;;; hindent for reformatting code

(use-package hindent
  :ensure)

;;; hi2 for indentation

(use-package hi2
  :ensure
  :init (add-hook 'haskell-mode-hook 'turn-on-hi2))

(provide 'init-haskell)
;;; init-haskell.el ends here
