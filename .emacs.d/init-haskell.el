;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;; There are a lot of choices to make in setting up Emacs for writing
;; Haskell.  In this file, choices made that I think are the most
;; current best practices:
;;

;; - flycheck over flymake
;; - ghc-ng over ghc-mod

;; and choices made out of personal taste:

;; - hi2 indentation style
;; - hindent over stylish-haskell
;; - company over AC

;;; Sources/links:

;; https://www.reddit.com/r/haskell/comments/2uspan/modern_emacs_haskellmode/
;; http://blog.hoersten.co/post/110096363794/modern-emacs-haskell-mode
;; https://github.com/chrisdone/emacs-haskell-config
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

;;; Old config for WIP:

;; (use-package haskell-mode
;;   :ensure
;;   :init (progn
;;           (when (fboundp 'interactive-haskell-mode)
;;             (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
;;           (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
;;           (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;           (add-hook 'haskell-mode-hook 'capitalized-words-mode)
;;           (use-package hi2
;;             :ensure
;;             :init (add-hook 'haskell-mode-hook 'turn-on-hi2))))

;;; Code:

(require 'flymake)
(require 'use-package)
(require 'bind-key)

;;; kill flymake setup done by haskell-mode

(eval-after-load "haskell-mode"
  (setq flymake-allowed-file-name-masks
        (delete '("\\.l?hs\\'" haskell-flymake-init) flymake-allowed-file-name-masks)))

;;; haskell mode does most of our work

(use-package haskell-mode
  :ensure)

;;; hindent for reformatting code

(use-package hindent
  :ensure)

;;; hi2 for indentation

(use-package hi2
  :ensure
  :init (add-hook 'haskell-mode-hook 'turn-on-hi2))

(provide 'init-haskell)
;;; init-haskell.el ends here
