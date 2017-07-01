;;; init-haskell --- Sean's configuration for writing Haskell in Emacs

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'haskell)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'subword)

;; TODO polish, and package for Debian
(use-package haskell-tab-indent
  :load-path "~/.emacs.d/site-lisp")

(setq
 ;; indentation preferences
 haskell-indentation-layout-offset 4
 haskell-indentation-left-offset 4
 haskell-indentation-show-indentations nil

 ;; we rely on `haskell-mode-goto-loc' for our M-. binding, but still
 ;; generate a TAGS file for completion
 haskell-tags-on-save t

 ;; this tends to get in the way
 haskell-mode-contextual-import-completion nil

 ;; enable standard features from haskell-mode docs
 haskell-process-suggest-remove-import-lines t
 haskell-process-auto-import-loaded-modules t
 haskell-process-log t

 ;; guess whether this is a stack or pure cabal project
 haskell-process-type 'auto)

;; Use a local hook to turn on an appropriate indentation mode.  Use
;; `haskell-indentation-mode' by default, but if our .dir-locals.el
;; specifies `indent-tabs-mode', we should instead use my
;; `haskell-tab-indent-mode'
(add-hook 'haskell-mode-hook
          (lambda ()
            (add-hook 'hack-local-variables-hook
                      (lambda ()
                        (if indent-tabs-mode
                            (haskell-tab-indent-mode 1)
                          (haskell-indentation-mode 1)))
                      nil t)))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'subword-mode)

(diminish 'interactive-haskell-mode)
(diminish 'subword-mode)

;; standard Haskell repl interaction bindings
(bind-key "C-c C-l" 'haskell-process-load-or-reload  haskell-mode-map)
(bind-key "C-c C-b" 'haskell-interactive-bring       haskell-mode-map)
(bind-key "C-c C-i" 'haskell-process-do-info         haskell-mode-map)
(bind-key "C-c C-c" 'haskell-process-cabal-build     haskell-mode-map)
(bind-key "C-c C-k" 'haskell-interactive-mode-clear  haskell-mode-map)
(bind-key "C-c C"   'haskell-process-cabal           haskell-mode-map)

;; same again for `haskell-cabal-mode'
(bind-key "C-c C-b" 'haskell-interactive-bring       haskell-cabal-mode-map)
(bind-key "C-c C-k" 'haskell-interactive-mode-clear  haskell-cabal-mode-map)
(bind-key "C-c C-c" 'haskell-process-cabal-build     haskell-cabal-mode-map)
(bind-key "C-c C"   'haskell-process-cabal           haskell-cabal-mode-map)

;;; these two bindings require GHCi 8 or newer (or GHCi-ng)

;; jump asynchronously; no need for a TAGS file
(bind-key "M-."     'haskell-mode-goto-loc           interactive-haskell-mode-map)

;; pass C-u to insert a missing type signature
(bind-key "C-c C-t" 'haskell-mode-show-type-at       interactive-haskell-mode-map)

;; ensure that company falls back to dabbrevs when haskell-mode cannot
;; complete, such as in where clauses (this is straight from
;; haskell-mode docs)
(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))

(provide 'init-haskell)
;;; init-haskell.el ends here
