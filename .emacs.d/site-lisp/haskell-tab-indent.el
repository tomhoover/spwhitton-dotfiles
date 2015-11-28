;;; haskell-tab-indent --- blah blah

;;; Commentary:

;;; Code:

(defun haskell-tab-indent ()
  "Blah blah."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (looking-at "where")
	(haskell-tab-indent-where)
      (haskell-tab-indent-cycle))))

(defun haskell-tab-indent-where ()
  ;; `haskell-tab-indent' leaves us just after the indentation
  (delete-region (line-beginning-position) (point))
  (insert "  "))

(defun haskell-tab-indent-cycle ()
  (let ((previous-line-tabs (haskell-tab-indent-previous-line-tabs))
        (this-line-tabs (haskell-tab-indent-this-line-tabs)))
    (if (= (1+ previous-line-tabs) this-line-tabs)
        (haskell-tab-indent-reset)
      (haskell-tab-indent-indent))))

(defun haskell-tab-indent-reset ()
  (save-excursion
    (back-to-indentation)
    (delete-region (line-beginning-position) (point))))

(defun haskell-tab-indent-indent ()
  (save-excursion
    (back-to-indentation)
    (insert "\t")))

(defun haskell-tab-indent-previous-line-tabs ()
  (save-excursion
    (forward-line -1)
    (haskell-tab-indent-this-line-tabs)))

(defun haskell-tab-indent-this-line-tabs ()
  (save-excursion
    (save-restriction
      (back-to-indentation)
      (narrow-to-region (line-beginning-position) (point))
      (beginning-of-line)
      (let ((count 0))
	(while (re-search-forward "\t" nil t)
	  (setq count (1+ count)))
	count))))

;;;###autoload
(define-minor-mode haskell-tab-indent-mode
  "Blah blah."
  :lighter " TabInd"
  (kill-local-variable 'indent-line-function)
  (when haskell-tab-indent-mode
    (set (make-local-variable 'indent-line-function) 'haskell-tab-indent)
    (set (make-local-variable 'indent-tabs-mode) t)))

(provide 'haskell-tab-indent)
;;; haskell-tab-indent.el ends here
