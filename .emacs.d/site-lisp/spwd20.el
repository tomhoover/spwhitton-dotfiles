;;; spwd20.el --- minor mode for d20 tabletop roleplaying games

;; Copyright (C) 2017  Sean Whitton

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; URL:
;; Version: 0.1
;; Package-Version: 0.1
;; Keywords:

;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 's)
(require 'seq)
(require 'dash)

(defun spwd20--roll (exp)
  "Evaluate dice roll expression EXP.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (let* ((exps (s-slice-at "[+-]" exp))
         (ours (s-chop-prefixes (list "+" "-") (car exps)))
         (sign (if (string= (substring exp 0 1) "-")
                   -1 1)))
    (+ (if (s-index-of "d" ours)
           (let* ((split (s-split "[dk]" ours))
                  (times (string-to-int (seq-elt split 0)))
                  (sides (string-to-int (seq-elt split 1)))
                  (keep (if (> (length split) 2)
                            (string-to-int (seq-elt split 2))
                          nil))
                  (rolls))
             (while (> times 0)
               (let ((roll (+ 1 (random (- sides 1)))))
                 (push roll rolls)))
             (-sum
              (if keep
                  (seq-drop (sort rolls) keep)
                rolls)))
         (string-to-int ours))
       (-sum (seq-map 'spwd20--roll (cdr exps))))))

;;;###autoload
(define-minor-mode spwd20-mode
  ""
  :lighter " d20"
  (when spwd20-mode
    (message "activating")))

(provide 'spwd20)
;;; spwd20.el ends here
