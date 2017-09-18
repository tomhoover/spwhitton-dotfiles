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

(defcustom spwd20-party nil
  "Party initiative modifiers.")

(defun spwd20--roll (exp)
  "Evaluate dice roll expression EXP.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (let ((exps (seq-map (lambda (s) (s-chop-prefix "+" s))
                       (s-slice-at "[+-]" exp))))
    (-sum (seq-map 'spwd20--roll-inner exps))))

(defun spwd20--roll-inner (exp)
  (let* ((sign (if (s-prefix-p "-" exp) -1 1))
         (ours (s-chop-prefix "-" exp))
         (split (seq-map 'string-to-int (s-split "[dk]" ours)))
         (times (seq-elt split 0))
         (sides (ignore-errors (seq-elt split 1)))
         (keep (ignore-errors (seq-elt split 2)))
         (rolls))
    (* sign
       (if (not sides)
           times
         (while (> times 0)
           (let ((roll (+ 1 (random (- sides 1)))))
             (push roll rolls))
           (setq times (- times 1)))
         (-sum
          (if keep
              (seq-drop (sort rolls '<) (- times keep))
            rolls))))))

(defun spwd20-initiative ()
  "Generates an Org-mode table with initiative order and monster HP."
  (interactive "*")
  (let ((rows))
    (let (name-input init-input hd-input num-input)
      (loop
       do (setq name-input (read-string "Monster/NPC name (blank when done): "))
       (when (> (length name-input) 0)
         (setq init-input (read-string (concat name-input "'s init modifier: "))
               hd-input (read-string (concat name-input "'s hit points: "))
               num-input (string-to-int
                          (read-string (concat "How many " name-input "? "))))
         (while (> num-input 0)
           (let ((init (int-to-string
                        (spwd20--roll (concat
                                       "1d20"
                                       (spwd20--num-to-term init-input)))))
                 (hp (int-to-string (spwd20--roll hd-input))))
             (push (list "" name-input init-input init hp "0") rows))
           (setq num-input (- num-input 1))))
       while (-all? (lambda (x) (> (length x) 0))
                    (list name-input init-input hd-input))))
    (dolist (pc spwd20-party)
      (let ((init (read-string (concat (car pc) "'s initiative roll: "))))
        (push (list "" (car pc) (int-to-string (car (cdr pc))) init "-" "-")
              rows)))
    (insert "Round of combat: 1\n\n|Creature|Mod|Init|HP|Damage|Status|\n|-\n")
    (dolist (row rows)
      (dolist (cell row)
        (insert "|" cell))
      (insert "|\n"))))

(defun spwd20--num-to-term (n)
  (if (>= n 0)
      (concat "+" (int-to-string n))
    (int-to-string n)))

;;;###autoload
(define-minor-mode spwd20-mode
  ""
  :lighter " d20"
  (when spwd20-mode
    (message "activating")))

(provide 'spwd20)
;;; spwd20.el ends here
