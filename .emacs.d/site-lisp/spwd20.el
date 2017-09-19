;;; spwd20.el --- minor mode for d20 tabletop roleplaying games

;; Copyright (C) 2017  Sean Whitton

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; URL:
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: (s seq dash)
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

;;; A minor mode intended for use in an Org-mode file in which you are
;;; keeping your GM notes for a d20 game.

;;; Example file footer:
;;;
;;;     # Local Variables:
;;;     # eval: (spwd20-mode 1)
;;;     # spwd20-party: (("Zahrat" . 2) ("Ennon" . 4) ("Artemis" . 5))
;;;     # End:

;;; Code:

(require 's)
(require 'seq)
(require 'dash)

(defcustom spwd20-party nil
  "Party initiative modifiers.")

(defcustom spwd20-dice-sound
  "~/lib/annex/doc/sounds/147531__ziembee__diceland.wav"
  "Path to a sound file that `play-sound-file' can play.")

(defvar spwd20-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f9>") 'spwd20-initiative-dwim)
    (define-key map (kbd "<f10>") 'spwd20-damage)
    (define-key map (kbd "<f11>") 'spwd20-roll)
    (define-key map (kbd "<f12>") 'spwd20-d20)
    map)
  "Keymap for `spwd20-mode'.")

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
           (let ((roll (1+ (random (- sides 1)))))
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
         ;; in 5e, all monsters of the same kind have the same
         ;; initiative
         ;; TODO defcustom to toggle this for other editions
         (let ((init (int-to-string
                      (spwd20--roll (concat
                                     "1d20"
                                     (spwd20--num-to-term init-input))))))
           (while (> num-input 0)
             (let ((hp (int-to-string (spwd20--roll hd-input))))
               (push (list
                      "" name-input (spwd20--num-to-term init-input) init hp "0")
                     rows))
             (setq num-input (- num-input 1)))))
       while (-all? (lambda (x) (> (length x) 0))
                    (list name-input init-input hd-input))))
    (dolist (pc spwd20-party)
      (let ((init (read-string (concat (car pc) "'s initiative roll: "))))
        (push (list "" (car pc) (spwd20--num-to-term (cdr pc)) init "-" "-")
              rows)))
    (insert
     "Round of combat: 1\n|Turn|Creature|Mod|Init|HP|Damage|Status|\n|-\n")
    (dolist (row rows)
      (dolist (cell row)
        (insert "|" cell))
      (insert "|\n"))
    (delete-char -1)
    (org-table-goto-column 4)
    (org-table-sort-lines nil ?N)
    (org-table-goto-line 2)
    (org-table-goto-column 1)
    (insert ">>>>")                     ; four chars in 'Turn'
    (org-table-align)))

(defun spwd20-initiative-advance ()
  "Advance the turn tracker in an initiative table."
  (interactive "*")
  (when (org-at-table-p)
    (let* ((back (search-backward ">>>>" (org-table-begin) t))
           (forward (search-forward ">>>>" (org-table-end) t))
           (cur (if back back forward)))
      (goto-char cur)
      (skip-chars-backward ">")
      (delete-char 4)
      (if (save-excursion (org-table-goto-line (1+ (org-table-current-line))))
          (progn
            (forward-line 1)
            (org-table-next-field)
            (insert ">>>>"))
        (save-excursion
          (search-backward "Round of combat:")
          (search-forward-regexp "[0-9]+")
          (skip-chars-backward "0-9")
          (replace-match
           (int-to-string (1+ (string-to-int (match-string 0))))))
        (org-table-goto-line 2)
        (insert ">>>>"))))
  (org-table-align))

(defun spwd20-damage (dmg)
  "Apply damage to the monster/NPC in the initiative table row at point."
  (interactive "*nDamage dealt: ")
  (when (org-at-table-p)
    (org-table-goto-column 6)
    (when (looking-at "[0-9]+")
      (replace-match
       (int-to-string (+ dmg (string-to-int (match-string 0))))))))

(defun spwd20-roll (exp)
  "Prompt, evaluate and display dice roll expression EXP.

Accepts roll20's extension for rolling multiple dice and keeping
the best N of them, e.g., 4d6k3."
  (interactive "sRoll: ")
  (message "%s = %s" exp (int-to-string (spwd20--roll exp)))
  (play-sound-file spwd20-dice-sound))

(defun spwd20-d20 ()
  "Roll two d20, showing result with advantage and disadvantage, and with neither."
  (interactive)
  (let* ((fst (spwd20--roll "1d20"))
         (snd (spwd20--roll "1d20"))
         (fst* (int-to-string fst))
         (snd* (int-to-string snd))
         (adv (if (>= fst snd)
                  (concat (propertize fst* 'face 'bold) " " snd*)
                (concat fst* " " (propertize snd* 'face 'bold))))
         (disadv (if (<= fst snd)
                     (concat (propertize fst* 'face 'bold) " " snd*)
                   (concat fst* " " (propertize snd* 'face 'bold)))))
    (message "No adv./disadv.: %s\t\tWith advantage: %s\t\tWith disadvantage: %s"
             fst* adv disadv))
  (play-sound-file spwd20-dice-sound))

(defun spwd20-initiative-dwim ()
  "Start a new combat or advance the turn tracker, based on point."
  (interactive "*")
  (if (org-at-table-p)
      (spwd20-initiative-advance)
    (spwd20-initiative)))

(defun spwd20--num-to-term (n)
  (let ((k (if (stringp n) (string-to-int n) n)))
    (if (>= k 0)
        (concat "+" (int-to-string k))
      (int-to-string k))))

;;;###autoload
(define-minor-mode spwd20-mode
  "Bind convenience functions for running a d20-like game in an
Org-mode document."
  :lighter " d20")

(provide 'spwd20)
;;; spwd20.el ends here
