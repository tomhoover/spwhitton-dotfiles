;;; pomodoro.el --- Pomodoro Technique implementation for emacs -*- lexical-binding: t -*-

;; Author: Victor Deryagin
;; Copyright (C) 2011-2013 Victor Deryagin <vderyagin@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;;
;; Information on installation and usage can be found in the README.md file
;;

;;; Code:

(require 'notifications)

(defvar pomodoro-work-duration 25 "Time in minutes of work.")
(defvar pomodoro-short-break-duration 5 "Time in minutes of short break.")
(defvar pomodoro-long-break-duration 15 "Time in minutes of long break.")
(defvar pomodoro-set-number 4 "Number of sets until a long break.")
(defvar pomodoro-icon notifications-application-icon "Icon used for notification.")

(defvar pomodoro-state-change-hook '(pomodoro-status))
(defvar pomodoro-update-hook '(pomodoro-update-modeline))

(defvar pomodoro-work-start-hook nil "Hooks run on beginning of each work set.")
(defvar pomodoro-pause-start-hook nil "Hooks run on beginning of each pause.")

(defvar pomodoro-display-string "")
(defvar pomodoro-current-set nil "Current set number.")
(defvar pomodoro-minutes-left nil "Minutes left to state change.")
(defvar pomodoro-current-state nil "Current state.")
(defvar pomodoro-timer nil)

;;;###autoload
(defun pomodoro ()
  "Start pomodoro timer.  Ask to restart if already running."
  (interactive)
  (if (pomodoro-running-p)
      (when (y-or-n-p "Pomodoro is already running.  Restart it? ")
        (pomodoro--stop)
        (pomodoro))
    (setq pomodoro-minutes-left 0
          pomodoro-current-state 'long-break
          pomodoro-timer (run-at-time nil 60 'pomodoro-update))))

(defun pomodoro-rewind ()
  "Rewind pomodoro timer to the beginning of the current set."
  (interactive)
  (pomodoro-start-work)
  (pomodoro-update))

(defun pomodoro-skip-forward ()
  "Skip to the beginning of the next set."
  (interactive)
  (pomodoro-next-set)
  (pomodoro-rewind))

(defun pomodoro--stop ()
  "Stop pomodoro timer."
  (when (timerp pomodoro-timer)
    (cancel-timer pomodoro-timer))
  (setq pomodoro-minutes-left nil
        pomodoro-current-set nil
        pomodoro-current-state nil
        pomodoro-timer nil))

(defun pomodoro-stop ()
  "Stop pomodoro timer and run hooks to update modeline and display notification."
  (interactive)
  (if (pomodoro-running-p)
      (progn
        (pomodoro--stop)
        (run-hooks 'pomodoro-update-hook 'pomodoro-state-change-hook))
    (message "Pomodoro is not running.")))

(defun pomodoro-last-set-p ()
  "Return t if current work set is the last in cycle, nil otherwise."
  (= pomodoro-current-set (1- pomodoro-set-number)))

(defun pomodoro-start-break ()
  "Start break (short or long, depending on whether current set is the last in the cycle)."
  (if (pomodoro-last-set-p)
      (setq pomodoro-minutes-left pomodoro-long-break-duration
            pomodoro-current-state 'long-break)
    (setq pomodoro-minutes-left pomodoro-short-break-duration
          pomodoro-current-state 'short-break))
  (run-hooks 'pomodoro-state-change-hook 'pomodoro-pause-start-hook))

(defun pomodoro-start-work ()
  "Start work."
  (setq pomodoro-minutes-left pomodoro-work-duration
        pomodoro-current-state 'work)
  (run-hooks 'pomodoro-state-change-hook 'pomodo-work-start-hook))

(defun pomodoro-next-set ()
  "Move on to the next set, start from the beginning if current set is the last in the cycle."
  (setq pomodoro-current-set
        (if pomodoro-current-set
            (% (1+ pomodoro-current-set) pomodoro-set-number)
          0)))

(defun pomodoro-update ()
  "Update timer status, transition between states if needed.
Called every minute by `pomodoro-timer'."
  (when (= pomodoro-minutes-left 0)
    (if (eq pomodoro-current-state 'work)
        (pomodoro-start-break)
      (progn
        (pomodoro-next-set)
        (pomodoro-start-work))))
  (run-hooks 'pomodoro-update-hook)
  (setq pomodoro-minutes-left (1- pomodoro-minutes-left)))

(defun pomodoro-running-p ()
  "Predicate used to test whether pomodoro timer is running or not."
  (memq pomodoro-timer timer-list))

(defun pomodoro-status ()
  "Display notification with current pomodoro timer status."
  (interactive)
  (let ((notification-body ""))
    (when (pomodoro-running-p)
      (setq notification-body
            (concat
             (format "%d set\n" (1+ pomodoro-current-set))
             (format "%d minute(s) left" pomodoro-minutes-left))))
    (notifications-notify
     :title    (pomodoro-current-state)
     :body     notification-body
     :app-icon pomodoro-icon)))


(defun pomodoro-current-state ()
  "Current pomodoro state as string."
  (cond
   ((eq pomodoro-current-state 'work)
    "Work")
   ((eq pomodoro-current-state 'short-break)
    "Short break")
   ((eq pomodoro-current-state 'long-break )
    "Long break")
   (t
    "Not running")))

(defun pomodoro-update-modeline ()
  "Update the modeline."
  (if (pomodoro-running-p)
      (progn
        (setq pomodoro-display-string (pomodoro-display-string))
        (unless global-mode-string (setq global-mode-string '("")))
        (add-to-list 'global-mode-string 'pomodoro-display-string 'append))
    (setq global-mode-string
          (delq 'pomodoro-display-string global-mode-string)))
  (force-mode-line-update))


(defun pomodoro-display-string ()
  "Return modeline display string for current status of pomodoro timer."
  (cond
   ((eq pomodoro-current-state 'work)
    (format "W%d-%d"
            (1+ pomodoro-current-set)
            pomodoro-minutes-left))
   ((eq pomodoro-current-state 'short-break)
    (format "B%d-%d"
            (1+ pomodoro-current-set)
            pomodoro-minutes-left))
   ((eq pomodoro-current-state 'long-break)
    (format "LB-%d" pomodoro-minutes-left))))

(provide 'pomodoro)

;;; pomodoro.el ends here
