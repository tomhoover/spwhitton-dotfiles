;;; org-mairix-el.el --- create Org links to mairix.el searches

;; Copyright (C) 2014  Sean Whitton

;; Author: Sean Whitton <spw@sdf.org>
;; Keywords: mail

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;;; Commentary:

;; Leverage existing mairix.el to link to particular mail messages,
;; unlike org-mairix.el which uses Gnus or mutt to view the resultant
;; message

;;; Code:

(require 'org)
(require 'mairix)
(require 'f)
(require 's)

(defvar org-mairix-el-store "~/.org-mairix-el-link")

(org-add-link-type "mairixel" 'org-mairix-el-open)

(defun org-mairix-el-open (search)
  "Function to open a mairix link SEARCH."
  (mairix-search (concat "m:" search) nil))

(defun org-mairix-el-link ()
  "Return a link with description DESCRIPTION to a message by its ID."
  (interactive )
  (when (f-exists? org-mairix-el-store)
    (let ((message-id (s-trim (f-read org-mairix-el-store))))
      (concat "[[mairixel:"
              message-id
              "]["
              (read-string "E-mail link description: ")
              "]]"))))

(defun org-mairix-el-insert-link ()
  "Store a link to a message by its ID."
  (interactive)
  (when (f-exists? org-mairix-el-store)
    (let ((message-id (s-trim (f-read org-mairix-el-store))))
      (org-insert-link nil (concat "mairixel:" message-id)))))

(provide 'org-mairix-el)
;;; org-mairix-el ends here
