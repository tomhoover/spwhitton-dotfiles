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

;;;###autoload
(define-minor-mode spwd20-mode
  ""
  :lighter " d20"
  (when spwd20-mode
    (message "activating")))

(provide 'spwd20)
;;; spwd20.el ends here
