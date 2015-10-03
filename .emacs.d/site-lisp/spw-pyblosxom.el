;;; spw-pyblosxom --- helper functions for Sean's Org-mode-managed Pyblosxom blog

;;; Commentary:

;;; automate repetitive tasks

;;; Code:

(require 'magit)
(require 'f)
(require 's)

(defvar spw-pyblosxom-image-extensions '("png" "jpg" "jpeg")
  "Extensions for image files that may be inserted with `spw-pyblosxom-insert-image'.")

(defun spw-pyblosxom--image-extensions-regexp ()
  "Put `spw-pyblosxom-image-extensions' into a regexp disjunction."
  (let ((regexp ""))
    (dolist (elt spw-pyblosxom-image-extensions regexp)
        (if (string= regexp "")
            (setq regexp elt)
          (setq regexp (concat regexp "\\|" elt))))))

(defun spw-pyblosxom--get-images (&optional nothumbs)
  "Return a list of image files inserted in the buffer with `spw-pyblosxom-insert-image'.
With optional argument NOTHUMBS, exclude thumbnail files."
  (let ((images))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp (concat
                                     "\\[http:\\/\\/spw.sdf.org\\/blog\\/\\(.*?\\("
                                     (spw-pyblosxom--image-extensions-regexp)
                                     "\\)\\)\\]") nil t)
        (if (not (and nothumbs (string-match-p (concat "thumb\\.\\("
                                                       (spw-pyblosxom--image-extensions-regexp)
                                                       "\\)")
                                               (match-string-no-properties 1))))
            (add-to-list 'images (concat "~/doc/www/blog/"
                                         (match-string-no-properties 1))))))
    images))

(defun spw-pyblosxom-publish ()
  "Set the rdate.py-readable blog publishing timestamp, stage in git and publish."
  (interactive)
  ;; 1. set the publication date for rdate.py script
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^#\\+HTML: #published [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\(\\?\\?:\\?\\?:\\?\\?\\|[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")
        (replace-match (concat "#+HTML: #published "
                               (format-time-string "%Y-%m-%d %H:%M:00" nil t)))
      (message "entry yasnippet broken")))
  (save-buffer)
  ;; 2. stage and publish images and post
  (let ((to-publish (cons
                     (buffer-file-name)
                     (spw-pyblosxom--get-images))))

    (dolist (elt to-publish)
      (org-publish-file elt)
      (magit-stage-file (f-relative elt (magit-toplevel)))))
  ;; 3. open a magit status buffer
  (magit-status (magit-toplevel)))

(defun spw-pyblosxom-insert-image (file arg)
  "Insert image file FILE into one of my blog posts.
With prefix argument ARG, thumbnail it."
  (interactive "fimage file: \nP")
  ;; 1. copy image file into right directory with sensible filename
  (let ((images (spw-pyblosxom--get-images t)))
    (let  ((images-count (length images)))
      (let ((this-image (concat (f-base buffer-file-name)
                                (number-to-string (+ images-count 1))
                                "."
                                (f-ext file)))
            (this-image-thumb (concat (f-base buffer-file-name)
                                      (number-to-string (+ images-count 1))
                                      "thumb."
                                      (f-ext file))))

        (copy-file file this-image)
        ;; 2. unless arg, create thumbnail
        (when (not arg)
          (let ((image-size (split-string (shell-command-to-string
                                           (concat
                                            "identify -format \"%w %h\" "
                                            this-image)))))
            (let ((image-width (string-to-number (elt image-size 0)))
                  (image-height (string-to-number (elt image-size 1))))
              (let ((resize-string (if (< image-height image-width)
                                       "500x" "x500")))

                (call-process-shell-command "convert" nil nil nil
                                            this-image
                                            "-resize"
                                            resize-string
                                            this-image-thumb)))))
        ;; 3. insert link into post at point
        (insert "[[http://spw.sdf.org/blog"
                (replace-regexp-in-string
                 "/home/swhitton/doc/www/blog"
                 "" default-directory)
                this-image
                "]")
        (when (not arg)
          (insert "[http://spw.sdf.org/blog"
                  (replace-regexp-in-string
                   "/home/swhitton/doc/www/blog"
                   "" default-directory)
                  this-image-thumb
                  "]"))
        (insert "]")))))

(defun spw-pyblosxom-insert-page-link (link)
  "Grab HTML <title> of LINK, ask user to edit and then insert Org URI link."
  (interactive "sPaste URL: ")
  (let ((title (s-trim (shell-command-to-string (concat "httphtmltitle.py " link)))))
    (let ((edited-title (read-string "Edit title: " title)))
      (insert "[[" link "][" edited-title  "]]"))))

;;;###autoload
(defun spw-pyblosxom-org-mode-hook ()
  "Perhaps enable spw-pyblosxom-mode when firing up Org-mode."
  (if (or load-file-name buffer-file-name)
      (if (string-match-p (concat (substitute-in-file-name "$HOME") "/doc/www/blog/")
                          (file-name-directory (or load-file-name buffer-file-name)))
          (spw-pyblosxom-mode))))

;;;###autoload
(define-minor-mode spw-pyblosxom-mode
  "Helper functions for my Org-mode-managed Pyblosxom blog."
  :lighter " spwPb"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c B B") 'spw-pyblosxom-publish)
            (define-key map (kbd "C-c B i") 'spw-pyblosxom-insert-image)
            (define-key map (kbd "C-c B l") 'spw-pyblosxom-insert-page-link)
            map))

;;;###autoload
(add-hook 'org-mode-hook 'spw-pyblosxom-org-mode-hook)

(provide 'spw-pyblosxom)
;;; spw-pyblosxom.el ends here
