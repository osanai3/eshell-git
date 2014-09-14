;;; eshell-git-blame.el --- git blame for eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'eshell-git-core)
(require 'eshell-git-show-commit)

;; provide minir mode (insert clickable text, inhibit modification, turn off modified flag)
;; invocation from eshell and buffer
;; inherit color, major mode, etc...

(defvar eshell-git-blame-tag-length 8)

(defun eshell-git-get-blame-lines (file)
  (eshell-git-lines-map
   (lambda (line)
     (eshell-git-button-string
      (substring line 0 eshell-git-blame-tag-length)
      'commit))
   (eshell-git-invoke-command (list "blame" "-s" "--root" file))))

(defun eshell-git-insert-blame-commit ()
  (let ((localname
         (if (tramp-tramp-file-p (buffer-file-name))
             (tramp-file-name-localname
              (tramp-dissect-file-name (buffer-file-name)))
           (buffer-file-name))))
    (save-excursion
      (goto-char 0)
      (mapc
       (lambda (button)
         (insert button " ") (forward-line 1) (beginning-of-line))
       (eshell-git-lines (eshell-git-get-blame-lines localname))))))

(defun eshell-git-delete-blame-commit ()
  (save-excursion
    (goto-char 0)
    (while (not (= (point) (point-max)))
      (delete-char (+ eshell-git-blame-tag-length 1))
      (forward-line 1)
      )))

(define-minor-mode eshell-git-blame-mode "blame mode for eshell-git"
  nil nil nil
  (if eshell-git-blame-mode
      (if (buffer-modified-p)
          (progn
            (setq eshell-git-blame-mode nil)
            (error "try after save buffer"))
        (eshell-git-insert-blame-commit)
        (read-only-mode 1)
        (set-buffer-modified-p nil))
    (read-only-mode 0)
    (eshell-git-delete-blame-commit)
    (set-buffer-modified-p nil)))

(defun eshell-git-blame (file)
  (find-file file)
  (eshell-git-blame-mode))

(provide 'eshell-git-blame)

;;; eshell-git-blame.el ends here
