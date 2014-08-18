;;; eshell-git-show-commit.el --- git show <commit> for eshell -*- lexical-binding: t; -*-

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
(require 'eshell-git-diff)

(define-button-type 'eshell-git-commit
  'action
  (lambda (button)
    (eshell-git-show-commit
     (buffer-substring (button-start button) (button-end button)))))

(defun eshell-git-get-commit-attribute (commit)
  (eshell-git-invoke-command (list "show" "--no-patch" commit)))

(defun eshell-git-get-commit (commit)
  (concat
   (eshell-git-get-commit-attribute commit)
   (eshell-git-propertize-diff
    (eshell-git-invoke-command (list "show" "--format=format:" commit)))))

(defun eshell-git-show-commit (commit)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "show-commit"
    (lambda ()
      (insert
       (eshell-git-get-commit commit))))))

(provide 'eshell-git-show-commit)

;;; eshell-git-show-commit.el ends here
