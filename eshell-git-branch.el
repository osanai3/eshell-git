;;; eshell-git-branch.el --- git branch for eshell -*- lexical-binding: t; -*-

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

(define-button-type 'eshell-git-checkout-branch
  'action
  (lambda (button)
    (message
     (eshell-git-invoke-command
      (list "checkout"
            (buffer-substring (button-start button) (button-end button)))))))

(defun eshell-git-format-branch (string)
  (eshell-git-lines-map
   (lambda (line)
     (let ((prefix (substring line 0 2))
           (branch (substring line 2)))
       (concat prefix (eshell-git-button-string branch 'checkout-branch))))
   string))

(defun eshell-git-branch (&rest args)
  (eshell-git-format-branch
   (eshell-git-invoke-command (cons "branch" args))))

(provide 'eshell-git-branch)

;;; eshell-git-branch.el ends here
