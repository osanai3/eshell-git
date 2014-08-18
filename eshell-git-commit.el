;;; eshell-git-commit.el --- git commit for eshell -*- lexical-binding: t; -*-

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

(defun eshell-git-do-commit (commit-message &rest args)
  "Do git commit."
  (eshell-git-invoke-command
   (append
    (list "commit" "-m" commit-message)
    args)))

(define-button-type 'eshell-git-do-commit-by-above-message
  'action
  (lambda (button)
    (message
     (apply 'eshell-git-do-commit
            (buffer-substring (point-min) (button-start button))
            (button-get button 'eshell-git-commit-argument)))
    (kill-buffer)))

(defun eshell-git-default-commit-message (args)
  (concat
   "\n\n"
   (eshell-git-button-string "commit" 'do-commit-by-above-message
                             'eshell-git-commit-argument args)
   "\n"
   (eshell-git-get-diff "--cached")))

(defun eshell-git-commit (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "commit"
    (lambda ()
      (insert (eshell-git-default-commit-message args))))))

(defun eshell-git-commit-no-edit (&rest args)
  (eshell-git-invoke-command (append (list "commit" "--no-edit") args)))


(provide 'eshell-git-commit)

;;; eshell-git-commit.el ends here
