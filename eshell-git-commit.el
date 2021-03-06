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
    (message "%s"
     (apply 'eshell-git-do-commit
            (buffer-substring (point-min) (button-start button))
            (button-get button 'eshell-git-commit-argument)))
    (kill-buffer)))

(defun eshell-git-commit-initial-buffer-content (default-commit-message args)
  (concat
   default-commit-message
   "\n\n"
   (eshell-git-button-string "commit" 'do-commit-by-above-message
                             'eshell-git-commit-argument args)
   "\n"
   (eshell-git-get-diff "--cached")))

(defun eshell-git-commit (&rest args)
  (let ((default-commit-message (eshell-git-default-commit-message args)))
    (if default-commit-message
        (eshell-git-pop-to-buffer
         (eshell-git-get-buffer
          "commit"
          (lambda ()
            (insert
             (eshell-git-commit-initial-buffer-content
              default-commit-message args)))))
      (eshell-git-invoke-command (cons "commit" args)))))

(defun eshell-git-default-commit-message (args)
  "Return default commit message. If edit buffer is not required, return nil."
  ;; TODO : detect --reuser-message, --file, -c, --reedit-message, -t, --template, -e, --edit
  ;; FIXME : misjudge "git commit -t -m"
  (cond
   ((or (member "-m" args)
        (member "--no-edit" args)
        (member "-C" args)
        (member "-F" args))
    nil)
   ((member "--amend" args) (eshell-git-get-commit-message "HEAD"))
   (t "")))

(defun eshell-git-get-commit-message (commit)
  (eshell-git-invoke-command (list "log" "-n" 1 "--pretty=format:%B" commit)))

(provide 'eshell-git-commit)

;;; eshell-git-commit.el ends here
