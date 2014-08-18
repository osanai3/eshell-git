;;; eshell-git-log.el --- git log for eshell -*- lexical-binding: t; -*-

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

(defun eshell-git-format-log-line (line)
  (cl-destructuring-bind
      (hash subject) (eshell-git-fields line)
    (concat (eshell-git-button-string hash 'commit) "\t" subject)))

(defcustom eshell-git-log-max-count 20 "max-count option for git log"
  :group 'eshell-git)

(defun eshell-git-get-log (&rest args)
  "Get git log and format it."
  (concat
   (apply 'eshell-git-get-log-partially eshell-git-log-max-count 0 args)
   (apply 'eshell-git-log-button eshell-git-log-max-count eshell-git-log-max-count args)))

(defun eshell-git-get-log-partially (max-count skip &rest args)
  (eshell-git-lines-map
   'eshell-git-format-log-line
   (eshell-git-invoke-command
    (append
     (list
      "log"
      "--format=tformat:%h\t%s"
      (format "--max-count=%d" max-count)
      (format "--skip=%d" skip))
     args))))

(define-button-type 'eshell-git-append-log
  'action
  (lambda (button)
    (let ((start (button-start button))
          (end (button-end button))
          (max-count (button-get button 'max-count))
          (skip (button-get button 'skip))
          (args (button-get button 'args)))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert
         (apply 'eshell-git-get-log-partially max-count skip args)
         (apply 'eshell-git-log-button max-count (+ skip max-count) args))))))

(defun eshell-git-log-button-string (max-count skip &rest args)
  (format "more (max-count=%d skip=%d args=%S)" max-count skip args))

(defun eshell-git-log-button (max-count skip &rest args)
  (eshell-git-button-string
   (apply 'eshell-git-log-button-string max-count skip args)
   'append-log
   'max-count max-count
   'skip skip
   'args args))

(defun eshell-git-log (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "log"
    (lambda ()
      (insert
       (apply 'eshell-git-get-log args))))))

(provide 'eshell-git-log)

;;; eshell-git-log.el ends here
