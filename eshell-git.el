;;; eshell-git.el --- git frontend for eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>
;; Version: 0.1

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

;;; Commentary:
;; This program is git frontend for eshell.
;; * Use synchronous process for fast response in remote host.

;;; Code:

(require 'cl-lib)

;;; utility function

(defun eshell-git-lines (string)
  (split-string string "\n" t))

;;; command invoke function

(defun eshell-git-invoke-command (args)
  "Run git command with args."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply (apply-partially 'process-file "git" nil t nil) args))))

;;; git accessor function

(defun eshell-git-get-status ()
  "Get git status and parse it."
  (mapcar
   (lambda (line) (cons (substring line 3) (substring line 0 2)))
   (eshell-git-lines
    (eshell-git-invoke-command '("status" "--porcelain")))))

;;; user interface function

;;; eshell-git.el ends here
