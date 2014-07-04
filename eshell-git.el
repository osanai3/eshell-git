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

(defgroup eshell-git nil "Git frontend for eshell." :group 'convenience)

;;; utility function

(defun eshell-git-lines (string)
  (split-string string "\n" t))

(eval-when-compile
  (assert
   (equal
    (eshell-git-lines "aaa\nbbb\n")
    '("aaa" "bbb"))))

(defun eshell-git-unlines (strings)
  (mapconcat 'identity strings "\n"))

(eval-when-compile
  (assert
   (equal
    (eshell-git-unlines '("aaa" "bbb"))
    "aaa\nbbb")))


(defun eshell-git-to-string (value)
  (cond ((stringp value) value)
        ((numberp value) (number-to-string value))
        (t (error "eshell-git-to-string : Cannot convert to string %S" value))
   ))

(eval-when-compile
  (assert
   (equal
    (eshell-git-to-string "aaa")
    "aaa"))
  (assert
   (equal
    (eshell-git-to-string 1)
    "1"))
  )

;;; command invoke function

(defcustom eshell-git-command-option
  '("--no-pager")
  "always add this option to git command")

(defun eshell-git-invoke-command-helper (process-file-function args)
  "Run git command with args."
  (let ((string-args (mapcar 'eshell-git-to-string args)))
    (with-output-to-string
      (with-current-buffer
          standard-output
        (apply
         (apply-partially process-file-function "git" nil t nil)
         (append eshell-git-command-option string-args))))))

(defun eshell-git-invoke-command (args)
  (eshell-git-invoke-command-helper 'process-file args))

(eval-when-compile
  (assert
   (equal
    (eshell-git-invoke-command-helper
     (lambda (command infile buffer display &rest args)
       (assert (equal command "git"))
       (assert (not infile))
       (assert buffer)
       (assert (not display))
       (assert (equal args (append eshell-git-command-option '("-n" "1"))))
       (insert "out"))
     '("-n" 1))
    "out"
    )))

;;; git accessor function

(defun eshell-git-get-status ()
  "Get git status and parse it."
  (mapcar
   (lambda (line) (cons (substring line 3) (substring line 0 2)))
   (eshell-git-lines
    (eshell-git-invoke-command '("status" "--porcelain")))))

;;; user interface function

(defcustom subcommand-list
  '("st")
  "available subcommand list" :group 'eshell-git)

(defun eshell-git (subcommand &rest args)
  "Main function to be invoked from eshell."
  (if (member subcommand subcommand-list)
      (apply (intern (concat "eshell-git-" subcommand)) args)
    (eshell-git-fallback subcommand args)))

(defun eshell-git-st ()
  (eshell-git-unlines
   (mapcar
    (lambda (status) (concat (cdr status) " " (car status)))
    (eshell-git-get-status))))

(defun eshell-git-fallback (subcommand args)
  "Just run git with given arguments."
  (eshell-git-invoke-command (cons subcommand args))
  )

;;; eshell-git.el ends here
