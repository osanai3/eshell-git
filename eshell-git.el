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

;;; TO DO
;; * git log
;; * remove delegation from git ci
;; * alias system

;;; Code:

(require 'cl-lib)

(defgroup eshell-git nil
  "Git frontend for eshell."
  :group 'convenience)

;; faces

(defgroup eshell-git-faces nil
  "Faces used by eshell-git."
  :group 'eshell-git
  :group 'faces)

(defface eshell-git-staged
  '((t :inherit diff-added))
    "Face for staged status."
    :group 'eshell-git-faces)

(defface eshell-git-not-staged
  '((t :inherit diff-removed))
    "Face for not staged status."
    :group 'eshell-git-faces)

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

(defcustom eshell-git-command-config
  '(("man.viewer" . "cat")
    ("man.cat.cmd" . "man -P cat"))
  "always add this config option to git command")

(defun eshell-git-convert-config-to-option (config)
  (apply
   'append
   (mapcar
    (lambda (entry) (list "-c" (concat (car entry) "=" (cdr entry))))
    config)))

(eval-when-compile
  (assert
   (equal
    (eshell-git-convert-config-to-option '(("aaa" . "bbb") ("ccc" . "ddd")))
    '("-c" "aaa=bbb" "-c" "ccc=ddd"))))

(defun eshell-git-build-option (args)
  (append
   eshell-git-command-option
   (eshell-git-convert-config-to-option eshell-git-command-config)
   (mapcar 'eshell-git-to-string args)))

(defconst eshell-git-process-file-function 'process-file)

(defun eshell-git-invoke-command (args)
  "Run git command with args."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply
       (apply-partially eshell-git-process-file-function "git" nil t nil)
       (eshell-git-build-option args)))))

(eval-when-compile
  (let ((eshell-git-process-file-function
         (lambda (command infile buffer display &rest args)
           (assert (equal command "git"))
           (assert (not infile))
           (assert buffer)
           (assert (not display))
           (assert (equal
                    args
                    (eshell-git-build-option '("-n" "1"))))
           (insert "out"))))
    (assert
     (equal
      (eshell-git-invoke-command '("-n" 1))
      "out"
      ))))

;;; git accessor function

(defun eshell-git-parse-status (string)
  (mapcar
   (lambda (line) (cons (substring line 3) (substring line 0 2)))
   (eshell-git-lines string)))

(eval-when-compile
  (assert
   (equal
    (eshell-git-parse-status "MM aaa.el\nA  bbb.el")
    '(("aaa.el" . "MM") ("bbb.el" . "A "))
    )))

(defun eshell-git-get-status ()
  "Get git status and parse it."
  (eshell-git-parse-status
    (eshell-git-invoke-command '("status" "--porcelain"))))

;;; user interface function

(defun eshell-git (subcommand &rest args)
  "Main function to be invoked from eshell."
  (let ((funname (intern (concat "eshell-git-" subcommand))))
  (if (fboundp funname)
      (apply funname args)
    (eshell-git-fallback subcommand args))))

(defun eshell-git-propertize-st (status)
  (if (equal status "??")
      status
    (concat
     (propertize (substring status 0 1) 'face 'eshell-git-staged)
     (propertize (substring status 1 2) 'face 'eshell-git-not-staged))))

(defun eshell-git-format-st (status-alist)
  (eshell-git-unlines
   (mapcar
    (lambda (status)
      (concat (eshell-git-propertize-st (cdr status)) " " (car status)))
    status-alist)))

(eval-when-compile
  (assert
   (equal
    (eshell-git-format-st '(("aaa.el" . "MM") ("bbb.el" . "A ")))
    "MM aaa.el\nA  bbb.el"
    )))

(defun eshell-git-st ()
  (eshell-git-status))

(defun eshell-git-status ()
  (eshell-git-format-st (eshell-git-get-status)))

(defcustom eshell-git-ci-delegate nil "used by eshell-git-ci")

(defun eshell-git-commit (&rest args)
  (if eshell-git-ci-delegate
      (funcall eshell-git-ci-delegate)
    (eshell-git-fallback "commit" args)))

(defun eshell-git-ci (&rest args)
  (apply 'eshell-git-commit args))

(defun eshell-git-help (command)
  (let* ((name (format "*eshell-git help %s*" command))
         (buffer (get-buffer name)))
    (pop-to-buffer
     (if buffer buffer
       (with-current-buffer
           (generate-new-buffer name)
         (insert (eshell-git-invoke-command (list "help" command)))
         (Man-fontify-manpage)
         (Man-mode)
         (set-buffer-modified-p nil)
         (current-buffer))))))

(defun eshell-git-diff (&rest args)
  (with-current-buffer
      (let* ((name "*eshell-git diff*")
             (buffer (get-buffer name)))
        (if buffer buffer (generate-new-buffer name)))
    (erase-buffer)
    (insert (eshell-git-invoke-command (cons "diff" args)))
    (diff-mode)
    (set-buffer-modified-p nil)
    (pop-to-buffer (current-buffer))))

(defun eshell-git-di (&rest args)
  (apply 'eshell-git-diff args))

(defun eshell-git-dc (&rest args)
  (apply 'eshell-git-di (cons "--cached" args)))

(defun eshell-git-fallback (subcommand args)
  "Just run git with given arguments."
  (eshell-git-invoke-command (cons subcommand args))
  )

;;; eshell-git.el ends here
