;;; eshell-git-core.el --- core function -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defgroup eshell-git nil
  "Git frontend for eshell."
  :group 'convenience)

;; faces

(defgroup eshell-git-faces nil
  "Faces used by eshell-git."
  :group 'eshell-git
  :group 'faces)

;;; utility function

(defun eshell-git-prefixp (prefix string)
  (equal prefix (substring string 0 (min (length string) (length prefix)))))

(defun eshell-git-suffixp (suffix string)
  (equal suffix (substring string (- (min (length string) (length suffix))))))

(cl-assert
 (eshell-git-prefixp "aaa" "aaabbb"))

(cl-assert
 (not (eshell-git-prefixp "aaa" "aabbb")))

(cl-assert
 (not (eshell-git-prefixp "aaa" "aa")))

(cl-assert
 (eshell-git-suffixp "bbb" "aaabbb"))

(cl-assert
 (not (eshell-git-suffixp "bbb" "aaabb")))

(cl-assert
 (not (eshell-git-suffixp "aaa" "aa")))

(defun eshell-git-lines (string)
  (cond ((equal string "") nil)
        ((eshell-git-suffixp "\n" string) (split-string (substring string 0 -1) "\n"))
        (t (split-string string "\n"))))

(cl-assert
 (equal
  (eshell-git-lines "aaa\nbbb\n\nccc\n\n")
  '("aaa" "bbb" "" "ccc" "")))

(cl-assert
 (equal
  (eshell-git-lines "aaa\nbbb")
  '("aaa" "bbb")))

(cl-assert
 (equal
  (eshell-git-lines "")
  nil))

(cl-assert
 (equal
  (eshell-git-lines "\n")
  '("")))

(defun eshell-git-fields (string)
  (cond ((equal string "") nil)
        ((eshell-git-suffixp "\t" string) (split-string (substring string 0 -1) "\t"))
        (t (split-string string "\t"))))

(defun eshell-git-unlines (strings)
  (mapconcat (lambda (line) (concat line "\n")) strings ""))

(cl-assert
 (equal
  (eshell-git-unlines '("aaa" "bbb"))
  "aaa\nbbb\n"))

(defun eshell-git-lines-map (function string)
  (eshell-git-unlines
   (delq nil
         (mapcar function
                 (eshell-git-lines string)))))

(defun eshell-git-to-string (value)
  (cond ((stringp value) value)
        ((numberp value) (number-to-string value))
        (t (error "eshell-git-to-string : Cannot convert to string %S" value))))

(cl-assert
 (equal
  (eshell-git-to-string "aaa")
  "aaa"))
(cl-assert
 (equal
  (eshell-git-to-string 1)
  "1"))

(defun eshell-git-button-string (string type &rest args)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply
       'insert-text-button
       string
       'type (intern (concat "eshell-git-" (symbol-name type)))
       args))))

;;; command invoke function

(defcustom eshell-git-command-option
  '("--no-pager")
  "always add this option to git command")

(defcustom eshell-git-command-config
  '(("man.viewer" . "cat")
    ("man.cat.cmd" . "man -P cat")
    ("core.editor" . "false"))
  "always add this config option to git command")

(defcustom eshell-git-command-config-prefer-original
  '("user.name" "user.email")
  "do not add -c name=value if original config exists")

(defun eshell-git-convert-config-to-option (config)
  (apply
   'append
   (mapcar
    (lambda (entry)
      (unless (and
               (member (car entry) eshell-git-command-config-prefer-original)
               (eshell-git-get-original-config (car entry)))
        (list "-c" (concat (car entry) "=" (cdr entry)))))
    config)))

(cl-assert
 (equal
   (eshell-git-convert-config-to-option '(("aaa" . "bbb") ("ccc" . "ddd")))
  '("-c" "aaa=bbb" "-c" "ccc=ddd")))

(defun eshell-git-build-option (args)
  (append
   eshell-git-command-option
   (eshell-git-convert-config-to-option eshell-git-command-config)
   (mapcar 'eshell-git-to-string args)))

(defconst eshell-git-process-file-function 'process-file)

(defun eshell-git-invoke-command (args)
  "Run git command with args."
  (let ((ret nil))
    (let ((output
           (with-output-to-string
             (setq ret
                   (with-current-buffer
                       standard-output
                     (apply
                      eshell-git-process-file-function "git" nil t nil
                      (eshell-git-build-option args)))))))
      (if (eq ret 0) output
        (throw 'eshell-git-command-error (list output args ret))))))

(let* ((eshell-git-command-config nil)
       (eshell-git-process-file-function
        (lambda (command infile buffer display &rest args)
          (cl-assert (equal command "git"))
          (cl-assert (not infile))
          (cl-assert buffer)
          (cl-assert (not display))
          (cl-assert (equal
                      args
                      (eshell-git-build-option '("-n" "1"))))
          (insert "out")
          0)))
  (cl-assert
   (equal
    (eshell-git-invoke-command '("-n" 1))
    "out")))

(defun eshell-git-get-original-config (name)
  (let* ((eshell-git-command-config nil)
         (output
          (catch 'eshell-git-command-error
            (car (eshell-git-lines
                  (eshell-git-invoke-command (list "config" name)))))))
    (when (stringp output) output)))

;;; user interface function

(defcustom eshell-git-alias-list
  '(("st" . "status")
    ("ci" . "commit")
    ("di" . "diff")
    ("co" . "checkout"))
  "available alias")

(defun eshell-git (subcommand &rest args)
  "Main function to be invoked from eshell."
  (let ((alias (cdr (assoc subcommand eshell-git-alias-list))))
    (if alias
        (cond
         ((stringp alias)
          (apply 'eshell-git-without-alias alias args))
         ((functionp alias)
          (apply alias args))
         ((and (listp alias))
          (apply 'eshell-git-without-alias (append alias args)))
         (t (error "invalid alias definition : %S" alias)))
      (apply 'eshell-git-without-alias subcommand args))))

(defun eshell-git-without-alias (subcommand &rest args)
  (let ((funname (intern (concat "eshell-git-" subcommand))))
    (if (fboundp funname)
        (apply funname args)
      (apply 'eshell-git-plain subcommand args))))

(defun eshell-git-get-buffer (name callback)
  (let ((original-default-directory default-directory))
    (with-current-buffer
        (let* ((name (format "*eshell-git %s*" name))
               (buffer (get-buffer name)))
          (if buffer buffer (generate-new-buffer name)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory original-default-directory)
        (funcall callback)
        (set-buffer-modified-p nil))
      (current-buffer))))

(let ((buffer (eshell-git-get-buffer "test" (lambda () (insert "test")))))
  (cl-assert
   (equal
    (with-current-buffer buffer (buffer-string))
    "test"))
  (cl-assert
   (equal
    (buffer-name buffer)
    "*eshell-git test*"))
  (cl-assert
   (not (buffer-modified-p buffer))))

(defun eshell-git-pop-to-buffer (buffer)
  (pop-to-buffer buffer)
  (goto-char (point-min))
  buffer)

(defun eshell-git-plain (&rest args)
  "Just run git with given arguments."
  (eshell-git-invoke-command args))

(provide 'eshell-git-core)

;;; eshell-git.el ends here
