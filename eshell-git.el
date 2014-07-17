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
;; * Use synchronous process for fast response in remote host via tramp.
;;     * You cannot input ssh passphrase interactively. Use ssh-agent forwarding if necessary.
;; * Use emacs buffer instead of git pager.

;; TO DO
;; * add default commit message
;; * show detail of each commit in log buffer
;; * gradual get git log

;;; Code:

(require 'cl-lib)
(require 'man)

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
    ("man.cat.cmd" . "man -P cat")
    ("core.editor" . "false"))
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

(defun eshell-git-do-commit (commit-message)
  "Do git commit."
  (eshell-git-invoke-command (list "commit" "-m" commit-message)))

(defconst eshell-git-log-format-param "%h%n%an%n%ar%n%s")

(defun eshell-git-parse-log (string)
  (eshell-git-parse-log-lines (eshell-git-lines string)))

(defun eshell-git-parse-log-lines (lines)
  (when lines
    (cl-destructuring-bind
        (hash author-name author-date subject &rest rest) lines
      (cons
       (list
        (cons 'hash hash)
        (cons 'author-name author-name)
        (cons 'author-date author-date)
        (cons 'subject subject))
       (eshell-git-parse-log-lines rest)))))

(defun eshell-git-get-log (&rest args)
  "Get git log and parse it."
  (let ((format-argument
         (format "--format=format:%s" eshell-git-log-format-param)))
    (eshell-git-parse-log
     (eshell-git-invoke-command
      (cons "log" (cons format-argument args))))))

;;; mode

(define-derived-mode eshell-git-commit-mode text-mode "Eshell-Git-Commit"
  "commit mode used by eshell-git"
  (defun eshell-git-do-commit-from-buffer ()
    (interactive)
    (message (eshell-git-do-commit (buffer-string)))
    (kill-buffer))
  (define-key eshell-git-commit-mode-map (kbd "C-c C-c") 'eshell-git-do-commit-from-buffer))

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
          (apply 'eshell-git-without-alias (cons alias args)))
         ((functionp alias)
          (apply alias args))
         ((and (listp alias))
          (apply 'eshell-git-without-alias (append alias args)))
         (t (error "invalid alias definition : %S" alias)))
      (apply 'eshell-git-without-alias (cons subcommand args)))))

(defun eshell-git-without-alias (subcommand &rest args)
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

(defun eshell-git-status ()
  (eshell-git-format-st (eshell-git-get-status)))

(defun eshell-git-get-buffer (name callback)
  (with-current-buffer
      (let* ((name (format "*eshell-git %s*" name))
             (buffer (get-buffer name)))
        (if buffer buffer (generate-new-buffer name)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (funcall callback)
      (set-buffer-modified-p nil))
    (current-buffer)))

(eval-when-compile
  (let ((buffer (eshell-git-get-buffer "test" (lambda () (insert "test")))))
    (assert
     (equal
      (with-current-buffer buffer (buffer-string))
      "test"))
    (assert
     (equal
      (buffer-name buffer)
      "*eshell-git test*"))
    (assert
     (not (buffer-modified-p buffer)))))

(defun eshell-git-pop-to-buffer (buffer)
  (pop-to-buffer buffer)
  (goto-char (point-min))
  buffer)

(defun eshell-git-commit ()
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "commit"
    (lambda ()
      (eshell-git-commit-mode)))))

(defun eshell-git-help (command)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    (format "help %s" command)
    (lambda ()
      (insert (eshell-git-invoke-command (list "help" command)))
      (Man-fontify-manpage)
      (Man-mode)
      ))))

(defun eshell-git-diff (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "diff"
    (lambda ()
      (insert (eshell-git-invoke-command (cons "diff" args)))
      (diff-mode)))))

(defun eshell-git-format-log (log-list)
  (eshell-git-unlines
   (mapcar
    (lambda (log)
      (concat
       (cdr (assoc 'hash log))
       "\t"
       (cdr (assoc 'subject log))))
    log-list)))

(defun eshell-git-log (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "log"
    (lambda ()
      (insert
       (eshell-git-format-log
        (apply 'eshell-git-get-log args)))))))

(defun eshell-git-fallback (subcommand args)
  "Just run git with given arguments."
  (eshell-git-invoke-command (cons subcommand args)))

(defun eshell-git-start ()
  (fset 'eshell/git (symbol-function 'eshell-git)))

(provide 'eshell-git)

;;; eshell-git.el ends here
