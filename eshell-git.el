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
;; * do not popup commit-message buffer when commit-message is given in command line
;; * refactoring : file separation
;; * error handling of git command
;; * delete git log button after first commit

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

(defface eshell-git-diff-header
  '((t :inherit diff-header))
  "Face for header line."
  :group 'eshell-git-faces)

(defface eshell-git-diff-file-header
  '((t :inherit diff-file-header))
  "Face for file header line."
  :group 'eshell-git-faces)

(defface eshell-git-diff-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for hunk header line."
  :group 'eshell-git-faces)

(defface eshell-git-diff-added
  '((t :inherit diff-added))
  "Face for added line."
  :group 'eshell-git-faces)

(defface eshell-git-diff-removed
  '((t :inherit diff-removed))
  "Face for removed line."
  :group 'eshell-git-faces)

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

(defun eshell-git-comment-string (string)
  (eshell-git-lines-map (lambda (str) (concat "# " str)) string))

(cl-assert
 (equal
  (eshell-git-comment-string "aaa\nbbb")
  "# aaa\n# bbb\n"))

(defun eshell-git-remove-comment-string (string)
  (eshell-git-lines-map
   (lambda (str) (if (eshell-git-prefixp "#" str) nil str))
   string))

(cl-assert
 (equal
  (eshell-git-remove-comment-string "aaa\n\nbbb\n#ccc")
  "aaa\n\nbbb\n"))

(defun eshell-git-to-string (value)
  (cond ((stringp value) value)
        ((numberp value) (number-to-string value))
        (t (error "eshell-git-to-string : Cannot convert to string %S" value))
   ))

(cl-assert
 (equal
  (eshell-git-to-string "aaa")
  "aaa"))
(cl-assert
 (equal
  (eshell-git-to-string 1)
  "1"))

(define-button-type 'eshell-git-commit
  'action
  (lambda (button)
    (eshell-git-show-commit
     (buffer-substring (button-start button) (button-end button)))))

(define-button-type 'eshell-git-diff
  'action
  (lambda (button)
    (eshell-git-diff (button-get button 'file)))
  'face 'eshell-git-not-staged)

(define-button-type 'eshell-git-diff-cached
  'action
  (lambda (button)
    (eshell-git-diff "--cached" (button-get button 'file)))
  'face 'eshell-git-staged)

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
  (with-output-to-string
    (with-current-buffer
        standard-output
      (apply
       eshell-git-process-file-function "git" nil t nil
       (eshell-git-build-option args)))))

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
          (insert "out"))))
  (cl-assert
   (equal
    (eshell-git-invoke-command '("-n" 1))
    "out")))

(defun eshell-git-get-original-config (name)
  (let* ((eshell-git-command-config nil)
         (output (car (eshell-git-lines
                       (eshell-git-invoke-command (list "config" name))))))
    (unless (equal output "") output)))

;;; git accessor function

(defun eshell-git-propertize-st (line)
  (when (<= 2 (length line))
    (concat
     (let ((file (substring line 3)))
       (if (eshell-git-prefixp "??" line)
           (eshell-git-button-string (substring line 0 2) 'diff 'file file)
         (concat
          (eshell-git-button-string (substring line 0 1) 'diff-cached 'file file)
          (eshell-git-button-string (substring line 1 2) 'diff 'file file))))
     (substring line 2))))

(defun eshell-git-format-status (string)
  (eshell-git-lines-map 'eshell-git-propertize-st string))

(defun eshell-git-get-status ()
  "Get git status and parse it."
  (eshell-git-format-status
    (eshell-git-invoke-command '("status" "--porcelain"))))

(defvar eshell-git-commit-arguments)

(defun eshell-git-do-commit (commit-message)
  "Do git commit."
  (eshell-git-invoke-command
   (append
    (list "commit" "-m" commit-message)
    eshell-git-commit-arguments)))

(defun eshell-git-format-log-line (line)
  (cl-destructuring-bind
      (hash subject) (eshell-git-fields line)
    (concat (eshell-git-button-string hash 'commit) "\t" subject)))

(defcustom eshell-git-log-max-count 20 "max-count option for git log")

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

(defun eshell-git-propertize-diff (string)
  (eshell-git-lines-map
   (lambda (line)
     (cond
      ((or (eshell-git-prefixp "+++ " line)
           (eshell-git-prefixp "--- " line))
       (concat
        (propertize
         (substring line 0 4)
         'face 'eshell-git-diff-header)
        (propertize
         (substring line 4)
         'face '(eshell-git-diff-file-header eshell-git-diff-header))))
      ((eshell-git-prefixp "@@ " line)
       (propertize line 'face 'eshell-git-diff-hunk-header))
      ((eshell-git-prefixp "+" line)
       (propertize line 'face 'eshell-git-diff-added))
      ((eshell-git-prefixp "-" line)
       (propertize line 'face 'eshell-git-diff-removed))
      (t line)))
   string))

(defun eshell-git-get-diff (&rest args)
  (eshell-git-propertize-diff
   (eshell-git-invoke-command (cons "diff" args))))

(defun eshell-git-get-commit-attribute (commit)
  (eshell-git-invoke-command (list "show" "--no-patch" commit)))

(defun eshell-git-get-commit (commit)
  (concat
   (eshell-git-get-commit-attribute commit)
   (eshell-git-propertize-diff
    (eshell-git-invoke-command (list "show" "--format=format:" commit)))))

;;; mode

(define-derived-mode eshell-git-commit-mode text-mode "Eshell-Git-Commit"
  "commit mode used by eshell-git"
  (defun eshell-git-do-commit-from-buffer ()
    (interactive)
    (message
     (eshell-git-do-commit (eshell-git-remove-comment-string (buffer-string))))
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

(defun eshell-git-status ()
  (eshell-git-get-status))

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

(defun eshell-git-default-commit-message ()
  (concat
   "\n\n"
   (eshell-git-comment-string
    (eshell-git-invoke-command '("diff" "--cached" "--stat")))))

(defun eshell-git-commit (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "commit"
    (lambda ()
      (eshell-git-commit-mode)
      (setq-local eshell-git-commit-arguments args)
      (insert (eshell-git-default-commit-message))))))

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
      (insert (apply 'eshell-git-get-diff args))))))

(defun eshell-git-log (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "log"
    (lambda ()
      (insert
       (apply 'eshell-git-get-log args))))))

(defun eshell-git-show-commit (commit)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "show-commit"
    (lambda ()
      (insert
       (eshell-git-get-commit commit))))))

(defun eshell-git-plain (&rest args)
  "Just run git with given arguments."
  (eshell-git-invoke-command args))

(defun eshell-git-start ()
  (fset 'eshell/git (symbol-function 'eshell-git)))

(provide 'eshell-git)

;;; eshell-git.el ends here
