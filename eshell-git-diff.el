;;; eshell-git-diff.el --- git diff for eshell -*- lexical-binding: t; -*-

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

(defun eshell-git-diff (&rest args)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    "diff"
    (lambda ()
      (insert (apply 'eshell-git-get-diff args))))))

(provide 'eshell-git-diff)

;;; eshell-git-diff.el ends here
