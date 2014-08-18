;;; eshell-git-status.el --- git status for eshell -*- lexical-binding: t; -*-

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

(defface eshell-git-staged
  '((t :inherit diff-added))
    "Face for staged status."
    :group 'eshell-git-faces)

(defface eshell-git-not-staged
  '((t :inherit diff-removed))
    "Face for not staged status."
    :group 'eshell-git-faces)

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

(defun eshell-git-status ()
  (eshell-git-get-status))

(provide 'eshell-git-status)

;;; eshell-git-status.el ends here
