;;; eshell-git.el --- git frontend for eshell -*- lexical-binding: t; -*-

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

;; TO DO
;; * clickable git branch
;; * detect requirement of commit mesage (& remove commit-no-edit)
;; * git merge
;; * delete git log button after first commit

;;; Code:

(require 'eshell-git-core)
(require 'eshell-git-status)
(require 'eshell-git-commit)
(require 'eshell-git-log)
(require 'eshell-git-show-commit)
(require 'eshell-git-diff)
(require 'eshell-git-help)
(require 'eshell-git-blame)

(defun eshell-git-start ()
  (fset 'eshell/git (symbol-function 'eshell-git)))

(provide 'eshell-git)

;;; eshell-git.el ends here
