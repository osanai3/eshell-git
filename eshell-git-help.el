;;; eshell-git-help.el --- git help for eshell -*- lexical-binding: t; -*-

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
(require 'man)

(defun eshell-git-help (command)
  (eshell-git-pop-to-buffer
   (eshell-git-get-buffer
    (format "help %s" command)
    (lambda ()
      (insert (eshell-git-invoke-command (list "help" command)))
      (Man-fontify-manpage)
      (Man-mode)
      ))))

(provide 'eshell-git-help)

;;; eshell-git-help.el ends here
