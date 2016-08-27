;;; setup-ido-smex.el --- Setup ido and smex -- deprecated settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shiguang Wang

;; Author: Shiguang Wang <sgwang@dev11120.prn1.facebook.com>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; ido settings
(require 'ido)
(require 'idomenu)
(autoload 'idomenu "idomenu" nil t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-user-filename-at-point t
      ido-max-prospects 10
      ido-everywhere t
      ido-ubiquitous t)
(ido-mode t)

;; using smex for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(provide 'setup-ido-smex)
;;; setup-ido-smex.el ends here
