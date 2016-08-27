;;; my-keymap.el --- Keymaps                         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shiguang Wang

;; Author: Shiguang Wang <sgwang@dev11120.prn1.facebook.com>
;; Keywords:

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

;;;
;;; define the command prefix C-z
;;;
(define-prefix-command 'ctl-z-map)
(global-set-key (kbd "C-z") 'ctl-z-map)

;;;
;;; set regexp incremental search
;;;
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;;;
;;; Toggle comment
;;;
(global-set-key (kbd "<f8>") 'comment-or-uncomment-region)
(global-set-key (kbd "C-z b") 'comment-or-uncomment-region)

;;;
;;; Toggle Hide/Show
;;;
(global-set-key (kbd "<f7>") 'hs-toggle-hiding)
(global-set-key (kbd "C-z h") 'hs-toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

;;;
;;; buffer freely moves
;;;
(global-set-key (kbd "C-z i") 'buf-move-up)
(global-set-key (kbd "C-z k") 'buf-move-down)
(global-set-key (kbd "C-z j") 'buf-move-left)
(global-set-key (kbd "C-z l") 'buf-move-right)

;;;
;;; linum
;;;
(global-set-key (kbd "C-z ;") 'linum-mode)

;;;
;;; embedded speedbar to the left
;;;
(global-set-key (kbd "C-z c") 'sr-speedbar-toggle)

;;;
;;; goto line command
;;;
(global-set-key (kbd "C-z g") 'goto-line)

;;;
;;; shell invokation
;;;
(global-set-key (kbd "C-z C-s") 'shell)
(global-set-key (kbd "C-z s") 'eshell)

;;;
;;; Add another quick auto expand shortcut -- works in windows system
;;;
(global-unset-key (kbd "s-/"))
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "S-SPC") 'hippie-expand)

;;;
;;; newline and indent defined as ctrl+return
;;;
(global-set-key (kbd "<C-RET>") 'newline-and-indent)
(add-hook 'prog-mode-hook
          (function (lambda ()
                      (define-key
                        prog-mode-map
                        (kbd "RET") 'newline-and-indent))))

(provide 'my-keymap)
;;; my-keymap.el ends here
