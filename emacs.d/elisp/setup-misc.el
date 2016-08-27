;;; setup-misc.el --- Misc customizations.           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shiguang Wang

;; Author: Shiguang Wang <sgwang@dev11120.prn1.facebook.com>
;; Keywords: convenience, extensions

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

;; Customized settings for emacs.

;;; Code:

;;;
;;; Add fringe to emacs in terminal
;;;
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))2 2)))

;;;
;;; Enable mouse clicks in terminal
;;;
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(defun my-terminal-mouse (&optional frame)
  "Establish settings for the current terminal."
  (if (not frame) ;; The initial call.
      (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
        ;; Re-initialise the mode in case of a new terminal.
        (xterm-mouse-mode 1)
        )
    ))
;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).
(my-terminal-mouse)
(add-hook 'after-make-frame-functions 'my-terminal-mouse)

;;;
;;; dealing with the backup files
;;;
(setq backup-directory-alist `(("." . "~/emacs_backup")))
(setq backup-copying-when-linked t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;;;
;;; show column number
;;;
(column-number-mode t)

;;;
;;; show line numbers in the buffer using linum
;;;
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))

;;;
;;; disable the tool bar
;;;
(tool-bar-mode -1)

;;;
;;; hippie expand settings
;;;
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;;;
;;; elisp mode turn on paredit
;;;
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;;
;;; show paren matching
;;;
(show-paren-mode t)

;;;
;;; highlight the block to copy
;;;
(transient-mark-mode t)

;;;
;;; auto adjest the cursor position
;;;
(mouse-avoidance-mode 'animate)

;;;
;;; yes-or-no by y-or-n
;;;
(fset 'yes-or-no-p 'y-or-n-p)

;;;
;;; display time
;;;
(display-time)

;;;
;;; text mode auto-fill
;;;
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;
;;; yasnippet
;;;
(yas/global-mode 1)

;;;
;;; undo-tree
;;;
(global-undo-tree-mode)

;;;
;;; color match parens and other structure characters to make code easy to follow
;;;
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;
;;; autocomplete settings
;;;
(ac-config-default)
(setq ac-auto-start 3)        ; autostart after input 3 or more characters
(setq ac-auto-show-menu 0.5)  ; wait 0.5 second to show the menu
(setq ac-use-quick-help nil)  ; do not use quick help
(setq ac-ignore-case nil)     ; distinguish case

;;;
;;; highlight indentation
;;;
(add-hook 'prog-mode-hook 'highlight-indentation-mode)


(provide 'setup-misc)
;;; setup-misc.el ends here
