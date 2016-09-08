;;; setup-evil.el --- Customize Evil                 -*- lexical-binding: t; -*-

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

;; Copied from https://github.com/wasamasa/dotemacs/

;;; Code:

(defun my-real-function (fun)
  "Figure out the actual symbol behind a function.
Returns a different symbol if FUN is an alias, otherwise FUN."
  (let ((symbol-function (symbol-function fun)))
    (if (symbolp symbol-function)
        symbol-function
      fun)))

(defun my-derived-mode-p (mode modes)
  (let ((parent (my-real-function mode)))
    (while (and parent (not (memq parent modes)))
      (setq parent (my-real-function (get parent 'derived-mode-parent))))
    parent))

(with-eval-after-load 'evil-core
  (defun evil-initial-state (mode &optional default)
    "Return the Evil state to use for MODE.
Returns DEFAULT if no initial state is associated with MODE.
The initial state for a mode can be set with
`evil-set-initial-state'."
    (let (state modes)
      (catch 'done
        (dolist (entry (nreverse (evil-state-property t :modes)) default)
          (setq state (car entry)
                modes (symbol-value (cdr entry)))
          (when (or (memq mode modes)
                    (my-derived-mode-p mode modes))
            (throw 'done state)))))))

(setq evil-default-state 'emacs
      evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil
      evil-normal-state-modes '(text-mode prog-mode fundamental-mode
                                          css-mode conf-mode
                                          TeX-mode LaTeX-mode
                                          diff-mode))

;; More Emacs-like feel
(setq evil-cross-lines t
      evil-move-cursor-back nil
      evil-want-fine-undo t
      evil-symbol-word-search t)


;;;
;;; ESC exit Evil insert state and also terminates the company ac.
;;;
(defun my-company-abort ()
  (interactive)
  (company-abort)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'insert))
    (evil-force-normal-state)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<escape>") 'my-company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort))

;;;
;;; extra keybindings
;;;
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)
;; make insert state more comfortable as emacs state.
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

(define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-n") 'next-line)
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-k") 'kill-line)


(evil-mode 1)

(provide 'setup-evil)
;;; setup-evil.el ends here
