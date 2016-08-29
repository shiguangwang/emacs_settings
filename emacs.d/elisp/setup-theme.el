;;;
;;; Add the theme directory
;;;
(setq custom-theme-directory "~/.emacs.d/themes")

;;;
;;; load-theme
;;;
(defun my-load-theme (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (load-theme 'my-solarized t)))
(my-load-theme)
(add-hook 'after-make-frame-functions 'my-load-theme)

;;;
;;; disable questions about theme loading
;;;
(setq custom-safe-themes t)

;;;
;;; themed tooltips
;;;
(setq x-gtk-use-system-tooltips nil)

(provide 'setup-theme)
