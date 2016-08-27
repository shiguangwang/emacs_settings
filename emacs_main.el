;;;
;;; Uncomment if on devserver
;;;
; (require 'setup-at-fb)

;;;
;;; Add the path ~/.emacs.d/elisp to loadpath
;;;
(let ((plugin-base "~/.emacs.d/elisp"))
  (add-to-list 'load-path plugin-base))


;;;
;;; Install Packages and Set Loadpath
;;;
(require 'my-package-repo)

;;;
;;; Requirements
;;;
(require 'auto-complete-config)
(require 'buffer-move)
(require 'grep)
(require 'highlight-indentation)
(require 'linum)
(require 'mouse)
(require 'rainbow-delimiters)
(require 'sr-speedbar)
(require 'undo-tree)
(require 'yasnippet)

;;;
;;; Customized Settings
;;;
(require 'setup-copy-word-line)
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-helm-projectile)
(require 'setup-hideshow)
(require 'setup-irony)
(require 'setup-misc)

;;;
;;; Customized Keymap
;;;
(require 'my-keymap)


(custom-set-variables
 '(c-basic-offset 2)
 '(display-time-day-and-date t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-message t)
 '(js-indent-level 2)
 '(linum-format "%4d \u2502 ")
 '(nxml-slash-auto-complete-flag t)  ; autocomplete ending in xml editing
 '(sr-speedbar-right-side nil)
 '(x-select-enable-clipboard t)
 )
