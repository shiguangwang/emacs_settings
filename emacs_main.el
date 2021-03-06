;;;
;;; Add the path ~/.emacs.d/elisp to loadpath
;;;
(let ((plugin-base "~/.emacs.d/elisp"))
  (add-to-list 'load-path plugin-base))

;;;
;;; custom set variables
;;;
(custom-set-variables
 '(c-basic-offset 2)
 '(display-time-day-and-date t)
 '(echo-keystrokes 0.5)  ; shorten the keystroke echo wait time
 '(gc-cons-threshold 50000000)  ; allow more than 800 KB cache
 '(indent-tabs-mode nil)
 '(inhibit-startup-message t)
 '(js-indent-level 2)
 '(line-number-display-limit-width 10000)
 '(linum-format "%4d \u2502 ")
 '(nxml-slash-auto-complete-flag t)  ; autocomplete ending in xml editing
 '(recenter-positions '(top middle bottom))
 '(server-use-tcp t)  ; allow multiple Emacs daemons
 '(sr-speedbar-right-side nil)
 '(x-select-enable-clipboard t)
 )

;;;
;;; Install Packages and Set Loadpath
;;;
(require 'my-package-repo)

;;;
;;; Requirements
;;;
(require 'auto-complete-config)
(require 'buffer-move)
(require 'evil)
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
;; FB
(if (string-match  (rx-to-string `(: ,"facebook.com" eos) t)
		   system-name)
    (require 'setup-at-fb))
(require 'setup-backup-files)
(require 'setup-copy-word-line)
(require 'setup-evil)
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
