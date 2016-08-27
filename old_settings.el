
;;; ==========
;;; FB related
;;; ==========

;; -*- Emacs-Lisp -*-
;; import the master.emacs file
(defconst master-dir (getenv "LOCAL_ADMIN_SCRIPTS"))
(defconst engshare-master (getenv "ADMIN_SCRIPTS"))
(if (file-exists-p (expand-file-name "master.emacs" master-dir))
    (load-library (expand-file-name "master.emacs" master-dir))
  (load-library (expand-file-name "master.emacs" engshare-master)))

;; Set a few things up for Hack code.
(setq hack-for-hiphop-root (expand-file-name "www" "~"))
(load "/home/engshare/tools/hack-for-hiphop")

;; Style customizations for PHP and JS code.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(indent-tabs-mode nil)
 '(js-indent-level 2))

;; ;; ctags
;; (visit-tags-table "~/fbcode/TAGS")
;; (let (large-file-warning-threshold 'nil) (visit-tags-table "~/fbcode/TAGS"))

;;; ==========
;;; My packages
;;; ==========

;; Internet Proxy
(setq url-proxy-services
      '(("no_proxy" . "fbcdn\\.net")
        ("http" . "fwdproxy.any.facebook.com:8080")
        ("https" . "fwdproxy.any.facebook.com:8080")))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Add packages to the Emacs
;; Packages are installed if they are not already present
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      irony            ;; clang integration
                      company          ;; auto completion framework
                      company-irony    ;; irony plugin for company
                      irony-eldoc      ;; doc strings plugin for irony
                      yasnippet        ;; functio nparameters substitution
                      undo-tree
                      buffer-move
                      sr-speedbar
                      paredit
                      rainbow-delimiters
                      auto-complete
                      ido
                      idomenu
                      smex
                      hideshow
                      async
                      helm
                      helm-ls-hg
                      helm-swoop
                      helm-gtags
                      helm-projectile
                      helm-ag
                      helm-codesearch
                      json
                      thingatpt
                      grep
                      cl-lib
                      projectile
                      dash
                      ag
                      dumb-jump
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Loadpath settings
;; Add the path ~/.emacs.d/elpa and all subdirectories to loadpath
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Add the path ~/.emacs.d/elisp to loadpath
(let ((plugin-base "~/.emacs.d/elisp"))
  (add-to-list 'load-path plugin-base))


;;; ==========
;;; My Tweaks
;;; ==========

;; define the command prefix C-z
(define-prefix-command 'ctl-z-map)
(global-set-key (kbd "C-z") 'ctl-z-map)

;; set regexp incremental search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;; Toggle comment
(global-set-key (kbd "C-z b") 'comment-or-uncomment-region)

;; Toggle Hide/Show
(global-set-key (kbd "C-z a") 'hs-toggle-hiding)

;; Enable mouse clicks in terminal
(require 'mouse)
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

;; dealing with the backup files
(setq backup-directory-alist `(("." . "~/emacs_backup")))
(setq backup-copying-when-linked t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; buffer freely moves
(require 'buffer-move)
(global-set-key (kbd "C-z i") 'buf-move-up)
(global-set-key (kbd "C-z k") 'buf-move-down)
(global-set-key (kbd "C-z j") 'buf-move-left)
(global-set-key (kbd "C-z l") 'buf-move-right)

;; show column number
(column-number-mode t)

;; show line numbers in the buffer using linum
(require 'linum)
(setq linum-format "%4d \u2502 ")
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))
(global-set-key (kbd "C-z ;") 'linum-mode)

;; put the speedbar into the current frame
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "C-z c") 'sr-speedbar-toggle)

;; disable the tool bar
(tool-bar-mode -1)

;; goto line command
(global-set-key (kbd "C-z g") 'goto-line)

;; shell invokation
(global-set-key (kbd "C-z C-s") 'shell)
(global-set-key (kbd "C-z s") 'eshell)

;; Add another quick auto expand shortcut
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(global-unset-key (kbd "s-/"))
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "S-SPC") 'hippie-expand)

;; display the time and date
(setq display-time-day-and-date t)
(display-time)

;; text mode auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; elisp mode turn on paredit
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; show paren matching
(show-paren-mode t)

;; highlight the block to copy
(transient-mark-mode t)

;; prohibit the startup window
(setq inhibit-startup-message t)

;; auto adjest the cursor position
(mouse-avoidance-mode 'animate)

;; yes-or-no by y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; copy and paste across emacs and other program
(setq x-select-enable-clipboard t)

;;yasnippet: textmate like template tool
(require 'yasnippet)
(yas/global-mode 1)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; color match parens and other structure characters to make code easy to follow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; autocomplete settings
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 3)  ; autostart after input 3 or more characters
(setq ac-auto-show-menu 0.5)  ; wait 0.5 second to show the menu
(setq ac-use-quick-help nil)  ; do not use quick help
(setq ac-ignore-case nil)  ; distinguish case

;; xml editing, autocomplete ending
(setq nxml-slash-auto-complete-flag t)

;; newline and indent defined as ctrl+return
(global-set-key (kbd "<C-return>") 'newline-and-indent)
(add-hook 'prog-mode-hook
          (function (lambda ()
                      (define-key
                        prog-mode-map
                        (kbd "RET") 'newline-and-indent))))

;; ;; ido settings
;; (require 'ido)
;; (require 'idomenu)
;; (autoload 'idomenu "idomenu" nil t)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-create-new-buffer 'always
;;       ido-user-filename-at-point t
;;       ido-max-prospects 10
;;       ido-everywhere t
;;       ido-ubiquitous t)
;; (ido-mode t)

;; ;; using smex for M-x
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; hideshow
;;;###autoload
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

;;;###autoload
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-z h") 'hs-toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

;; Add fringe to emacs in terminal
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))2 2)))


;;; ==============
;;; C++ Settings
;;; ==============

;; Helm
(require 'grep)
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'helm-projectile)
(helm-projectile-on)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; irony auto completion based on clang
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap completion-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; highlight indentation
(require 'highlight-indentation)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)

;;; =================
;;; Copy without selection
;;; =================
;; Base function
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
         (lambda ()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank))))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))))
;; copy the word under cursor
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )
(global-set-key (kbd "C-c w")         (quote copy-word))
;; copy the line under cursor
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  ;;(paste-to-mark arg)
  )
(global-set-key (kbd "C-c l")         (quote copy-line))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
