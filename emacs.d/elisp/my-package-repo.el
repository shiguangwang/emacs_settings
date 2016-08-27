;;; my-package-repo.el --- The plugins repo          -*- lexical-binding: t; -*-

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

;; If the package listed not installed, it will be automatically
;; installed at initialization time.

;;; Code:

(require 'package)

;; FB Internet Proxy
(setq url-proxy-services
      '(("no_proxy" . "fbcdn\\.net")
        ("http" . "fwdproxy.any.facebook.com:8080")
        ("https" . "fwdproxy.any.facebook.com:8080")))

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
                      ag
                      async
                      auto-complete
                      buffer-move
                      centered-cursor-mode
                      cl-lib
                      company          ;; auto completion framework
                      company-irony    ;; irony plugin for company
                      dash
                      dumb-jump
                      grep
                      helm
                      helm-ag
                      helm-codesearch
                      helm-gtags
                      helm-ls-hg
                      helm-projectile
                      helm-swoop
                      hideshow
                      hl-spotlight
                      ido
                      idomenu
                      irony            ;; clang integration
                      irony-eldoc      ;; doc strings plugin for irony
                      json
                      paredit
                      projectile
                      rainbow-delimiters
                      smex
                      sr-speedbar
                      thingatpt
                      undo-tree
                      yasnippet        ;; functio nparameters substitution
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;; Loadpath settings
;; Add the path ~/.emacs.d/elpa and all subdirectories to loadpath
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Add the path ~/.emacs.d/elisp to loadpath
(let ((plugin-base "~/.emacs.d/elisp"))
  (add-to-list 'load-path plugin-base))


(provide 'my-package-repo)
;;; my-package-repo.el ends here
