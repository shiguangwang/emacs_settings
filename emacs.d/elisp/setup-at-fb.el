;;; setup-at-fb.el --- Facebook Related Settings     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shiguang Wang

;; Author: Shiguang Wang <sgwang@dev11120.prn1.facebook.com>
;; Keywords: convenience, c, languages

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

;; ;; ctags
;; (visit-tags-table "~/fbcode/TAGS")
;; (let (large-file-warning-threshold 'nil) (visit-tags-table "~/fbcode/TAGS"))

;; FB Internet Proxy
(setq url-proxy-services
      '(("no_proxy" . "fbcdn\\.net")
        ("http" . "fwdproxy.any.facebook.com:8080")
        ("https" . "fwdproxy.any.facebook.com:8080")))


(provide 'setup-at-fb)
;;; setup-at-fb.el ends here
