;;; setup-backup-files.el --- Backups go in different places.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shiguang Wang

;; Author: Shiguang Wang <sgwang@dev11120.prn1.facebook.com>
;; Keywords: files

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

(setq version-control t     ;; User version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t   ;; Copy all files, don't rename them.
      vc-make-backup-files t  ;; Also backup versioned files.
      backup-copying-when-linked t
      )

;; Default and per-save backups go here.
(setq backup-directory-alist `(("." . "~/.emacs_backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs_backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" buckup on each save. The first save results in
  ;; both a per-session and a per-save buckup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (buckup-buffer)))

(add-hook 'before-save-hook 'force-backup-of-buffer)

(provide 'setup-backup-files)
;;; setup-backup-files.el ends here
