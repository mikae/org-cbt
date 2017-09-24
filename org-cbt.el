;; org-cpt.el --- cbt
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL: todo
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             .
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'org)
(require 'f)
(require 'cl-lib)

(defconst org-cbt-thought-diary-dir-format "%Y-%m-%d"
  "Format of thought diary entry dirs.")

(defconst org-cbt-thought-diary-entry-format "case-%d.org"
  "Format of thought diary entry files.")

(defvar org-cbt-home nil
  "Directory where org-cbt files are stored in.")

(defvar org-cbt-thought-diary-dir nil
  "Directory for thought diary.")

(define-skeleton --org-cbt-thought-diary-entry-skeleton
  "Create thought diary entry skeleton."
  ""
  "* $DATE \n"
  "\n"
  "* Time\n"
  "\n"
  "* Situation\n"
  "\n"
  "* Thought(s)\n"
  "\n"
  "* Emotion\n"
  "\n"
  "* Adaptive Response\n"
  "\n"
  "* Outcome\n"
  "\n"
  )

(defun --org-cbt-try-setup ()
  (cl-flet ((--create-dir (loc)
                          (if (and loc
                                   (f-absolute-p loc))
                              (unless (f-dir-p loc)
                                (f-mkdir loc))
                            (error "org-cbt-try-setup: not a valid directory: %s" loc))))
    (--create-dir org-cbt-home)
    (--create-dir org-cbt-thought-diary-dir)))

(defun org-cbt-set-home (dir)
  "Set home dir of `org-cbt'."
  (setq org-cbt-home dir)
  (setq org-cbt-thought-diary-dir (f-join dir
                                          "thought-diary")))

(defun org-cbt-thought-diary-edit-entry ()
  "Edit entry of thought journal"
  (interactive)
  (--org-cbt-try-setup)
  (let* ((--dir (f-join org-cbt-thought-diary-dir
                        (format-time-string org-cbt-thought-diary-dir-format)))
         (--counter 1)
         (--file))
    ;; find empty number
    (while
        (progn
          (setq --file
                (f-join --dir
                        (format org-cbt-thought-diary-entry-format
                                --counter)))
          (setq --counter (1+ --counter))
          (f-file-p --file)))
    (unless (f-dir-p --dir)
      (f-mkdir --dir))
    (find-file --file)
    (--org-cbt-thought-diary-entry-skeleton)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\$DATE")
      (replace-match (format-time-string "%d-%m-%Y")))))

(provide 'org-cbt)
;;; org-cbt.el ends here
