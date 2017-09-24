;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-
(require 'org-cbt)
(require 'f)
(require 'buttercup)

(defconst --org-location     "/tmp/org")
(defconst --org-cbt-home (f-join --org-location
                                 "cbt"))
(defconst --org-cbt-thought-diary-dir (f-join --org-cbt-home
                                              "thought-diary"))

(defun --setup ()
  (when (f-dir-p --org-cbt-home)
    (f-delete --org-cbt-home :force))

  (setq org-location --org-location)
  (setq org-cbt-home nil)
  (setq org-cbt-thought-diary-dir nil)

  (unless (f-dir-p org-location)
    (f-mkdir org-location)))

(defun --clear ()
  (when (f-dir-p --org-cbt-home)
    (f-delete --org-cbt-home :force)))

(describe "It"
  (it "works"))

(describe "org-cbt-set-home"
  (it "Changes home location of org-cbt"
    (org-cbt-set-home --org-cbt-home)
    (expect org-cbt-home
            :to-equal --org-cbt-home)
    (expect org-cbt-thought-diary-dir
            :to-equal --org-cbt-thought-diary-dir)))

(describe "org-cbt-thought-diary-edit-entry"
  (before-all
   (defun --create-new ()
     (org-cbt-thought-diary-edit-entry)
     (save-buffer)
     (kill-buffer)))
  (after-all
   (fmakunbound '--create-new))

  (before-each
    (--setup))
  (after-each
    (--clear))

  (it "Raises an error if `org-cbt-home' is not set."
    (expect (org-cbt-thought-diary-edit-entry)
            :to-throw))

  (it "Creates dirs if it doesn't exist"
    (org-cbt-set-home --org-cbt-home)
    (--create-new)
    (expect (f-dir-p --org-cbt-home)
            :to-be t)
    (expect (f-dir-p --org-cbt-thought-diary-dir)
            :to-be t))

  (it "Creates a thought diary directory and file"
    (org-cbt-set-home --org-cbt-home)
    (--create-new)
    (let* ((--dir (f-join --org-cbt-thought-diary-dir
                          (format-time-string org-cbt-thought-diary-dir-format)))
           (--file (f-join --dir
                           (format org-cbt-thought-diary-entry-format 1))))
      (expect (f-dir-p --dir)
              :to-be t)
      (expect (f-file-p --file)
              :to-be t)))

  (it "Is able to create several files"
    (org-cbt-set-home --org-cbt-home)
    (--create-new)
    (--create-new)
    (--create-new)
    (let* ((--dir (f-join --org-cbt-thought-diary-dir
                          (format-time-string org-cbt-thought-diary-dir-format)))
           (--file-1 (f-join --dir
                             (format org-cbt-thought-diary-entry-format 1)))
           (--file-2 (f-join --dir
                             (format org-cbt-thought-diary-entry-format 2))))
      (expect (f-file-p --file-1)
              :to-be t)
      (expect (f-file-p --file-2)
              :to-be t)))
  )

;; Local Variables:
;; eval: (put 'describe    'lisp-indent-function 'defun)
;; eval: (put 'it          'lisp-indent-function 'defun)
;; eval: (put 'before-each 'lisp-indent-function 'defun)
;; eval: (put 'after-each  'lisp-indent-function 'defun)
;; eval: (put 'before-all  'lisp-indent-function 'defun)
;; eval: (put 'after-all   'lisp-indent-function 'defun)
;; End:
