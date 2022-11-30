(require 'asdf)

(defun main ()
  "Create a problem file.

Reads the year of the problem and the problem number from argv.  In
SBCL, that's *posix-argv*, apparently."
  (destructuring-bind (program year day) *posix-argv*
    (declare (ignore program))
    (when (or (null year)
              (null day))
      (format *error-output*
              "Too few arguments!~%Expected: (year day).~%Got: ~a~%"
              (cdr *posix-argv*))
      (exit :code 1))
    (let* ((n-year (read-from-string year))
           (n-day (read-from-string day))
           (package-name (format nil "~a-day-~a" n-year n-day))
           (file-name (format nil "~a.lisp" package-name))
           (input-file (format nil "~a.in" package-name)))
      (when (or (not (numberp n-year))
                (not (numberp n-day)))
        (format *error-output*
                "Either year or day is not a number!~%Got ~a~%"
                (cdr *posix-argv*)))
      (with-open-file (file (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp
                                                           (format nil
                                                                   "src/~a"
                                                                   file-name))
                            :direction :output
                            :if-does-not-exist :create)
        (format file
                "(defpackage ~a~%  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)~%  (:shadowing-import-from :arrow-macros :->>))~%"
                package-name)
        (format file
                "(in-package ~a)~%~%"
                package-name))
      (with-open-file (file (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp
                                                           (format nil
                                                                   "src/~a"
                                                                   input-file))
                            :direction :output
                            :if-does-not-exist :create)
        (format file ""))
      (uiop:run-program (format nil
                                "emacs --batch --script add-problem.el ~a"
                                package-name)))))

(eval-when (:execute)
  (main))
