;;; add-problem --- adds a problem file to the ASDF file in CWD -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun main ()
  "Open the ASDF file in this directory and add the supplied problem file."
  (find-file "advent-of-code-2022-in-common-lisp.asd")
  (search-forward "(:file \"main\")")
  (let ((begin (point))
        (indent-tabs-mode nil))
    (insert (format "\n(:file \"%s\")" (car command-line-args-left)))
    (indent-region begin (point)))
  (save-buffer))

(main)

(provide 'add-problem)
;;; add-problem.el ends here
