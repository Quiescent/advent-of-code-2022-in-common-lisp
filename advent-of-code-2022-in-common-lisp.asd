(defsystem "advent-of-code-2022-in-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-ppcre :trivia :trivia.ppcre :arrow-macros :metabang-bind)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "advent-of-code-2022-in-common-lisp/tests"))))

(defsystem "advent-of-code-2022-in-common-lisp/tests"
  :author ""
  :license ""
  :depends-on ("advent-of-code-2022-in-common-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent-of-code-2022-in-common-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
