(defsystem "advent-of-code-2022-in-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-ppcre :trivia :trivia.ppcre :arrow-macros :metabang-bind :datastructures :fset :croatoan)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "2022-day-21")
                 (:file "2022-day-20")
                 (:file "2022-day-19")
                 (:file "2022-day-18")
                 (:file "2022-day-17")
                 (:file "2022-day-16")
                 (:file "2022-day-15")
                 (:file "2022-day-14")
                 (:file "2022-day-13")
                 (:file "2022-day-12")
                 (:file "2022-day-11")
                 (:file "2022-day-10")
                 (:file "2022-day-9")
                 (:file "2022-day-8")
                 (:file "2022-day-7")
                 (:file "2022-day-6")
                 (:file "2022-day-5")
                 (:file "2022-day-4")
                 (:file "2022-day-3")
                 (:file "2022-day-2")
                 (:file "2022-day-1"))))
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
