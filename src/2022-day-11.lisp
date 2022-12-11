(defpackage 2022-day-11
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-11)

(defun part-1 ()
  (let* ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-11.in"))
                  (do ((line (read-line f nil nil) (read-line f nil nil))
                       (i 0 (1+ i))
                       (current-monkey (list))
                       (monkeys))
                      ((null line) (apply #'vector (nreverse monkeys)))
                    (push line current-monkey)
                    (when (string-equal line "")
                      (bind (((id starting operation test true-branch false-branch . _)
                              (reverse current-monkey)))
                        (push (list (match starting
                                      ((ppcre "Starting items: (.*)"
                                              items)
                                       (->> (split ", " items)
                                         (mapcar #'read-from-string))))
                                    (match operation
                                      ((ppcre "Operation: new = old ([+*]) ((\\d+)|old)"
                                              (read operation)
                                              (read amount))
                                       (list operation amount)))
                                    (match test
                                      ((ppcre "Test: divisible by (\\d+)"
                                              (read amount))
                                       amount))
                                    (match true-branch
                                      ((ppcre "If true: throw to monkey (\\d+)"
                                              (read id))
                                       id))
                                    (match false-branch
                                      ((ppcre "If false: throw to monkey (\\d+)"
                                              (read id))
                                       id)))
                              monkeys))
                      (setf current-monkey nil)))))
         (scores (make-array (list (length input)) :initial-element 0)))
    (dotimes (i 20)
      (dotimes (j (length input))
        (bind (((items (op amount) test dest-true dest-false)
                (aref input j)))
          (dolist (current-item items)
            (incf (aref scores j))
            (let* ((new-worry (funcall op current-item (if (eq amount 'old)
                                                           current-item
                                                           amount)))
                   (bored-worry (floor new-worry 3))
                   (dest (if (= 0 (mod bored-worry test))
                             dest-true
                             dest-false)))
              (setf (car (aref input dest))
                    (nconc (car (aref input dest))
                           (list bored-worry)))))
          (setf (car (aref input j)) nil))))
    (let ((sorted (sort scores #'>)))
      (* (aref sorted 0)
         (aref sorted 1)))))

(defun part-2 ()
  (let* ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-11.in"))
                  (do ((line (read-line f nil nil) (read-line f nil nil))
                       (i 0 (1+ i))
                       (current-monkey (list))
                       (monkeys))
                      ((null line) (apply #'vector (nreverse monkeys)))
                    (push line current-monkey)
                    (when (string-equal line "")
                      (bind (((id starting operation test true-branch false-branch . _)
                              (reverse current-monkey)))
                        (push (list (match starting
                                      ((ppcre "Starting items: (.*)"
                                              items)
                                       (let ((q (datastructures::make-queue)))
                                         (dolist (x
                                                  (->> (split ", " items)
                                                    (mapcar #'read-from-string))
                                                  q)
                                           (datastructures::enqueue q x)))))
                                    (match operation
                                      ((ppcre "Operation: new = old ([+*]) ((\\d+)|old)"
                                              (read operation)
                                              (read amount))
                                       (list operation amount)))
                                    (match test
                                      ((ppcre "Test: divisible by (\\d+)"
                                              (read amount))
                                       amount))
                                    (match true-branch
                                      ((ppcre "If true: throw to monkey (\\d+)"
                                              (read id))
                                       id))
                                    (match false-branch
                                      ((ppcre "If false: throw to monkey (\\d+)"
                                              (read id))
                                       id)))
                              monkeys))
                      (setf current-monkey nil)))))
         (scores (make-array (list (length input)) :initial-element 0))
         (max-worry (apply #'lcm (map 'list (lambda (monkey) (nth 2 monkey)) input))))
    (dotimes (i 10000)
      (dotimes (j (length input))
        (bind (((items (op amount) test dest-true dest-false)
                (aref input j)))
          (do ()
              ((datastructures::queue-empty items))
            (incf (aref scores j))
            (let* ((current-item (datastructures::dequeue items))
                   (new-worry (funcall op current-item (if (eq amount 'old)
                                                           current-item
                                                           amount)))
                   (bored-worry (mod new-worry max-worry))
                   (dest (if (= 0 (mod bored-worry test))
                             dest-true
                             dest-false)))
              (datastructures::enqueue (car (aref input dest))
                                       bored-worry))))))
    (let ((sorted (sort scores #'>)))
      (* (aref sorted 0)
         (aref sorted 1)))))

;; 14401200024
