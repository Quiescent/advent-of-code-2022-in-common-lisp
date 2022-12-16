(defpackage 2022-day-16
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-16)

(defun part-1 ()
  (bind (((graph . rate)
          (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-16.in"))
            (do ((line (read-line f nil nil) (read-line f nil nil))
                 (graph (make-hash-table))
                 (rates (make-hash-table)))
                ((null line) (cons graph rates))
              (match line
                ((ppcre "Valve ([A-Z][A-Z]) has flow rate=(\\d+); tunnels? leads? to valves? (.*)"
                        (read room)
                        (read rate)
                        others)
                 (dolist (other (->> (split ", " others)
                                  (mapcar #'read-from-string)))
                   (push other (gethash room graph))
                   (setf (gethash room rates) rate)))))))
         (compressed (compress-graph graph rate))
         (rates (compress-rates rate)))
    (format t "rates: ~a~%" rates)
    (search-from compressed rates)))

(defun compress-rates (rates)
  (iter
    (with final-vertices = (rooms-with-valves rates))
    (with compressed = (make-array (list (length final-vertices))))
    (for (room rate) in-hashtable rates)
    (when (> rate 0)
      (setf (aref compressed (position room final-vertices)) rate))
    (finally (return compressed))))

(defun rooms-with-valves (rates)
  (iter
    (for (room rate) in-hashtable rates)
    (when (> rate 0)
      (collecting room into result))
    (finally (return (cons 'AA result)))))

(defun compress-graph (graph rates)
  (iter
    (with final-vertices = (rooms-with-valves rates))
    (with new-graph = (make-array (list (length final-vertices))))
    (for room in final-vertices)
    (for idx from 0)
    (setf (aref new-graph idx)
          (distances-to-others room graph final-vertices))
    (finally (return new-graph))))

(defun distances-to-others (room graph vertices-of-interest)
  (let ((q (datastructures:make-queue))
        (seen (make-hash-table)))
    (datastructures:enqueue q room)
    (iter outer
      (for distance from 1)
      (while (not (datastructures:queue-empty q)))
      (for current-room = (datastructures:dequeue q))
      (iter
        (for other-room in (gethash current-room graph))
        (when (not (gethash other-room seen))
          (when (member other-room vertices-of-interest)
            (in outer (collecting (cons distance
                                        (position other-room
                                                  vertices-of-interest)))))
          (datastructures:enqueue q other-room)
          (setf (gethash other-room seen) t))))))

(defun search-from (graph rates)
  (iter
    (for i from 0 below 40)
    (with routes = (list (list 0 0 0 (list 0))))
    (format t "(length routes): ~a~%" (length routes))
    (for (best . remaining) = (prune routes))
    ;; (format t "best: ~a~%" best)
    (when best (maximizing best))
    (while remaining)
    (setf routes remaining)
    ;; (format t "remaining: ~a~%" remaining)
    (setf routes (expand routes graph rates))
    ;; (format t "expanded: ~a~%" routes)
    ))

(defun prune (routes)
  (iter
    (for route in routes)
    (for (minute current-room total open-valves) = route)
    (cond
      ((< minute 30) (collecting route into remaining))
      ((= minute 30) (maximizing total into best)))
    (finally (return (cons best remaining)))))

(defun expand (routes graph rates)
  (iter outer
    (for (minute current-room total open-valves) in routes)
    (for ajdoining-rooms = (aref graph current-room))
    (when (and (> (aref rates current-room) 0)
               (not (member current-room open-valves)))
      (collecting (tick-one (list minute
                                  current-room
                                  total
                                  (cons current-room open-valves))
                            1
                            rates)))
    (collecting (tick-one (list minute
                                current-room
                                total
                                open-valves)
                          (- 30 minute)
                          rates))
    (iter
      (for (distance . other-room) in ajdoining-rooms)
      (when (<= (+ distance minute) 30)
        (in outer (collecting (tick-one (list minute
                                              other-room
                                              total
                                              open-valves)
                                        distance
                                        rates)))))))

(defun tick-one (route time rates)
  (bind (((minute current-room total open-valves) route))
    (list (+ minute time)
          current-room
          (+ total
             (* time (->> (mapcar (lambda (room) (aref rates room)) open-valves)
                       (apply #'+))))
          open-valves)))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-16.in"))
                 )))
    ))
