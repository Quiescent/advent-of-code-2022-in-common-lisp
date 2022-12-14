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
    (format t "compressed: ~a~%" compressed)
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
          (sort (distances-to-others room graph final-vertices) #'>
                :key (lambda (cell) (gethash (nth (cdr cell) final-vertices)
                                             rates))))
    (finally (return new-graph))))

(defun distances-to-others (room graph vertices-of-interest)
  (let ((q (datastructures::make-queue))
        (seen (make-hash-table)))
    (setf (gethash room seen) t)
    (datastructures::enqueue q (cons 0 room))
    (iter outer
      (while (not (datastructures::queue-empty q)))
      (for (distance . current-room) = (datastructures::dequeue q))
      ;; (format t "current-room: ~a~%" current-room)
      ;; (format t "distance: ~a~%" distance)
      (iter
        (for other-room in (gethash current-room graph))
        (when (not (gethash other-room seen))
          (when (member other-room vertices-of-interest)
            (in outer (collecting (cons (1+ distance)
                                        (position other-room
                                                  vertices-of-interest)))))
          (datastructures::enqueue q (cons (1+ distance) other-room))
          (setf (gethash other-room seen) t))))))

(defun search-from (graph rates)
  (iter
    (with seen = (make-hash-table :test #'equal))
    (for i from 0 below 40)
    (with routes = (list (list 0 0 0 (list 0))))
    (format t "(length routes): ~a~%" (length routes))
    (for (best . remaining) = (prune routes))
    ;; (format t "best: ~a~%" best)
    (when best (maximizing best))
    (while remaining)
    (setf routes remaining)
    (setf routes
          (remove-if (lambda (route) (gethash route seen))
                     routes))
    (iter
      (for route in routes)
      (setf (gethash route seen) t))
    ;; (format t "routes: ~a~%" routes)
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
      ((= minute 30) (progn
                       (maximizing total into best)
                       (finding open-valves maximizing total into best-route))))
    (finally (progn
               (format t "best-route: ~a~%" best-route)
               (return (cons best remaining))))))

(defun expand (routes graph rates)
  (iter outer
    (for (minute current-room total open-valves) in routes)
    (for ajdoining-rooms = (aref graph current-room))
    (collecting (tick-one (list minute
                                current-room
                                total
                                open-valves)
                          (- 30 minute)
                          rates))
    (iter
      (for (distance . other-room) in ajdoining-rooms)
      (when (and (<= (+ distance minute) 30)
                 (not (member other-room open-valves)))
        (in outer (collecting (bind (((new-minute
                                       new-room
                                       new-total
                                       new-open-valves)
                                      (tick-one (list minute
                                                      other-room
                                                      total
                                                      open-valves)
                                                distance
                                                rates)))
                                (bind ((result (tick-one (list new-minute
                                                               new-room
                                                               new-total
                                                               new-open-valves)
                                                         1
                                                         rates)))
                                  (list (1+ new-minute)
                                        new-room
                                        (nth 2 result)
                                        (cons other-room new-open-valves))))))))))

(defun tick-one (route time rates)
  (bind (((minute current-room total open-valves) route))
    (list (+ minute time)
          current-room
          (+ total
             (* time (->> (mapcar (lambda (room) (aref rates room)) open-valves)
                       (apply #'+))))
          open-valves)))

(defun part-2 ()
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
    (format t "compressed: ~a~%" compressed)
    (search-from-with-elephants compressed rates)))

(defvar rate-cache
  (make-hash-table))

(declaim (ftype (function (vector fixnum) fixnum) rate))
(defun rate (rates valves)
  (declare (optimize (debug 0) (safety 0) (speed 3))
           (type vector rates)
           (type fixnum valves))
  (let ((result #1=(aref rate-cache valves)))
    (declare (type fixnum result))
    (or (and (/= result most-negative-fixnum) result)
        (setf #1#
              (the fixnum
                   (iter
                     (for i from 0 below (length rates))
                     (when (> (logand (ash 1 i) valves) 0)
                       (summing (the fixnum (aref rates i))))))))))

(defun search-from-with-elephants (graph rates)
  (declare (optimize (debug 0) (safety 0) (speed 3))
           (type vector rates))
  (let* ((best most-negative-fixnum)
         (all-valves (1- (expt 2 (length rates))))
         (rate-cache (make-array (list (1+ all-valves))
                                 :initial-element most-negative-fixnum)))
    (declare (type fixnum all-valves best))
    (format t "all-valves: ~a~%" all-valves)
    (labels ((recur (time total room room-el travel travel-el valves used)
               (declare (type fixnum time total room room-el valves used))
               ;; (format t "~a~%" (list time total room room-el travel travel-el valves used))
               (cond
                 ((= time 30) (progn
                                (when (> total best)
                                  (print total))
                                (setf best (max best total))
                                total))
                 ((= valves all-valves)
                  (let ((next-result (the fixnum
                                          (+ total
                                             (the fixnum
                                                  (* (rate rates all-valves)
                                                     (the fixnum (- 30 time))))))))
                    (when (> next-result best)
                      (print next-result))
                    (setf best (max best next-result))
                    next-result))
                 ((< (the fixnum (+ total (the fixnum
                                               (* (rate rates all-valves)
                                                  (the fixnum (- 30 time)))))) best)
                  most-negative-fixnum)
                 (t
                  (let ((min-time (min (the fixnum
                                            (or travel most-positive-fixnum))
                                       (the fixnum
                                            (or travel-el most-positive-fixnum)))))
                    (cond
                      ((and (not (eq min-time most-positive-fixnum))
                            (>= min-time (- 30 time)))
                       (progn
                         ;; (format t "Not enough time~%")
                         (recur  30
                                 (the fixnum
                                      (+ total (the fixnum
                                                    (* (rate rates valves)
                                                       (the fixnum (- 30 time))))))
                                 room
                                 room-el
                                 nil
                                 nil
                                 valves
                                 used)))
                      ((= min-time most-positive-fixnum)
                       (progn
                         ;; (format t "No one travelling~%")
                         (iter
                           (for (distance . other-room)
                                in (aref graph room))
                           (when (> (logand (ash 1 other-room) used) 0)
                             (next-iteration))
                           (maximizing
                            (or (iter
                                  (for (distance-el . other-room-el)
                                       in (aref graph room-el))
                                  (when (or (eq other-room other-room-el)
                                            (> (logand (ash 1 other-room-el) used) 0))
                                    (next-iteration))
                                  (maximizing (or (recur time
                                                         total
                                                         other-room
                                                         other-room-el
                                                         (1+ distance)
                                                         (1+ distance-el)
                                                         valves
                                                         (logior (ash 1 other-room)
                                                                 (logior (ash 1 other-room-el)
                                                                         used)))
                                                  most-negative-fixnum)))
                                most-negative-fixnum)))))
                      ((and (eq min-time travel)
                            (eq min-time travel-el))
                       (let ((delta (the fixnum (* min-time (rate rates valves)))))
                         ;; (format t "Both arrive~%")
                         (if (= used all-valves)
                             (recur (+ time min-time)
                                    (+ total delta)
                                    room
                                    room-el
                                    nil
                                    nil
                                    (the fixnum (logior (the fixnum (ash 1 room))
                                                        (logior (the fixnum
                                                                     (ash 1 room-el))
                                                                valves)))
                                    used)
                             (iter
                               (for (distance . other-room)
                                    in (aref graph room))
                               (when (> (logand (ash 1 other-room) used) 0)
                                 (next-iteration))
                               (maximizing
                                (or (iter
                                      (for (distance-el . other-room-el)
                                           in (aref graph room-el))
                                      (when (or (eq other-room other-room-el)
                                                (> (logand (ash 1 other-room-el)
                                                           used)
                                                   0))
                                        (next-iteration))
                                      (maximizing (or (recur (+ time min-time)
                                                             (+ total delta)
                                                             other-room
                                                             other-room-el
                                                             (1+ distance)
                                                             (1+ distance-el)
                                                             (logior (ash 1 room)
                                                                     (logior (ash 1 room-el)
                                                                             valves))
                                                             (logior (ash 1 other-room)
                                                                     (logior (ash 1  other-room-el)
                                                                             used)))
                                                      most-negative-fixnum)))
                                    most-negative-fixnum))))))
                      ((eq min-time travel)
                       (let ((delta (the fixnum (* min-time (rate rates valves)))))
                         ;; (format t "You arrive~%")
                         (if (= used all-valves)
                             (recur (+ time min-time)
                                    (+ total delta)
                                    room
                                    room-el
                                    nil
                                    (if (null travel-el)
                                        travel-el
                                        (the fixnum (- travel-el travel)))
                                    (logior (the fixnum (ash 1 room))
                                            valves)
                                    used)
                             (iter
                               (for (distance . other-room)
                                    in (aref graph room))
                               (when (> (logand (ash 1 other-room) used) 0)
                                 (next-iteration))
                               (maximizing (or (recur (+ time min-time)
                                                      (+ total delta)
                                                      other-room
                                                      room-el
                                                      (1+ distance)
                                                      (- travel-el travel)
                                                      (logior (ash 1 room)
                                                              valves)
                                                      (logior (ash 1 other-room)
                                                              used))
                                               most-negative-fixnum))))))
                      ((eq min-time travel-el)
                       (let ((delta (the fixnum (* min-time (rate rates valves)))))
                         ;; (format t "Elephant arrives~%")
                         (if (= used all-valves)
                             (recur (+ time min-time)
                                    (+ total delta)
                                    room
                                    room-el
                                    (if (null travel)
                                        travel
                                        (the fixnum (- travel travel-el)))
                                    nil
                                    (logior (the fixnum (ash 1 room-el))
                                            valves)
                                    used)
                             (iter
                               (for (distance-el . other-room-el)
                                    in (aref graph room-el))
                               (when (> (logand (ash 1 other-room-el) used) 0)
                                 (next-iteration))
                               (maximizing (or (recur (+ time min-time)
                                                      (+ total delta)
                                                      room
                                                      other-room-el
                                                      (- travel travel-el)
                                                      (1+ distance-el)
                                                      (logior (ash 1 room-el)
                                                              valves)
                                                      (logior (ash 1 other-room-el)
                                                              used))
                                               most-negative-fixnum))))))
                      (t (progn
                           (format t "anomaly!: ~a~%" (list time total room room-el travel travel-el valves used))
                           (error "anomaly")))))))))
      (recur 4 0 0 0 nil nil 1 1))))

;; Wrong 2979
;; Wrong 2424
;; Wrong 2571

;; Optimisation notes.
;; Un-optimised part-2: 62.127s
;;
;; Use optimize 3, debug 0, safety 0:
;;  - A few changes: 59.593s
;; Don't expect too much better.
;;
;; Using the profiler...
;; 
;; The function RATES was the most expensive part of the solution.  I
;; turned valves into a bitmask and cached the rates and...
;;  - 21.019s(!)
;;
;; Next round of profiling...
;;
;; I turned used into a bitmask as well.  The results weren't as good
;; as I'd hoped.
;;  - 19.739s
;;
;; Next round of profiling...
;;
;; I turned the cache of rates into a massive array.  The results were
;; modest at best.
;;  - 18.998
;;
;; Turned on the optimizer and shaved off 1 more second.
;;  - 18.191
;;
;; Good enough for now.  It was fun! :-)
;;
;; I came back for more! XD
;;
;; I followed the compiler to get more unboxed arithmetic.
;;  - 16.537
;;
;; Can't say the code looks pretty right now.  Perhaps I should use
;; macros to cure that problem...

#+nil
(require :sb-sprof)

#+nil
(sb-sprof:with-profiling (:max-samples 10000
                          :report :flat
                          :loop nil)
  (part-2))
