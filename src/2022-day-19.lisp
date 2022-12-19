(defpackage 2022-day-19
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-19)

(defun part-1 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-19.in"))
                 (file-lines f))))
    (iter
      (for (factory-id
            ore-ore-cost
            clay-ore-cost
            obsidian-ore-cost
            obsidian-clay-cost
            geode-ore-cost
            geode-obsidian-cost)
           in input)
      (for i from 0)
      (for best = (best-score ore-ore-cost
                              clay-ore-cost
                              obsidian-ore-cost
                              obsidian-clay-cost
                              geode-ore-cost
                              geode-obsidian-cost))
      (format t "TICK ~a: ~a~%" i best)
      (summing (* factory-id best)))))

;; Wrong: 732

;; Encoding:
;; 0 ore
;; 1 clay
;; 2 obsidian
;; 3 geode

(defvar best-found most-negative-fixnum)

(defun best-score (ore-ore-cost
                   clay-ore-cost
                   obsidian-ore-cost
                   obsidian-clay-cost
                   geode-ore-cost
                   geode-obsidian-cost)
  (labels ((recur (minute
                   ores
                   clays
                   obsidians
                   geodes
                   ore
                   clay
                   obsidian
                   geode)
             (declare (type fixnum
                            minute
                            ores
                            clays
                            obsidians
                            geodes
                            ore
                            clay
                            obsidian
                            geode))
             (if (= minute 24)
                 (+ geodes geode)
                 (max
                  (if (and (> ores 0)
                           (> obsidians 0))
                      (let* ((time-to-build (max 1
                                                 (1+ (ceiling (- geode-ore-cost ore)
                                                              ores))
                                                 (1+ (ceiling (- geode-obsidian-cost
                                                                 obsidian)
                                                              obsidians))))
                             (next-minute (+ minute time-to-build)))
                        (if (<= next-minute 24)
                            (let (;; New bots
                                  (next-ores ores)
                                  (next-clays clays)
                                  (next-obsidians obsidians)
                                  (next-geodes (1+ geodes))
                                  ;; New materials
                                  (next-ore (- (+ ore (* time-to-build ores))
                                               geode-ore-cost))
                                  (next-clay (+ clay (* time-to-build clays)))
                                  (next-obsidian (- (+ obsidian (* time-to-build
                                                                   obsidians))
                                                    geode-obsidian-cost))
                                  (next-geode (+ geode (* time-to-build geodes))))
                              (recur next-minute
                                     next-ores
                                     next-clays
                                     next-obsidians
                                     next-geodes
                                     next-ore
                                     next-clay
                                     next-obsidian
                                     next-geode))
                            most-negative-fixnum))
                      most-negative-fixnum)
                  (if (and (> ores 0)
                           (> clays 0))
                      (let* ((time-to-build (max 1
                                                 (1+ (ceiling (- obsidian-ore-cost ore)
                                                              ores))
                                                 (1+ (ceiling (- obsidian-clay-cost clay)
                                                              clays))))
                             (next-minute (+ minute time-to-build)))
                        (if (<= next-minute 24)
                            (let (;; New bots
                                  (next-ores ores)
                                  (next-clays clays)
                                  (next-obsidians (1+ obsidians))
                                  (next-geodes geodes)
                                  ;; New materials
                                  (next-ore (- (+ ore (* time-to-build ores))
                                               obsidian-ore-cost))
                                  (next-clay (- (+ clay (* time-to-build clays))
                                                obsidian-clay-cost))
                                  (next-obsidian (+ obsidian (* time-to-build
                                                                obsidians)))
                                  (next-geode (+ geode (* time-to-build geodes))))
                              (recur next-minute
                                     next-ores
                                     next-clays
                                     next-obsidians
                                     next-geodes
                                     next-ore
                                     next-clay
                                     next-obsidian
                                     next-geode))
                            most-negative-fixnum))
                      most-negative-fixnum)
                  (let* ((time-to-build (max 1
                                             (1+ (ceiling (- clay-ore-cost ore)
                                                          ores))))
                         (next-minute (+ minute time-to-build)))
                    (if (<= next-minute 24)
                        (let (;; New bots
                              (next-ores ores)
                              (next-clays (1+ clays))
                              (next-obsidians obsidians)
                              (next-geodes geodes)
                              ;; New materials
                              (next-ore (- (+ ore (* time-to-build ores))
                                           clay-ore-cost))
                              (next-clay (+ clay (* time-to-build clays)))
                              (next-obsidian (+ obsidian (* time-to-build
                                                            obsidians)))
                              (next-geode (+ geode (* time-to-build geodes))))
                          (recur next-minute
                                 next-ores
                                 next-clays
                                 next-obsidians
                                 next-geodes
                                 next-ore
                                 next-clay
                                 next-obsidian
                                 next-geode))
                        most-negative-fixnum))
                  (let* ((time-to-build (max 1
                                             (1+ (ceiling (- ore-ore-cost ore)
                                                          ores))))
                         (next-minute (+ minute time-to-build)))
                    (if (<= next-minute 24)
                        (let (;; New bots
                              (next-ores (1+ ores))
                              (next-clays clays)
                              (next-obsidians obsidians)
                              (next-geodes geodes)
                              ;; New materials
                              (next-ore (- (+ ore (* time-to-build ores))
                                           ore-ore-cost))
                              (next-clay (+ clay (* time-to-build clays)))
                              (next-obsidian (+ obsidian (* time-to-build
                                                            obsidians)))
                              (next-geode (+ geode (* time-to-build geodes))))
                          (recur next-minute
                                 next-ores
                                 next-clays
                                 next-obsidians
                                 next-geodes
                                 next-ore
                                 next-clay
                                 next-obsidian
                                 next-geode))
                        most-negative-fixnum))
                  ;; Do nothing...
                  (let* ((time-to-build (- 24 minute))
                         (next-minute 24)
                         (next-ores ores)
                         (next-clays clays)
                         (next-obsidians obsidians)
                         (next-geodes geodes)
                         ;; New materials
                         (next-ore (+ ore (* time-to-build ores)))
                         (next-clay (+ clay (* time-to-build clays)))
                         (next-obsidian (+ obsidian (* time-to-build
                                                       obsidians)))
                         (next-geode (+ geode (* time-to-build geodes))))
                    (recur next-minute
                           next-ores
                           next-clays
                           next-obsidians
                           next-geodes
                           next-ore
                           next-clay
                           next-obsidian
                           next-geode))))))
    (recur 1 1 0 0 0 0 0 0 0)))

(defun file-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push (match line
            ((ppcre "Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian."
                    (read factory-id)
                    (read ore-ore-cost)
                    (read clay-ore-cost)
                    (read obsidian-ore-cost)
                    (read obsidian-clay-cost)
                    (read geode-ore-cost)
                    (read geode-obsidian-cost))
             (list factory-id
                   ore-ore-cost
                   clay-ore-cost
                   obsidian-ore-cost
                   obsidian-clay-cost
                   geode-ore-cost
                   geode-obsidian-cost)))
          lines)))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-19.in"))
                 (file-lines f))))
    (iter
      (for (factory-id
            ore-ore-cost
            clay-ore-cost
            obsidian-ore-cost
            obsidian-clay-cost
            geode-ore-cost
            geode-obsidian-cost)
           in (subseq input 0 (min 3 (length input))))
      (for i from 0)
      (for best = (let ((best-found most-negative-fixnum))
                    (best-score-2 ore-ore-cost
                                  clay-ore-cost
                                  obsidian-ore-cost
                                  obsidian-clay-cost
                                  geode-ore-cost
                                  geode-obsidian-cost)))
      (format t "TICK ~a: ~a~%" i best)
      (collecting best into result)
      (finally (return (apply #'* result))))))

(defun best-score-2 (ore-ore-cost
                     clay-ore-cost
                     obsidian-ore-cost
                     obsidian-clay-cost
                     geode-ore-cost
                     geode-obsidian-cost)
  (let ((max-ore-cost (max ore-ore-cost
                           clay-ore-cost
                           obsidian-ore-cost
                           geode-ore-cost)))
   (labels ((recur (minute
                    ores
                    clays
                    obsidians
                    geodes
                    ore
                    clay
                    obsidian
                    geode)
              (declare (type fixnum
                             minute
                             ores
                             clays
                             obsidians
                             geodes
                             ore
                             clay
                             obsidian
                             geode))
              (if (= minute 32)
                  (progn
                    (when (> (+ geodes geode) best-found)
                      (setf best-found (+ geodes geode)))
                    (max (+ geodes geode) best-found))
                  (max
                   (if (and (> ores 0)
                            (> obsidians 0))
                       (let* ((time-to-build (max 1
                                                  (1+ (ceiling (- geode-ore-cost ore)
                                                               ores))
                                                  (1+ (ceiling (- geode-obsidian-cost
                                                                  obsidian)
                                                               obsidians))))
                              (next-minute (+ minute time-to-build)))
                         (if (<= next-minute 32)
                             (let (;; New bots
                                   (next-ores ores)
                                   (next-clays clays)
                                   (next-obsidians obsidians)
                                   (next-geodes (1+ geodes))
                                   ;; New materials
                                   (next-ore (- (+ ore (* time-to-build ores))
                                                geode-ore-cost))
                                   (next-clay (+ clay (* time-to-build clays)))
                                   (next-obsidian (- (+ obsidian (* time-to-build
                                                                    obsidians))
                                                     geode-obsidian-cost))
                                   (next-geode (+ geode (* time-to-build geodes))))
                               (recur next-minute
                                      next-ores
                                      next-clays
                                      next-obsidians
                                      next-geodes
                                      next-ore
                                      next-clay
                                      next-obsidian
                                      next-geode))
                             most-negative-fixnum))
                       most-negative-fixnum)
                   (if (and (> ores 0)
                            (> clays 0)
                            (< obsidians geode-obsidian-cost))
                       (let* ((time-to-build (max 1
                                                  (1+ (ceiling (- obsidian-ore-cost ore)
                                                               ores))
                                                  (1+ (ceiling (- obsidian-clay-cost clay)
                                                               clays))))
                              (next-minute (+ minute time-to-build)))
                         (if (<= next-minute 32)
                             (let (;; New bots
                                   (next-ores ores)
                                   (next-clays clays)
                                   (next-obsidians (1+ obsidians))
                                   (next-geodes geodes)
                                   ;; New materials
                                   (next-ore (- (+ ore (* time-to-build ores))
                                                obsidian-ore-cost))
                                   (next-clay (- (+ clay (* time-to-build clays))
                                                 obsidian-clay-cost))
                                   (next-obsidian (+ obsidian (* time-to-build
                                                                 obsidians)))
                                   (next-geode (+ geode (* time-to-build geodes))))
                               (recur next-minute
                                      next-ores
                                      next-clays
                                      next-obsidians
                                      next-geodes
                                      next-ore
                                      next-clay
                                      next-obsidian
                                      next-geode))
                             most-negative-fixnum))
                       most-negative-fixnum)
                   (if (< clays obsidian-clay-cost)
                       (let* ((time-to-build (max 1
                                                  (1+ (ceiling (- clay-ore-cost ore)
                                                               ores))))
                              (next-minute (+ minute time-to-build)))
                         (if (<= next-minute 32)
                             (let (;; New bots
                                   (next-ores ores)
                                   (next-clays (1+ clays))
                                   (next-obsidians obsidians)
                                   (next-geodes geodes)
                                   ;; New materials
                                   (next-ore (- (+ ore (* time-to-build ores))
                                                clay-ore-cost))
                                   (next-clay (+ clay (* time-to-build clays)))
                                   (next-obsidian (+ obsidian (* time-to-build
                                                                 obsidians)))
                                   (next-geode (+ geode (* time-to-build geodes))))
                               (recur next-minute
                                      next-ores
                                      next-clays
                                      next-obsidians
                                      next-geodes
                                      next-ore
                                      next-clay
                                      next-obsidian
                                      next-geode))
                             most-negative-fixnum))
                       most-negative-fixnum)
                   (if (< ores max-ore-cost)
                       (let* ((time-to-build (max 1
                                                  (1+ (ceiling (- ore-ore-cost ore)
                                                               ores))))
                              (next-minute (+ minute time-to-build)))
                         (if (<= next-minute 32)
                             (let (;; New bots
                                   (next-ores (1+ ores))
                                   (next-clays clays)
                                   (next-obsidians obsidians)
                                   (next-geodes geodes)
                                   ;; New materials
                                   (next-ore (- (+ ore (* time-to-build ores))
                                                ore-ore-cost))
                                   (next-clay (+ clay (* time-to-build clays)))
                                   (next-obsidian (+ obsidian (* time-to-build
                                                                 obsidians)))
                                   (next-geode (+ geode (* time-to-build geodes))))
                               (recur next-minute
                                      next-ores
                                      next-clays
                                      next-obsidians
                                      next-geodes
                                      next-ore
                                      next-clay
                                      next-obsidian
                                      next-geode))
                             most-negative-fixnum))
                       most-negative-fixnum)
                   ;; Do nothing...
                   (let* ((time-to-build (- 32 minute))
                          (next-minute 32)
                          (next-ores ores)
                          (next-clays clays)
                          (next-obsidians obsidians)
                          (next-geodes geodes)
                          ;; New materials
                          (next-ore (+ ore (* time-to-build ores)))
                          (next-clay (+ clay (* time-to-build clays)))
                          (next-obsidian (+ obsidian (* time-to-build
                                                        obsidians)))
                          (next-geode (+ geode (* time-to-build geodes))))
                     (recur next-minute
                            next-ores
                            next-clays
                            next-obsidians
                            next-geodes
                            next-ore
                            next-clay
                            next-obsidian
                            next-geode))))))
     (recur 1 1 0 0 0 0 0 0 0))))
