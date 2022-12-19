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
      (for best = (best-score ore-ore-cost
                              clay-ore-cost
                              obsidian-ore-cost
                              obsidian-clay-cost
                              geode-ore-cost
                              geode-obsidian-cost))
      (summing (* factory-id best)))))

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
                   geode
                   under-construction)
             (declare (type fixnum
                            minute
                            ores
                            clays
                            obsidians
                            geodes
                            ore
                            clay
                            obsidian
                            geode
                            under-construction))
             (let ((next-ore (+ ore ores))
                   (next-clay (+ clay clays))
                   (next-obsidian (+ obsidian obsidians))
                   (next-geode (+ geode geodes))
                   (next-ores (if (= under-construction 0)
                                  (1+ ores)
                                  ores))
                   (next-clays (if (= under-construction 1)
                                   (1+ clays)
                                   clays))
                   (next-obsidians (if (= under-construction 2)
                                       (1+ obsidians)
                                       obsidians))
                   (next-geodes (if (= under-construction 3)
                                    (1+ geodes)
                                    geodes)))
               (format t "(list minute
                            ores
                            clays
                            obsidians
                            geodes
                            ore
                            clay
                            obsidian
                            under-construction): ~a~%" (list minute
                                                             ores
                                                             clays
                                                             obsidians
                                                             geodes
                                                             ore
                                                             clay
                                                             obsidian
                                                             next-geode
                                                             under-construction))
               (if (= minute 24)
                   (print geodes)
                   (max
                    (if (and (>= ore geode-ore-cost)
                             (>= obsidian geode-obsidian-cost))
                        (recur (1+ minute)
                               next-ores
                               next-clays
                               next-obsidians
                               next-geodes
                               (- next-ore geode-ore-cost)
                               next-clay
                               (- next-obsidian geode-obsidian-cost)
                               next-geode
                               3)
                        most-negative-fixnum)
                    (if (and (>= ore obsidian-ore-cost)
                             (>= ore obsidian-clay-cost))
                        (recur (1+ minute)
                               next-ores
                               next-clays
                               next-obsidians
                               next-geodes
                               (- next-ore obsidian-ore-cost)
                               (- next-clay obsidian-clay-cost)
                               next-obsidian
                               next-geode
                               2)
                        most-negative-fixnum)
                    (if (>= ore clay-ore-cost)
                        (recur (1+ minute)
                               next-ores
                               next-clays
                               next-obsidians
                               next-geodes
                               (- next-ore clay-ore-cost)
                               next-clay
                               next-obsidian
                               next-geode
                               1)
                        most-negative-fixnum)
                    (if (>= ore ore-ore-cost)
                        (recur (1+ minute)
                               next-ores
                               next-clays
                               next-obsidians
                               next-geodes
                               (- next-ore ore-ore-cost)
                               next-clay
                               next-obsidian
                               next-geode
                               0)
                        most-negative-fixnum)
                    (recur (1+ minute)
                           next-ores
                           next-clays
                           next-obsidians
                           next-geodes
                           next-ore
                           next-clay
                           next-obsidian
                           next-geode
                           -1))))))
    (recur 1 1 0 0 0 0 0 0 0 -1)))

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
                 )))
    ))
