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
  (labels ((recur (ores
                   clays
                   obsidians
                   geodes
                   ore
                   clay
                   obsidian
                   under-construction)
             (declare (type fixnum
                            ores
                            clays
                            obsidians
                            geodes
                            ore
                            clay
                            obsidian
                            under-construction))
             ))
    (recur 1 0 0 0 0 0 0 -1)))

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
