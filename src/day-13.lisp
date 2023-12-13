(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-13
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-13)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-13.in"))
    (labels ((recur (acc sub-pattern)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (nreverse (cons (coerce (nreverse sub-pattern) 'vector) acc))
                     (if (string-equal line "")
                         (recur (cons (coerce (nreverse sub-pattern) 'vector) acc) nil)
                         (recur acc (cons line sub-pattern)))))))
      (recur nil nil))))

(defun find-vertical-reflection (grid exclude-col)
  (iter
    (for x from 1 below (length (aref grid 0)))
    (when (eq x exclude-col)
      (next-iteration))
    (when (iter
            (for y from 0 below (length grid))
            (for row = (aref grid y))
            (always (iter
                      (for x-low from (1- x) downto 0)
                      (for x-high from x below (length (aref grid 0)))
                      (always (char-equal (aref row x-low)
                                          (aref row x-high))))))
      (return x))))

(defun find-horizontal-reflection (grid exclude-row)
  (iter
    (for y from 1 below (length grid))
    (when (eq y exclude-row)
      (next-iteration))
    (when (iter
            (for x from 0 below (length (aref grid 0)))
            (always (iter
                      (for y-low from (1- y) downto 0)
                      (for y-high from y below (length grid))
                      (always (char-equal (aref (aref grid y-low) x)
                                          (aref (aref grid y-high) x))))))
      (return y))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (iter
      (for grid in problem)
      (for ver = (find-vertical-reflection grid nil))
      (when ver
        (summing ver)
        (next-iteration))
      (for hor = (find-horizontal-reflection grid nil))
      (when hor
        (summing (* 100 hor))))))

(defun correct-smudge (grid)
  (bind ((original-hor (find-horizontal-reflection grid nil))
         (original-ver (find-vertical-reflection grid nil)))
    (iter outer
      (for y from 0 below (length grid))
      (iter
        (for x from 0 below (length (aref grid 0)))
        (for current = (aref (aref grid y) x))
        (setf (aref (aref grid y) x)
              (if (char-equal current #\.) #\# #\.))
        (for ver = (find-vertical-reflection grid original-ver))
        (when ver
          (return-from outer ver))
        (for hor = (find-horizontal-reflection grid original-hor))
        (when hor
          (return-from outer (* 100 hor)))
        (setf (aref (aref grid y) x) current)))))

(defun part-2 ()
  (bind ((problem (read-problem)))
    (iter
      (for grid in problem)
      (summing (correct-smudge grid)))))
