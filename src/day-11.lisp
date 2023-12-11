(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-11
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-11)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-11.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (nreverse acc)
                     (recur (cons line acc))))))
      (recur nil))))

(defun empty-rows (grid)
  (iter
    (for row in grid)
    (for i from 0)
    (when (every (lambda (c) (char-equal c #\.)) row)
      (collecting i))))

(defun empty-cols (grid)
  (iter
    (for x from 0 below (length (car grid)))
    (for i from 0)
    (when (iter
            (for y from 0 below (length grid))
            (always (char-equal (-> (nth y grid) (aref x)) #\.)))
      (collecting i))))

(defun star-coordinates (grid)
  (iter outer
    (for row in grid)
    (for y from 0)
    (iter
      (for x from 0)
      (for char in-vector row)
      (when (char-equal char #\#)
        (in outer (collecting (cons x y)))))))

(defun add-rows (grid rows)
  (bind ((next (pop rows)))
    (iter
      (for row in grid)
      (for i from 0)
      (when (eq i next)
        (collecting (coerce (iter (for i from 0 below (length row)) (collecting #\.))
                            'string))
        (setf next (pop rows)))
      (collecting row))))

(defun transpose (grid)
  (bind ((new-grid (make-array (list (length (car grid))
                                     (length grid)))))
    (iter
      (for row in grid)
      (for y from 0)
      (iter
        (for char in-string row)
        (for x from 0)
        (setf (aref new-grid x y) char)))
    (iter
      (for x from 0 below (length (car grid)))
      (collecting
       (coerce (iter
                 (for y from 0 below (length grid))
                 (collecting (aref new-grid x y)))
               'string)))))

(defun add-cols (grid cols)
  (-> (transpose grid)
    (add-rows cols)
    transpose))

(defun part-1 ()
  (bind ((problem (read-problem))
         (rows (empty-rows problem))
         (cols (empty-cols problem))
         (expanded (-> (add-rows problem rows)
                     (add-cols cols)))
         (coords (star-coordinates expanded)))
    (iter
      (for rest on coords)
      (for (start-x . start-y) = (car rest))
      (summing
       (iter
         (for other-coord in (cdr rest))
         (for (end-x . end-y) = other-coord)
         (summing (+ (abs (- start-x end-x))
                     (abs (- start-y end-y)))))))))

(defun part-2 ()
  (bind ((problem (read-problem))
         (rows (empty-rows problem))
         (cols (empty-cols problem))
         (coords (star-coordinates problem)))
    (iter
      (for rest on coords)
      (for (start-x . start-y) = (car rest))
      (summing
       (iter
         (for other-coord in (cdr rest))
         (for (end-x . end-y) = other-coord)
         (for max-x = (max start-x end-x))
         (for min-x = (min start-x end-x))
         (for max-y = (max start-y end-y))
         (for min-y = (min start-y end-y))
         (summing (+ (abs (- start-x end-x))
                     (* (1- 1000000)
                        (count-if (lambda (col) (and (< col max-x)
                                                     (> col min-x)))
                                  cols))
                     (abs (- start-y end-y))
                     (* (1- 1000000)
                        (count-if (lambda (row) (and (< row max-y)
                                                     (> row min-y)))
                                  rows)))))))))
