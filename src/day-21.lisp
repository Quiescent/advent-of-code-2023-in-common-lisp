(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-21
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-21)

(defun read-problem (relative-path-name)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp relative-path-name))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (-> (nreverse acc) (coerce 'vector))
                     (recur (cons line acc))))))
      (recur nil))))

(defun starting-point (grid)
  (iter outer
    (for x from 0 below (length (aref grid 0)))
    (iter
      (for y from 0 below (length grid))
      (when (char-equal #\S (-> (aref grid y)
                              (aref x)))
        (return-from outer (complex x y))))))

(defvar up #c(0 -1))
(defvar down #c(0 1))
(defvar left #c(-1 0))
(defvar right #c(1 0))

(defun grid-get (grid c)
  (bind ((x (realpart c))
         (y (imagpart c)))
    (if (or (>= y (length grid))
            (< y 0)
            (>= x (length (aref grid 0)))
            (< x 0))
        #\#
        (-> (aref grid y)
          (aref x)))))

(defun bfs (grid)
  (bind ((queue (empty-seq))
         (seen (make-hash-table :test #'equal))
         (directions (list right down left up))
         (start (starting-point grid))
         (results (list)))
    (setf queue (with-last queue (cons start 0)))
    (setf (gethash (cons start 0) seen) 0)
    (iter
      (while (not (empty? queue)))
      (for (current . steps) = (first queue))
      (setf queue (less-first queue))
      (when (= steps 64)
        (push current results))
      (iter
        (for direction in directions)
        (for next = (+ current direction))
        (for next-state = (cons next (1+ steps)))
        (when (and (not (gethash next-state seen))
                   (char-equal #\. (grid-get grid next)))
          (setf (gethash next-state seen) t)
          (setf queue (with-last queue next-state)))))
    results))

(defun part-1 (&optional (relative-path-name "src/day-21.in"))
  (bind ((problem (read-problem relative-path-name))
         (distances (bfs problem)))
    (length distances)))

(defun test-1 ()
  (part-1 "src/day-21-test.in"))

(defun part-2 (&optional (relative-path-name "src/day-21.in"))
  (bind ((problem (read-problem relative-path-name)))
    ))

(defun test-2 ()
  (part-2 "src/day-21-test.in"))
