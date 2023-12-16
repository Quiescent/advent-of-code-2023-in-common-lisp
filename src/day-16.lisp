(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-16
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-16)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-16.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (-> (nreverse acc) (coerce 'vector))
                     (recur (cons line acc))))))
      (recur nil))))

(defvar left #c(-1 0))
(defvar right #c(1 0))
(defvar down #c(0 1))
(defvar up #c(0 -1))
(defvar up-down    (list up down))
(defvar left-right (list left right))

(defun interact (direction tile)
  (case tile
    (#\. (list direction))
    (#\| (if (or (eq direction left) (eq direction right))
             up-down
             (list direction)))
    (#\- (if (or (eq direction up) (eq direction down))
             left-right
             (list direction)))
    (#\/ (cond
           ((eq direction right) (list up))
           ((eq direction left) (list down))
           ((eq direction up) (list right))
           ((eq direction down) (list left))))
    (#\\ (cond
           ((eq direction right) (list down))
           ((eq direction left) (list up))
           ((eq direction up) (list left))
           ((eq direction down) (list right))))))

(defun grid-at (grid c)
  (-> (aref grid (imagpart c))
    (aref (realpart c))))

(defun in-bounds (position grid)
  (and (not (< (realpart position) 0))
       (not (< (imagpart position) 0))
       (not (>= (realpart position) (length (aref grid 0))))
       (not (>= (imagpart position) (length grid)))))

(defun trace-beam (grid initial-position initial-direction)
  (bind ((seen (make-hash-table :test #'equal)))
    (labels ((recur (position direction)
               (bind ((new-position (+ position direction)))
                 (when (in-bounds new-position grid)
                   (bind ((tile (grid-at grid new-position))
                          (new-directions (interact direction tile)))
                     (iter
                       (for new-direction in new-directions)
                       (when #1=(gethash (cons new-position new-direction) seen)
                             (next-iteration))
                       
                       (setf #1# t)
                       (recur new-position new-direction)))))))
      (setf (gethash (cons initial-position initial-direction) seen) t)
      (iter
        (for new-direction in (interact initial-direction (grid-at grid initial-position)))
        (recur initial-position new-direction))
      seen)))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (->> (iter
           (for (key value) in-hashtable (trace-beam problem #c(0 0) right))
           (collecting (car key)))
      remove-duplicates
      length)))

(defun energized-tiles (grid initial-position initial-direction)
  (->> (iter
         (for (key value) in-hashtable (trace-beam grid
                                                   initial-position
                                                   initial-direction))
         (collecting (car key)))
    remove-duplicates
    length))

(defun part-2 ()
  (bind ((problem (read-problem)))
    (max (iter
           (for y from 0 below (length problem))
           (maximize (energized-tiles problem (complex 0 y) right))
           (maximize (energized-tiles problem (complex (1- (length (aref problem 0))) y) left)))
         (iter
           (for x from 0 below (length (aref problem 0)))
           (maximize (energized-tiles problem (complex x 0) down))
           (maximize (energized-tiles problem (complex x (1- (length problem))) up))))))
