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
(defvar left-list (list left))
(defvar right-list (list right))
(defvar down-list (list down))
(defvar up-list (list up))

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
           ((eq direction right) up-list)
           ((eq direction left) down-list)
           ((eq direction up) right-list)
           ((eq direction down) left-list)))
    (#\\ (cond
           ((eq direction right) down-list)
           ((eq direction left) up-list)
           ((eq direction up) left-list)
           ((eq direction down) right-list)))))

(defun grid-at (grid c)
  (-> (aref grid (imagpart c))
    (aref (realpart c))))

(defun trace-beam (grid initial-position initial-direction)
  (bind ((seen (make-hash-table :test #'equal))
         (x-len (length (aref grid 0)))
         (y-len (length grid)))
    (labels ((in-bounds (position)
               (bind ((x (realpart position))
                      (y (imagpart position)))
                 (and (not (< x 0))
                      (not (< y 0))
                      (not (>= x x-len))
                      (not (>= y y-len)))))
             (recur (position direction)
               (bind ((new-position (+ position direction)))
                 (when (in-bounds new-position)
                   (bind ((tile (grid-at grid new-position))
                          (new-directions (interact direction tile)))
                     (iter
                       (for new-direction in new-directions)
                       (when (member new-direction #1=(gethash new-position seen))
                         (next-iteration))
                       (push new-direction #1#)
                       (recur new-position new-direction)))))))
      (push initial-direction (gethash initial-position seen))
      (iter
        (for new-direction in (interact initial-direction (grid-at grid initial-position)))
        (recur initial-position new-direction))
      seen)))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (->> (trace-beam problem #c(0 0) right)
      hash-table-count)))

(defun energized-tiles (grid initial-position initial-direction)
  (->> (trace-beam grid
                   initial-position
                   initial-direction)
    hash-table-count))

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
