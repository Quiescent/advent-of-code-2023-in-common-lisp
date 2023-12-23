(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-23
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-23)

(defun read-problem (file-relative-path)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp file-relative-path))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (-> (nreverse acc) (coerce 'vector))
                     (recur (cons line acc))))))
      (recur nil))))

(defun grid-at (grid c)
  (-> (aref grid (imagpart c))
    (aref (realpart c))))

(defun coord (x y)
  (complex x y))

(defun y-dim (grid)
  (length grid))

(defun x-dim (grid)
  (length (aref grid 0)))

(defun starting-point (grid)
  (iter
    (for x from 0 below (x-dim grid))
    (for coord = (coord x 0))
    (finding coord such-that (char-equal (grid-at grid coord) #\.))))

(defun end-point (grid)
  (iter
    (for x from 0 below (x-dim grid))
    (for coord = (coord x (1- (y-dim grid))))
    (finding coord such-that (char-equal (grid-at grid coord) #\.))))

(defvar up #c(0 -1))
(defvar down #c(0 1))
(defvar left #c(-1 0))
(defvar right #c(1 0))
(defvar all-deltas (list up down left right))

(defun coord-x (c)
  (realpart c))

(defun coord-y (c)
  (imagpart c))

(defun is-in-bounds (grid coord)
  (and (< (coord-x coord) (x-dim grid))
       (>= (coord-x coord) 0)
       (< (coord-y coord) (y-dim grid))
       (>= (coord-y coord) 0)))

(defun entry-direction-ok (tile direction)
  (or (and (char-equal tile #\>)
           (= direction right))
      (and (char-equal tile #\<)
           (= direction left))
      (and (char-equal tile #\^)
           (= direction up))
      (and (char-equal tile #\v)
           (= direction down))))

(defun connecting-points (grid c)
  (iter
    (for delta in all-deltas)
    (for new-coord = (+ delta c))
    (when (is-in-bounds grid new-coord)
      (bind ((tile (grid-at grid new-coord)))
        (when (or (char-equal tile #\.)
                  (and (member tile (list #\> #\^ #\v #\<))
                       (entry-direction-ok tile delta)))
          (collecting new-coord))))))

(defun is-not-in-path-back (from start c dist test-c)
  (and (/= start test-c)
       (iter
         (while (/= start c))
         (never (= c test-c))
         (for (next-coord . next-dist) = (gethash (cons c dist) from))
         (setf c    next-coord
               dist next-dist))))

(defun print-path (from start c dist)
  (format t "FROM:~%")
  (format t "c: ~a~%" c)
  (iter
    (while (/= start c))
    (format t "c: ~a~%" c)
    (for (next-coord . next-dist) = (gethash (cons c dist) from))
    (setf c    next-coord
          dist next-dist)))

(defun longest-paths (grid)
  (bind ((start (starting-point grid))
         (end (end-point grid))
         (queue (seq (cons start 0)))
         (seen (make-hash-table :test #'equal))
         (from (make-hash-table :test #'equal)))
    (setf (gethash start seen) 0)
    (setf (gethash (cons start 0) from) (cons start 0))
    (iter
      (while (not (empty? queue)))
      (for (coord . dist) = (first queue))
      (when (/= dist (gethash coord seen))
        (next-iteration))
      (when (= coord end)
        (format t "(cons coord dist): ~a~%" (cons coord dist))
        (print-path from start coord dist))
      (setf queue (less-first queue))
      (iter
        (for new-coord in (connecting-points grid coord))
        (for new-dist = (1+ dist))
        (when (and (< (gethash new-coord seen most-negative-fixnum) new-dist)
                   (is-not-in-path-back from start coord dist new-coord))
          (setf (gethash new-coord seen) new-dist)
          (setf (gethash (cons new-coord new-dist) from) (cons coord dist))
          (setf queue (with-last queue (cons new-coord new-dist))))))
    (gethash end seen)))

(defun part-1 (&optional (file-relative-path "src/day-23.in"))
  (bind ((grid (read-problem file-relative-path)))
    (longest-paths grid)))

(defun test-1 ()
  (part-1 "src/day-23-test.in"))

(defun connecting-points-2 (grid c)
  (iter
    (for delta in all-deltas)
    (for new-coord = (+ delta c))
    (when (is-in-bounds grid new-coord)
      (bind ((tile (grid-at grid new-coord)))
        (when (or (char-equal tile #\.)
                  (member tile (list #\> #\^ #\v #\<)))
          (collecting new-coord))))))

(defun longest-paths-2 (grid)
  (bind ((start (starting-point grid))
         (end (end-point grid))
         (queue (seq (cons start 0)))
         (seen (make-hash-table :test #'equal))
         (from (make-hash-table :test #'equal)))
    (setf (gethash start seen) 0)
    (setf (gethash (cons start 0) from) (cons start 0))
    (iter
      (while (not (empty? queue)))
      (for (coord . dist) = (first queue))
      (when (/= dist (gethash coord seen))
        (next-iteration))
      (when (= coord end)
        (format t "(cons coord dist): ~a~%" (cons coord dist))
        (print-path from start coord dist))
      (setf queue (less-first queue))
      (iter
        (for new-coord in (connecting-points-2 grid coord))
        (for new-dist = (1+ dist))
        (when (and (< (gethash new-coord seen most-negative-fixnum) new-dist)
                   (is-not-in-path-back from start coord dist new-coord))
          (setf (gethash new-coord seen) new-dist)
          (setf (gethash (cons new-coord new-dist) from) (cons coord dist))
          (setf queue (with-last queue (cons new-coord new-dist))))))
    (gethash end seen)))

(defun part-2 (&optional (file-relative-path "src/day-23.in"))
  (bind ((problem (read-problem file-relative-path)))
    (longest-paths-2 problem)))

;; Wrong: 5018 (too low)
;; Also too low: 5019

(defun test-2 ()
  (part-2 "src/day-23-test.in"))
