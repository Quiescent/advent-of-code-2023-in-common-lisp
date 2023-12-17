(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-17
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-17)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-17.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (-> (nreverse acc) (coerce 'vector))
                     (recur (cons line acc))))))
      (recur nil))))

(defun grid-at (grid c)
  (-> (aref grid (imagpart c))
    (aref (realpart c))
    digit-char-p))

(defun grid-in-bounds (grid c)
  (bind ((x-bound (length (aref grid 0)))
         (y-bound (length grid))
         (x (realpart c))
         (y (imagpart c)))
    (and (>= x 0)
         (>= y 0)
         (< x x-bound)
         (< y y-bound))))

(defvar right #c(1 0))
(defvar left #c(-1 0))
(defvar up #c(0 -1))
(defvar down #c(0 1))

(defun turn-left (c)
  (* c #c(0 -1)))

(defun turn-right (c)
  (* c #c(0 1)))

(defun sum-components (c)
  (+ (realpart c)
     (imagpart c)))

(defun cheapest-path (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (from (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (destination (complex (1- (length (aref grid 0)))
                               (1- (length grid))))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid)))
         (first-tile-cost (grid-at grid 0)))
    (labels ((recur (remaining)
               (format t "(list c len dir cost): ~a~%" (list c len dir cost))
               (iter
                 (for new-dir in (sort (list dir (turn-right dir) (turn-left dir)) #'> :key #'sum-components))
                 (for new-len = (if (= new-dir dir) (1+ len) 1))
                 (when (> new-len 3)
                   (next-iteration))
                 (for new-c = (+ c new-dir))
                 (when (not (grid-in-bounds grid new-c))
                   (next-iteration))
                 (for tile = (grid-at grid new-c))
                 (for new-cost = (+ cost tile))
                 (when (or (>= new-cost best-distance)
                           (<= (gethash (list new-c new-len new-dir) shortest most-positive-fixnum) new-cost)
                           (> (+ new-cost (- max-x (realpart new-c)) (- max-y (imagpart new-c))) best-distance))
                   (next-iteration))
                 ;; (format t "from: ~a~%" c)
                 ;; (format t "(list new-c new-len new-dir new-cost): ~a~%" (list new-c new-len new-dir new-cost))
                 (setf (gethash (list new-c new-len new-dir) shortest) new-cost)
                 (setf (gethash (list new-c new-len new-dir) from) (list c len dir))
                 (if (= destination new-c)
                     (setf best-distance (print new-cost))
                     (recur new-c new-len new-dir new-cost)))))
      (setf (gethash (cons 0 0) shortest) first-tile-cost)
      (recur (seq (list c len dir cost)))
      shortest
      from)))

(defun part-1 ()
  (bind ((problem (read-problem))
         (costs (cheapest-path problem))
         (c (list (complex 12 12) 1 right)))
    costs
    ;; (iter
    ;;   (while (/= (car c) 0))
    ;;   (format t "c: ~a~%" c)
    ;;   (setf c (gethash c costs)))
    ;; (gethash (complex (1- (length (aref problem 0)))
    ;;                   (1- (length problem)))
    ;;          costs)
    ))

;; Wrong: 1214
;; Wrong: 661
;; Wrong: 663

(defun part-2 ()
  (bind ((problem (read-problem)))
    ))
