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
                     (recur (cons (cl:map 'vector #'digit-char-p line) acc))))))
      (recur nil))))

(defun grid-at (grid c)
  (-> (aref grid (imagpart c))
    (aref (realpart c))))

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

(defun cheapest-path (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (destination (complex (1- (length (aref grid 0)))
                               (1- (length grid))))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid)))
         (first-tile-cost (grid-at grid 0))
         (remaining (seq (list 0 0 right 0))))
    (setf (gethash (cons 0 0) shortest) first-tile-cost)
    (iter
      (while (not (empty? remaining)))
      (for (c len dir cost) = (first remaining))
      (setf remaining (less-first remaining))
      (setf (gethash (list c len dir) shortest) cost)
      (when (= destination c)
        (setf best-distance (print cost))
        (next-iteration))
      (iter
        (for new-dir in (list dir (turn-right dir) (turn-left dir)))
        (for new-len = (if (= new-dir dir) (1+ len) 1))
        (when (> new-len 3)
          (next-iteration))
        (for new-c = (+ c new-dir))
        (when (not (grid-in-bounds grid new-c))
          (next-iteration))
        (for tile = (grid-at grid new-c))
        (for new-cost = (+ cost tile))
        (when (not (or (>= new-cost best-distance)
                       (<= (gethash (list new-c new-len new-dir) shortest most-positive-fixnum) new-cost)
                       (> (+ new-cost (- max-x (realpart new-c)) (- max-y (imagpart new-c))) best-distance)))
          (setf remaining (with-first remaining (list new-c new-len new-dir new-cost))))))
    best-distance))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (cheapest-path problem)))

(defun sum-components (c)
  (+ (realpart c)
     (imagpart c)))

(defun cheapest-path-2 (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (destination (complex (1- (length (aref grid 0)))
                               (1- (length grid))))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid)))
         (first-tile-cost (grid-at grid 0)))
    (labels ((manhattan-to-end (c)
               (+ (- max-x (realpart c)) (- max-y (imagpart c))))
             (heap-manhattan (elem)
               (+ (cadr elem) (* 100 (manhattan-to-end (car elem))))))
      (bind ((remaining (datastructures:create-heap :key #'heap-manhattan)))
        (datastructures:insert remaining (list 0 0 0 right))
        (datastructures:insert remaining (list 0 0 0 down))
        (setf (gethash (cons 0 0) shortest) first-tile-cost)
        (iter
          (while (not (datastructures::heap-empty remaining)))
          (for (c cost len dir) = (datastructures:del-min remaining))
          (setf (gethash (list c len dir) shortest) cost)
          (when (and (= destination c)
                     (>= len 4))
            (setf best-distance (print cost))
            (next-iteration))
          (iter
            (for new-dir in (cons dir (when (>= len 4) (list (turn-right dir) (turn-left dir)))))
            (for new-len = (if (= new-dir dir) (1+ len) 1))
            (when (> new-len 10)
              (next-iteration))
            (for new-c = (+ c new-dir))
            (when (not (grid-in-bounds grid new-c))
              (next-iteration))
            (for tile = (grid-at grid new-c))
            (for new-cost = (+ cost tile))
            (when (not (or (not (grid-in-bounds grid (+ new-c (* new-dir (max (- 4 new-len) 0)))))
                           (>= new-cost best-distance)
                           (<= (gethash (list new-c new-len new-dir) shortest most-positive-fixnum) new-cost)
                           (> (+ new-cost (manhattan-to-end new-c)) best-distance)))
              (datastructures:insert remaining (list new-c new-cost new-len new-dir)))))
        best-distance))))

(defun cheapest-2-dfs (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (destination (complex (1- (length (aref grid 0))) (1- (length grid))))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid))))
    (labels ((manhattan-to-end (c)
               (+ (- max-x (realpart c)) (- max-y (imagpart c))))
             (recur (c cost dir)
               ;; (format t "(list c cost dir): ~a~%" (list c cost dir))
               (when (and (< cost best-distance)
                          (<= cost (gethash (cons c dir) shortest most-positive-fixnum)))
                 (if (= c destination)
                     (print (setf best-distance cost))
                     (iter
                       (for new-dir in (sort (list (turn-right dir) (turn-left dir)) #'> :key #'sum-components))
                       (bind ((new-cost (+ cost (or (iter
                                                      (for i from 1 below 4)
                                                      (for d = (* i new-dir))
                                                      (for new-c = (+ d c))
                                                      (when (not (grid-in-bounds grid new-c))
                                                        (finish))
                                                      (summing (grid-at grid new-c)))
                                                    0))))
                         (iter
                           (for i from 4 to 10)
                           (for d = (* i new-dir))
                           (for new-c = (+ d c))
                           ;; (format t "new-c: ~a~%" new-c)
                           (when (not (grid-in-bounds grid new-c))
                             (finish))
                           (incf new-cost (grid-at grid new-c))
                           (when (or (< (gethash (cons new-c new-dir) shortest most-positive-fixnum) new-cost)
                                     (>= new-cost best-distance)
                                     (> (+ new-cost (manhattan-to-end new-c)) best-distance))
                             (next-iteration))
                           (setf (gethash (cons new-c new-dir) shortest) new-cost)
                           (recur new-c new-cost new-dir))))))))
      (recur 0 0 right)
      (recur 0 0 down))
    best-distance))

(defun cheapest-2-alt (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid)))
         (queue (datastructures:create-heap :key #'car))
         (destination (complex (1- (length (aref grid 0))) (1- (length grid)))))
    (labels ((vertices (cost c dir)
               (iter outer
                 (for new-dir in (list (turn-left dir) (turn-right dir)))
                 (bind ((new-cost (+ cost
                                     (iter
                                       (for i from 1 below 4)
                                       (for d = (* new-dir i))
                                       (for new-c = (+ c d))
                                       (while (grid-in-bounds grid new-c))
                                       (summing (grid-at grid new-c))))))
                   (iter
                     (for i from 4 to 10)
                     (for d = (* i new-dir))
                     (for new-c = (+ c d))
                     (while (grid-in-bounds grid new-c))
                     (incf new-cost (grid-at grid new-c))
                     (in outer (collecting (list new-cost new-c new-dir)))))))
             (relax (cost c dir)
               ;; (format t "(vertices cost c dir): ~a~%" (vertices cost c dir))
               (when (= cost (gethash (cons c dir) shortest))
                (iter
                  (for (new-cost new-c new-dir) in (vertices cost c dir))
                  (bind ((current (gethash (cons new-c new-dir) shortest most-positive-fixnum)))
                    (when (< new-cost current)
                      (setf (gethash (cons new-c new-dir) shortest) new-cost)
                      (datastructures:insert queue (list new-cost new-c new-dir))))))))
      (setf (gethash (cons 0 right) shortest) (grid-at grid 0))
      (relax (grid-at grid 0) 0 right)
      (setf (gethash (cons 0 down) shortest) (grid-at grid 0))
      (relax (grid-at grid 0) 0 down)
      (iter
        (while (not (datastructures::heap-empty queue)))
        (for (cost c dir) = (datastructures:del-min queue))
        (when (= c destination)
          (format t "cost: ~a~%" cost))
        (when (= cost (gethash (cons c dir) shortest most-positive-fixnum))
          (relax cost c dir))))
    (min (gethash (cons (complex max-x max-y) down) shortest)
         (gethash (cons (complex max-x max-y) right) shortest))
    ;; shortest
    ))

(defun part-2 ()
  (bind ((problem (read-problem)))
    (cheapest-2-alt problem)
    ;; (cheapest-2-dfs problem)
    ))
