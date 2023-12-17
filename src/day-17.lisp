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

;; (defun cheapest-path-dfs (grid)
;;   (bind ((shortest (make-hash-table :test #'equal))
;;          (best-distance most-positive-fixnum)
;;          (destination (complex (1- (length (aref grid 0)))
;;                                (1- (length grid))))
;;          (max-x (1- (length (aref grid 0))))
;;          (max-y (1- (length grid)))
;;          (first-tile-cost (grid-at grid 0)))
;;     (labels ((recur (c len dir cost)
;;                (iter
;;                  (for new-dir in (sort (list dir (turn-right dir) (turn-left dir)) #'> :key #'sum-components))
;;                  (for new-len = (if (= new-dir dir) (1+ len) 1))
;;                  (when (> new-len 3)
;;                    (next-iteration))
;;                  (for new-c = (+ c new-dir))
;;                  (when (not (grid-in-bounds grid new-c))
;;                    (next-iteration))
;;                  (for tile = (grid-at grid new-c))
;;                  (for new-cost = (+ cost tile))
;;                  (when (or (>= new-cost best-distance)
;;                            (<= (gethash (list new-c new-len new-dir) shortest most-positive-fixnum) new-cost)
;;                            (> (+ new-cost (- max-x (realpart new-c)) (- max-y (imagpart new-c))) best-distance))
;;                    (next-iteration))
;;                  (setf (gethash (list new-c new-len new-dir) shortest) new-cost)
;;                  (if (= destination new-c)
;;                      (setf best-distance (print new-cost))
;;                      (recur new-c new-len new-dir new-cost)))))
;;       (setf (gethash (cons 0 0) shortest) first-tile-cost)
;;       (recur c len dir cost)
;;       best-distance)))

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
  (bind ((problem (read-problem))
         (costs (cheapest-path problem))
         (c (list (complex 12 12) 1 right)))
    costs
    ;; (iter
    ;;   (while (/= (car c) 0))
    ;;   (format t "c: ~a~%" c)
    ;;   (setf c (gethash c costs)))
    
    ))

;; Wrong: 1214
;; Wrong: 661
;; Wrong: 663

(defun cheapest-path-dfs-2 (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance 900)
         (destination (complex (1- (length (aref grid 0)))
                               (1- (length grid))))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid)))
         (first-tile-cost (grid-at grid 0)))
    (labels ((recur (c len dir cost)
               (iter
                 (for new-dir in (sort (cons dir (when (>= len 4) (list (turn-right dir) (turn-left dir)))) #'> :key #'sum-components))
                 (for new-len = (if (= new-dir dir) (1+ len) 1))
                 (when (> new-len 10)
                   (next-iteration))
                 (for new-c = (+ c new-dir))
                 (when (not (grid-in-bounds grid new-c))
                   (next-iteration))
                 (for tile = (grid-at grid new-c))
                 (for new-cost = (+ cost tile))
                 (when (or (not (grid-in-bounds grid (+ new-c (* new-dir (max (- 4 new-len) 0)))))
                           (>= new-cost best-distance)
                           (<= (gethash (list new-c new-len new-dir) shortest most-positive-fixnum) new-cost)
                           (> (+ new-cost (- max-x (realpart new-c)) (- max-y (imagpart new-c))) best-distance))
                   (next-iteration))
                 (setf (gethash (list new-c new-len new-dir) shortest) new-cost)
                 (if (and (>= new-len 4) (= destination new-c))
                     (setf best-distance (print new-cost))
                     (recur new-c new-len new-dir new-cost)))))
      (setf (gethash (cons 0 0) shortest) first-tile-cost)
      (recur 0 0 right first-tile-cost)
      best-distance)))

(defun cheapest-path-2 (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance 900 ;; 900 ;; 808;; (cheapest-path-dfs-2 grid)
                        )
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
          ;; (format t "(list c cost len dir): ~a~%" (list c cost len dir))
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

(defun cheapest-2-1 (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (destination (complex (1- (length (aref grid 0))) (1- (length grid))))
         (max-x (1- (length (aref grid 0))))
         (max-y (1- (length grid))))
    (labels ((manhattan-to-end (c)
               (+ (- max-x (realpart c)) (- max-y (imagpart c))))
             (heap-manhattan (elem)
               (+ (cadr elem) (* 100 (manhattan-to-end (car elem))))))
      (bind ((remaining (datastructures:create-heap :key #'heap-manhattan)))
        (datastructures:insert remaining (list 0 0 right))
        (datastructures:insert remaining (list 0 0 down))
        (labels ((enqueue-next-points (dir c cost)
                   (iter
                     (for new-dir in (list (turn-right dir) (turn-left dir)))
                     (bind ((new-cost cost))
                       (iter
                         (for i from 1 to 3)
                         (for d = (* i new-dir))
                         (for new-c = (+ d c))
                         (when (not (grid-in-bounds grid new-c))
                           (finish))
                         (incf new-cost (grid-at grid new-c))
                         (when (<= (gethash new-c shortest most-positive-fixnum) new-cost)
                           (next-iteration))
                         (datastructures:insert remaining (list new-c new-cost new-dir))
                         (setf 
                          (gethash c shortest) new-cost))))))
          (iter
            (while (not (datastructures::heap-empty remaining)))
            (for (c cost dir) = (datastructures:del-min remaining))
            (when (and (= c destination)
                       (< cost best-distance))
              (print (setf best-distance cost)))
            (enqueue-next-points dir c cost)))))
    best-distance))

(defun cheapest-2 (grid)
  (bind ((shortest (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (remaining (seq (list 0 0 right) (list 0 0 down)))
         (destination (complex (1- (length (aref grid 0))) (1- (length grid)))))
    (labels ((next-points (dir c cost)
               (bind ((new-points (seq)))
                (iter
                  (for new-dir in (list (turn-right dir) (turn-left dir)))
                  (bind ((new-cost cost))
                    (iter
                      (for i from 4 to 10)
                      (for d = (* i new-dir))
                      (for new-c = (+ d c))
                      ;; (format t "new-c: ~a~%" new-c)
                      (when (not (grid-in-bounds grid new-c))
                        (finish))
                      (incf new-cost (grid-at grid new-c))
                      (when (<= (gethash new-c shortest most-positive-fixnum) new-cost)
                        (next-iteration))
                      (setf new-points           (with-last new-points (list new-c new-cost new-dir))
                            (gethash c shortest) new-cost)))
                  (finally (return new-points))))))
      (iter
        (while (not (empty? remaining)))
        (for (c cost dir) = (first remaining))
        (setf remaining (less-first remaining))
        (format t "(list c cost dir): ~a~%" (list c cost dir))
        (when (= c destination)
          (print (setf best-distance cost)))
        (setf remaining (concat remaining (next-points dir c cost)))))
    best-distance))

(defun part-2 ()
  (bind ((problem (read-problem)))
    ;; (cheapest-path-dfs-2 problem)
    ;; (cheapest-path-2 problem)
    (cheapest-2-1 problem)
    ))

;; Wrong: 807
;; Wrong: 874
;; Wrong: 824
;; Wrong: 805
