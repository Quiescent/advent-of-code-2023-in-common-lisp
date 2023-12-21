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
         (start (starting-point grid)))
    (setf queue (with-last queue start))
    (setf (gethash start seen) 0)
    (iter
      (while (not (empty? queue)))
      (for current = (first queue))
      ;; (format t "current: ~a~%" current)
      (setf queue (less-first queue))
      (when (>= (gethash current seen) 64)
        (next-iteration))
      (iter
        (for direction in directions)
        (for next = (+ current direction))
        (when (and (not (gethash next seen))
                   (char-equal #\. (grid-get grid next)))
          (setf (gethash next seen)
                (1+ (gethash current seen)))
          (setf queue (with-last queue next)))))
    seen))

(defun part-1 (&optional (relative-path-name "src/day-21.in"))
  (bind ((problem (read-problem relative-path-name))
         (distances (bfs problem)))
    (iter
      (for (key value) in-hashtable distances)
      (counting (= 0 (mod value 2))))))

(defun test-1 ()
  (part-1 "src/day-21-test.in"))

(defun unlocked-bfs (grid start max-distance)
  (bind ((queue (empty-seq))
         (seen (make-hash-table :test #'equal))
         (directions (list right down left up)))
    (setf queue (with-last queue start))
    (setf (gethash start seen) 0)
    (iter
      (while (not (empty? queue)))
      (for current = (first queue))
      ;; (format t "current: ~a~%" current)
      (setf queue (less-first queue))
      (when (>= (gethash current seen) max-distance)
        (next-iteration))
      (iter
        (for direction in directions)
        (for next = (+ current direction))
        (when (and (not (gethash next seen))
                   (or (char-equal #\. (grid-get grid next))
                       (char-equal #\S (grid-get grid next))))
          (setf (gethash next seen)
                (1+ (gethash current seen)))
          (setf queue (with-last queue next)))))
    seen))

(defun count-reachable (seen)
  (iter
    (for (key value) in-hashtable seen)
    (counting (= (mod value 2) 1))))

(defun count-reachable-odds (seen)
  (iter
    (for (key value) in-hashtable seen)
    (counting (= (mod value 2) 0))))

(defun merge-seen (seen-1 seen-2)
  (bind ((new-seen (make-hash-table :test #'equal)))
    (iter
      (for (key-1 value-1) in-hashtable seen-1)
      (setf (gethash key-1 new-seen) value-1))
    (iter
      (for (key-2 value-2) in-hashtable seen-2)
      (setf (gethash key-2 new-seen) value-2))
    new-seen))

(defun sum-odds (m)
  (iter
    (for i from 1 to m by 2)
    (summing i)))

(defun diamond-perimeter (tri-length)
  (if (= 1 tri-length)
      1
      (* 4 (1- tri-length))))

(defun totals (grid initial-remaining)
  (bind ((start (starting-point grid))
         (start-x (realpart start))
         (start-y (imagpart start))
         (left-entry (complex 0 start-y))
         (right-entry (complex (1- (length (aref grid 0))) start-y))
         (top-entry (complex start-x 0))
         (bottom-entry (complex start-x (1- (length grid))))
         (start-bfs (unlocked-bfs grid start (* (length grid) (length grid))))
         (steps-remaining (length grid))
         (seen-entering-bottom (unlocked-bfs grid
                                             bottom-entry
                                             steps-remaining))
         (seen-entering-left (unlocked-bfs grid
                                           left-entry
                                           steps-remaining))
         (seen-entering-right (unlocked-bfs grid
                                            right-entry
                                            steps-remaining))
         (seen-entering-top (unlocked-bfs grid
                                          top-entry
                                          steps-remaining))
         (seen-bottom (count-reachable-odds seen-entering-bottom))
         (seen-left (count-reachable-odds seen-entering-left))
         (seen-right (count-reachable-odds seen-entering-right))
         (seen-top (count-reachable-odds seen-entering-top))
         (seen-top-right-edge (count-reachable-odds (merge-seen seen-entering-bottom
                                                           seen-entering-left)))
         (seen-bottom-right-edge (count-reachable-odds (merge-seen seen-entering-top
                                                              seen-entering-left)))
         (seen-top-left-edge (count-reachable-odds (merge-seen seen-entering-right
                                                          seen-entering-bottom)))
         (seen-bottom-left-edge (count-reachable-odds (merge-seen seen-entering-right
                                                             seen-entering-top)))
         (all-seen-reachable-evens (count-reachable start-bfs))
         (all-seen-reachable-odds (count-reachable-odds start-bfs))
         (tri-length (floor initial-remaining (length grid)))
         (side-to-side (1+ (* 2 (1- tri-length))))
         (interior-size (+ side-to-side (* 2 (sum-odds (- side-to-side 2)))))
         (interior-size-evens (iter
                                (for i from 2 below tri-length by 2)
                                ;; (format t "i: ~a~%" (diamond-perimeter i))
                                (summing (diamond-perimeter i))))
         (interior-size-odds (iter
                               (for i from 1 below tri-length by 2)
                               ;; (format t "i: ~a~%" (diamond-perimeter i))
                               (summing (diamond-perimeter i))))
         (hypo-len (floor (1- (* tri-length 2)) 2)))
    (format t "all-seen-reachable-evens: ~a~%" all-seen-reachable-evens)
    (format t "all-seen-reachable-odds: ~a~%" all-seen-reachable-odds)
    (format t "side-to-side: ~a~%" side-to-side)
    (format t "initial-remaining: ~a~%" initial-remaining)
    (format t "interior-size: ~a~%" interior-size)
    (format t "tri-length: ~a~%" tri-length)
    (format t "hypo-len: ~a~%" hypo-len)
    (format t "seen-bottom: ~a~%" seen-bottom)
    (format t "seen-top: ~a~%" seen-top)
    (format t "seen-left: ~a~%" seen-top)
    (format t "seen-right: ~a~%" seen-top)
    (+ (* all-seen-reachable-evens interior-size-odds)
       (* all-seen-reachable-odds interior-size-evens)
       seen-bottom
       seen-left
       seen-right
       seen-top
       (* seen-top-right-edge hypo-len)
       (* seen-bottom-right-edge hypo-len)
       (* seen-top-left-edge hypo-len)
       (* seen-bottom-left-edge hypo-len))))

(defun part-2 (&optional (relative-path-name "src/day-21.in"))
  (bind ((problem (read-problem relative-path-name)))
    (totals problem 26501365 ;; 589 ;; 458 ;; 26501365
            )))

;; 130 x 130 grid
;; Wrong: 623128465540000
;; Wrong: 623119228394556
;; Wrong: 2492501544794756
;; Wrong: 2492501544768136
;; Wrong: 623124610114329
;; Wrong: 623130770523477
;; Wrong: 623130770550097
;; Wrong: 620757164217893
;; Wrong: 621937848921722
;; Wrong: 621937800167297
;; Wrong: 621937788464320

(defun test-2 ()
  (part-2 "src/day-21-test.in"))
