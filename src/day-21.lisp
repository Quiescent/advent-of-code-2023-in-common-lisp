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
      (when (>= (gethash current seen) max-distance)
        (next-iteration))
      (setf queue (less-first queue))
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

(defun count-separate (seen)
  (iter
    (for (key value) in-hashtable seen)
    (counting (= (mod value 2) 0) into lights)
    (counting (/= (mod value 2) 0) into darks)
    (finally (return (cons lights darks)))))

(defun invert-colour (colour)
  (if (eq colour 'light) 'dark 'light))

(defun meta-bfs (grid initial-remaining)
  (bind ((queue (empty-seq))
         (seen (make-hash-table :test #'equal))
         (start (starting-point grid))
         (start-x (realpart start))
         (start-y (imagpart start))
         (left-entry (complex 0 start-y))
         (right-entry (complex (1- (length (aref grid 0))) start-y))
         (top-entry (complex start-x 0))
         (bottom-entry (complex start-x (1- (length grid))))
         (directions (list (cons right left-entry)
                           (cons down top-entry)
                           (cons left right-entry)
                           (cons up bottom-entry)))
         (start-bfs (unlocked-bfs grid start (* 130 130)))
         (steps-to-fill (+ (floor (1- (length grid)) 2) (1- (length grid))))
         ((light-size . dark-size) (count-separate start-bfs)))
    (format t "steps-to-fill: ~a~%" steps-to-fill)
    (setq queue (with-last queue (list #c(0 0) (starting-point grid) initial-remaining 'light)))
    (iter
      (while (not (empty? queue)))
      (for (meta-coord entry-point remaining colour) = (first queue))
      (format t "meta-coord: ~a~%" meta-coord)
      (setf queue (less-first queue))
      (if (>= remaining steps-to-fill)
          (progn
            (setf (gethash meta-coord seen) (if (eq colour 'light) light-size dark-size))
            (iter
              (for (direction . next-start) in directions)
              (for next-meta-coord = (+ direction meta-coord))
              (when (not (gethash next-meta-coord seen))
                (setf queue (with-last queue (list next-meta-coord
                                                   next-start
                                                   (- remaining steps-to-fill)
                                                   (invert-colour colour)))))))
          (progn
            (print "HERE")
            (setf (gethash meta-coord seen)
                 (unlocked-bfs grid entry-point remaining)))))))

(defun part-2 (&optional (relative-path-name "src/day-21.in"))
  (bind ((problem (read-problem relative-path-name)))
    (meta-bfs problem 26501365)))

;; 130 x 130 grid

(defun test-2 ()
  (part-2 "src/day-21-test.in"))
