(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-14
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-14)

(defun parse-line (line)
  line)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-14.in"))
    (labels ((recur (acc)
               (bind ((line (parse-line (read-line f nil nil))))
                 (if (null line)
                     (coerce (nreverse acc) 'vector)
                     (recur (cons line acc))))))
      (recur nil))))

(defun roll-north (x y-start grid)
  (iter
    (for y from (1- y-start) downto 0)
    (for next-block = (-> (aref grid y) (aref x)))
    (when (char-equal next-block #\.)
      (setf (-> (aref grid y)      (aref x)) #\O
            (-> (aref grid (1+ y)) (aref x)) #\.))
    (when (or (char-equal next-block #\#)
              (char-equal next-block #\O))
      (return))))

(defun roll-rocks-north (grid)
  (iter
    (for y from 0 below (length grid))
    (iter
      (for x from 0 below (length (aref grid 0)))
      (for block = (-> (aref grid y) (aref x)))
      (when (char-equal block #\O)
        (roll-north x y grid)))
    (finally (return grid))))

(defun compute-load (grid)
  (iter outer
    (for y from 0 below (length grid))
    (iter
      (for x from 0 below (length (aref grid 0)))
      (for block = (-> (aref grid y) (aref x)))
      (when (char-equal block #\O)
        (in outer (summing (- (length grid) y)))))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (roll-rocks-north problem)
    (compute-load problem)))

(defun roll-south (x y-start grid)
  (iter
    (for y from (1+ y-start) below (length grid))
    (for next-block = (-> (aref grid y) (aref x)))
    (when (char-equal next-block #\.)
      (setf (-> (aref grid y)      (aref x)) #\O
            (-> (aref grid (1- y)) (aref x)) #\.))
    (when (or (char-equal next-block #\#)
              (char-equal next-block #\O))
      (return))))

(defun roll-rocks-south (grid)
  (iter
    (for y from (1- (length grid)) downto 0)
    (iter
      (for x from 0 below (length (aref grid 0)))
      (for block = (-> (aref grid y) (aref x)))
      (when (char-equal block #\O)
        (roll-south x y grid)))
    (finally (return grid))))

(defun roll-east (x-start y grid)
  (iter
    (for x from (1+ x-start) below (length (aref grid 0)))
    (for next-block = (-> (aref grid y) (aref x)))
    (when (char-equal next-block #\.)
      (setf (-> (aref grid y) (aref x)) #\O
            (-> (aref grid y) (aref (1- x)))      #\.))
    (when (or (char-equal next-block #\#)
              (char-equal next-block #\O))
      (return))))

(defun roll-rocks-east (grid)
  (iter
    (for x from (1- (length (aref grid 0))) downto 0)
    (iter
      (for y from 0 below (length grid))
      (for block = (-> (aref grid y) (aref x)))
      (when (char-equal block #\O)
        (roll-east x y grid)))
    (finally (return grid))))

(defun roll-west (x-start y grid)
  (iter
    (for x from (1- x-start) downto 0)
    (for next-block = (-> (aref grid y) (aref x)))
    (when (char-equal next-block #\.)
      (setf (-> (aref grid y) (aref x))      #\O
            (-> (aref grid y) (aref (1+ x))) #\.))
    (when (or (char-equal next-block #\#)
              (char-equal next-block #\O))
      (return))))

(defun roll-rocks-west (grid)
  (iter
    (for x from 0 below (length grid))
    (iter
      (for y from 0 below (length grid))
      (for block = (-> (aref grid y) (aref x)))
      (when (char-equal block #\O)
        (roll-west x y grid)))
    (finally (return grid))))

(defun grid-to-key (grid)
  (reduce (lambda (acc next)
            (concatenate 'string acc next))
          grid))

(defun print-grid (grid)
  (iter
    (for row in-vector grid)
    (format t "~a~%" row))
  (format t "~%"))

(defun part-2 ()
  (bind ((problem (read-problem))
         (orig-problem (cl:map 'vector #'copy-seq problem))
         (seen (make-hash-table :test #'equal))
         (cycle-length-1 (iter
                           (initially
                            (setf (gethash (grid-to-key problem) seen) t))
                           (for i from 0 below 10000)
                           (-> (roll-rocks-north problem)
                             roll-rocks-west
                             roll-rocks-south
                             roll-rocks-east)
                           (when (gethash (grid-to-key problem) seen)
                             (return (1+ i)))
                           (setf (gethash (grid-to-key problem) seen) t)))
         (cycle-length-2 (iter
                           (initially
                            (setf seen (make-hash-table :test #'equal))
                            (setf (gethash (grid-to-key problem) seen) t))
                           (for i from 0 below 10000)
                           (-> (roll-rocks-north problem)
                             roll-rocks-west
                             roll-rocks-south
                             roll-rocks-east)
                           (when (gethash (grid-to-key problem) seen)
                             (return (1+ i)))
                           (setf (gethash (grid-to-key problem) seen) t))))
    (dotimes (n (+ cycle-length-1 (mod (- 1000000000 cycle-length-1) cycle-length-2)))
      (-> (roll-rocks-north orig-problem)
        roll-rocks-west
        roll-rocks-south
        roll-rocks-east))
    (compute-load orig-problem)))
