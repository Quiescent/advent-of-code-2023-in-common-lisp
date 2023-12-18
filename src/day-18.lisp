(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-18
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-18)

(defun parse-line (line)
  (match line
    ((ppcre "([UDLR]) ([0-9]+) \\((.*)\\)"
            (read direction)
            (read amount)
            colour)
     (list direction amount colour))))

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-18.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (nreverse acc)
                     (recur (cons (parse-line line) acc))))))
      (recur nil))))

(defvar up #c(0 -1))
(defvar down #c(0 1))
(defvar right #c(1 0))
(defvar left #c(-1 0))

(defun direction-to-complex (direction)
  (case direction
    (u up)
    (d down)
    (r right)
    (l left)))

(defun direction-between (c1 c2)
  (case (- c2 c1)
    (#c(0 -1) 'u)
    (#c(0 1) 'd)
    (#c(1 0) 'l)
    (#c(-1 0) 'r)))

(defun vector-pair-for-direction (direction)
  (case direction
    (u (cons left right))
    (d (cons left right))
    (r (cons up down))
    (l (cons up down))))

(defun grid-bounds (grid)
  (iter
    (for (key value) in-hashtable grid)
    (for x = (realpart key))
    (for y = (imagpart key))
    (minimizing x into min-x)
    (maximizing x into max-x)
    (minimizing y into min-y)
    (maximizing y into max-y)
    (finally (return (list min-x min-y max-x max-y)))))

(defun connects-to-outside (grid vectors loop)
  (bind (((min-x min-y max-x max-y) (grid-bounds grid)))
    (iter
      (for vector in vectors)
      (for c in (cdr loop))
      (for moved = (+ c vector))
      (for x = (realpart moved))
      (for y = (imagpart moved))
      (thereis (or (< x min-x)
                   (< y min-y)
                   (> x max-x)
                   (> y max-y))))))

(defun outside-and-inside-edges (grid loop)
  (iter
    (for cs on loop)
    (for c1 = (car cs))
    (for c2 = (cadr cs))
    (when (and c1 c2)
      (for direction = (direction-between c1 c2))
      (for (one . other) = (vector-pair-for-direction direction))
      (collecting one into one-side)
      (collecting other into other-side))
    (finally
     (return
       (if (connects-to-outside grid one-side loop)
           (list one-side other-side)
           (list other-side one-side))))))

(defun dig-holes (specs)
  (bind ((grid (make-hash-table :test #'equal))
         (c (complex 0 0))
         (loop (list c)))
    (setf (gethash c grid) t)
    (iter
      (for (direction amount colour) in specs)
      (iter
        (for i from 0 below amount)
        (for dir = (direction-to-complex direction))
        (incf c dir)
        (setf (gethash c grid) colour)
        (push c loop)))
    (cons loop grid)))

(defun flood-fill (grid loop)
  (bind ((flood-filled (make-hash-table :test #'equal))
         ((min-x min-y max-x max-y) (grid-bounds grid))
         (seen-dfs (make-hash-table :test #'equal)))
    (labels ((dfs (x y i)
               (cond
                 ((> i (max (- max-y min-y) (- max-x min-x))) nil)
                 ((or (<= x min-x)
                          (<= y min-y)
                          (>= x max-x)
                          (>= y max-y))
                      t)
                   (t (progn
                      (setf (gethash (cons x y) seen-dfs) t)
                      (iter
                        (for next-coord in (list (cons (1+ x) y)
                                                 (cons x (1+ y))
                                                 (cons x (1- y))
                                                 (cons (1- x) y)))
                        (thereis (and (not (gethash next-coord seen-dfs))
                                      (not (gethash (complex (car next-coord) (cdr next-coord)) grid))
                                      (setf (gethash next-coord seen-dfs) t)
                                      (dfs (car next-coord) (cdr next-coord) (1+ i)))))))))
             (bfs (x y)
               (bind ((remaining (seq (cons x y)))
                      (label (if (progn
                                   (setf seen-dfs (make-hash-table :test #'equal))
                                   (dfs x y 0))
                                 'out
                                 'in)))
                 (iter
                   (while (not (empty? remaining)))
                   (for (x . y) = (first remaining))
                   (setf remaining (less-first remaining))
                   (setf (gethash (cons x y) flood-filled) label)
                   (when (and (> x min-x)
                              (null (gethash (cons (1- x) y) flood-filled))
                              (null (gethash (complex (1- x) y) grid)))
                     (setf (gethash (cons (1- x) y) flood-filled) label)
                     (setf remaining (with-last remaining (cons (1- x) y))))
                   (when (and (< x max-x)
                              (null (gethash (cons (1+ x) y) flood-filled))
                              (null (gethash (complex (1+ x) y) grid)))
                     (setf (gethash (cons (1+ x) y) flood-filled) label)
                     (setf remaining (with-last remaining (cons (1+ x) y))))
                   (when (and (> y min-y)
                              (null (gethash (cons x (1- y)) flood-filled))
                              (null (gethash (complex x (1- y)) grid)))
                     (setf (gethash (cons x (1- y)) flood-filled) label)
                     (setf remaining (with-last remaining (cons x (1- y)))))
                   (when (and (< y max-y)
                              (null (gethash (cons x (1+ y)) flood-filled))
                              (null (gethash (complex x (1+ y)) grid)))
                     (setf (gethash (cons x (1+ y)) flood-filled) label)
                     (setf remaining (with-last remaining (cons x (1+ y)))))))))
      (iter
        (for x from min-x to max-x)
        (iter
          (for y from min-y to max-y)
          (when (and (null (gethash (complex x y) grid))
                     (null (gethash (cons x y) flood-filled)))
            (bfs x y))))
      flood-filled)))

(defun grid-bounds-xy (grid)
  (iter
    (for (key value) in-hashtable grid)
    (for x = (car key))
    (for y = (cdr key))
    (minimizing x into min-x)
    (maximizing x into max-x)
    (minimizing y into min-y)
    (maximizing y into max-y)
    (finally (return (list min-x min-y max-x max-y)))))

(defun draw-grid (grid loop-grid)
  (bind (((min-x min-y max-x max-y) (grid-bounds loop-grid)))
    (iter
        (for y from min-y to max-y)
        (iter
          (for x from min-x to max-x)
          (if (or (eq (gethash (cons x y) grid) 'in)
                  (gethash (complex x y) loop-grid))
              (format t "#")
              (format t ".")))
      (format t "~%"))))

(defun part-1 ()
  (bind ((problem (read-problem))
         ((loop . grid) (dig-holes problem))
         (filled (flood-fill grid loop)))
    (draw-grid filled grid)
    (+ (1- (length loop))
       (iter
         (for (key value) in-hashtable filled)
         (counting (eq value 'in))))))

(defun parse-line-2 (line)
  (match line
    ((ppcre "[UDLR] [0-9]+ \\((.*)\\)"
            colour)
     (list (case (digit-char-p (aref colour 6))
             (0 'r)
             (1 'd)
             (2 'l)
             (3 'u))
           (read-from-string (format nil "#x~a" (subseq colour 1 6)))))))

(defun read-problem-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-18.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (nreverse acc)
                     (recur (cons (parse-line-2 line) acc))))))
      (recur nil))))

(defstruct segment
  (start 0)
  (end 0)
  (direction nil))

(defun segments (problem)
  (bind ((c (complex 0 0)))
    (iter
      (for (dir-label amount) in problem)
      (for dir = (direction-to-complex dir-label))
      (for end = (+ c (* dir amount)))
      (collecting (make-segment :start c :end end :direction dir-label))
      (setf c end))))

(defun draw-grid-2 (grid)
  (bind (((min-x min-y max-x max-y) (grid-bounds grid)))
    (iter
        (for y from min-y to max-y)
        (iter
          (for x from min-x to max-x)
          (if (or ;; (eq (gethash (cons x y) grid) 'in)
                  (gethash (complex x y) grid)
                  )
              (format t "#")
              (format t ".")))
      (format t "~%"))))

(defun part-2 ()
  (bind ((problem (read-problem-2))
         (segments (segments problem)))
    (+ (floor
        (abs (iter
               (for segment in segments)
               (with-slots (start end) segment
                 (for prev-x = (realpart start))
                 (for prev-y = (imagpart start))
                 (for next-x = (realpart end))
                 (for next-y = (imagpart end))
                 (summing (- (* prev-x next-y) (* next-x prev-y))))))
        2)
       (floor (iter
                (for (dir amount) in problem)
                (summing amount))
              2)
       1)))
