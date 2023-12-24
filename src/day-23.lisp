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
      (setf queue (less-first queue))
      (when (/= dist (gethash coord seen))
        (next-iteration))
      (when (= coord end)
        (format t "(cons coord dist): ~a~%" (cons coord dist))
        (print-path from start coord dist))
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
          (collecting (cons new-coord delta)))))))

(defun is-not-in-path-back-2 (from start c dist direction test-c)
  (and (/= start test-c)
       (iter
         (while (/= start c))
         (never (= c test-c))
         (for (next-coord next-dist next-direction) = (gethash (list c dist direction) from))
         (setf c         next-coord
               dist      next-dist
               direction next-direction))))

(defun longest-paths-2 (grid)
  (bind ((start (starting-point grid))
         (end (end-point grid))
         (queue (seq (list start 0 up)))
         (seen (make-hash-table :test #'equal))
         (from (make-hash-table :test #'equal))
         (longest most-negative-fixnum))
    (setf (gethash (cons start up) seen) 0)
    (setf (gethash (cons start 0) from) (cons start 0))
    (iter
      (while (not (empty? queue)))
      (for (coord dist direction) = (first queue))
      (setf queue (less-first queue))
      ;; (format t "(list coord dist direction): ~a~%" (list coord dist direction))
      (when (/= dist (gethash (cons coord direction) seen))
        (next-iteration))
      (when (= coord end)
        (setf longest (max longest dist))
        (format t "(cons coord dist): ~a~%" (cons coord dist))
        ;; (print-path from start coord dist)
        )
      (iter
        (for new-coord-direction in (connecting-points-2 grid coord))
        (for (new-coord . new-direction) = new-coord-direction)
        (for new-dist = (1+ dist))
        (when (and (< (gethash new-coord-direction seen most-negative-fixnum) new-dist)
                   (is-not-in-path-back-2 from start coord dist direction new-coord))
          ;; (format t "new-coord-direction: ~a~%" new-coord-direction)
          (setf (gethash new-coord-direction seen) new-dist)
          (setf (gethash (list new-coord new-dist new-direction) from) (list coord dist direction))
          (setf queue (with-last queue (list new-coord new-dist new-direction))))))
    ))

(defun is-intersection (grid c)
  (> (iter
       (for (new-coord . _) in (connecting-points-2 grid c))
       (for tile = (grid-at grid new-coord))
       (counting (or (char-equal tile #\.)
                     (member tile (list #\> #\^ #\v #\<)))))
     2))

(defun intersections-reachable (grid start-c end)
  (bind ((seen (make-hash-table :test #'equal))
         (queue (seq start-c)))
    (setf (gethash start-c seen) 0)
    (iter outer
      (while (not (empty? queue)))
      (for c = (first queue))
      (setf queue (less-first queue))
      (iter
        (for (new-c . _) in (connecting-points-2 grid c))
        (when (not (gethash new-c seen))
          (setf (gethash new-c seen) (1+ (gethash c seen)))
          (if (or (= new-c end)
                  (is-intersection grid new-c))
              (in outer (collecting (cons new-c (gethash new-c seen))))
              (setf queue (with-last queue new-c))))))))

(defun compress-grid (grid)
  (bind ((graph (make-hash-table :test #'equal))
         (start (starting-point grid))
         (end (end-point grid)))
    (iter
      (for y from 0 below (y-dim grid))
      (iter
        (for x from 0 below (x-dim grid))
        (for c = (coord x y))
        (when (or (= start c) (= end c) (is-intersection grid c))
          (mapc (lambda (intersection)
                  (push intersection (gethash c graph)))
                (intersections-reachable grid c end)))))
    graph))

(defun is-not-in-path-back-graph (from start c dist test-c)
  (and (/= start test-c)
       (iter
         (while (/= start c))
         (never (= c test-c))
         (for (next-coord . next-dist) = (gethash (cons c dist) from))
         (setf c    next-coord
               dist next-dist))))

(defun longest-path-on-graph (graph start end)
  (bind ((seen (make-hash-table :test #'equal))
         (from (make-hash-table :test #'equal))
         (queue (seq (cons start 0))))
    (setf (gethash start seen) 0)
    ;; Dijkstra
    (iter
      (while (not (empty? queue)))
      (for (vertex . path-length) = (first queue))
      (format t "(cons vertex path-length): ~a~%" (cons vertex path-length))
      (setf queue (less-first queue))
      (when (/= path-length (gethash vertex seen))
        (next-iteration))
      (iter
        (for (next-vertex . next-distance) in (gethash vertex graph))
        (for next-path-length = (+ path-length next-distance))
        (when (and (> next-path-length (gethash next-vertex seen most-negative-fixnum))
                   (is-not-in-path-back-graph from start vertex path-length next-vertex))
          (setf (gethash (cons next-vertex next-path-length) from) (cons vertex path-length))
          (setf (gethash next-vertex seen) next-path-length)
          (setf queue (with-last queue (cons next-vertex next-path-length))))))

    ;; Bellman-Ford
    ;; (iter
    ;;   (for (i-v i-d) in-hashtable graph)
    ;;   (iter
    ;;     (for (c edges) in-hashtable graph)
    ;;     (format t "c: ~a~%" c)
    ;;     (when (= c end)
    ;;       (format t "~a~%"(gethash c seen most-negative-fixnum)))
    ;;     (iter
    ;;       (for (next-vertex . next-distance) in edges)
    ;;       (for next-path-length = (+ next-distance (gethash c seen most-negative-fixnum)))
    ;;       (when (> next-path-length (gethash next-vertex seen most-negative-fixnum))
    ;;         (setf (gethash next-vertex seen) next-path-length)))))
    (gethash end seen)))

(defun longest-of-all (graph start end)
  (labels ((recur (c dist seen)
             (if (= end c)
                 (print dist)
                 (iter
                   (for (new-coord . new-distance) in (gethash c graph))
                   (when (not (contains? seen new-coord))
                     (maximize (recur new-coord (+ dist new-distance) (with seen new-coord))
                               :into longest))
                   (finally (return (or longest most-negative-fixnum)))))))
    (recur start 0 (set start))))

(defun part-2 (&optional (file-relative-path "src/day-23.in"))
  (bind ((grid (read-problem file-relative-path))
         (graph (compress-grid grid))
         (start (starting-point grid))
         (end (end-point grid)))
    (longest-of-all graph start end)
    ;; (longest-paths-2 grid)
    ;; (longest-of-all grid)
    ))

;; Wrong: 5018 (too low)
;; Also too low: 5019

(defun test-2 ()
  (part-2 "src/day-23-test.in"))
