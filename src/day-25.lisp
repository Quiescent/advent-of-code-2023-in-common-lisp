(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-25
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-25)

(defun parse-line (line)
  (bind (((first connections) (split ": " line)))
    (cons (read-from-string first)
          (->>  (split " " connections)
            (mapcar #'read-from-string)))))

(defun read-problem (file-relative-path)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp file-relative-path))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (cons (parse-line line)
                                  acc))))))
      (recur nil))))

(defun components (graph)
  (bind ((seen (make-hash-table))
         (current-component (list))
         (components (list)))
    (labels ((recur (node)
               (iter
                 (for other in (gethash node graph))
                 (when (null (gethash other seen))
                   (setf (gethash other seen) t)
                   (push other current-component)
                   (recur other)))))
      (iter
        (for (key value) in-hashtable graph)
        (when (null (gethash key seen))
          (setf (gethash key seen) t)
          (setf current-component (list key))
          (recur key)
          (collecting current-component))))))

(defun copy-hashtable (table)
  (bind ((new-table (make-hash-table)))
    (iter
      (for (key value) in-hashtable table)
      (setf (gethash key new-table) value)
      (finally (return new-table)))))

(defun shuffle (xs)
  (iter
    (for i from (1- (length xs)) downto 1)
    (for j = (random (1+ i)))
    (rotatef (car (nthcdr i xs))
             (car (nthcdr j xs)))
    (finally (return xs))))

(defun dfs-until-three (graph &optional (run-time 1000000))
  (bind ((seen-count (make-hash-table :test #'equal))
         (seen (make-hash-table))
         (nodes (-> (iter
                      (for (key value) in-hashtable graph)
                      (collecting key))
                  (coerce 'vector))))
    (labels ((recur (node)
               (iter
                 (for other in (shuffle (gethash node graph)))
                 (when (null (gethash other seen))
                   (setf (gethash other seen) t)
                   (for edge = (sort (list node other)
                                     #'string-lessp
                                     :key #'symbol-name))
                   (incf (gethash edge seen-count 0))
                   (recur other)))))
      (iter
        (for i from 0 below run-time)
        (for start = (aref nodes (random (length nodes))))
        (setf seen (make-hash-table))
        (recur start))
      (-> (iter
             (for (key value) in-hashtable seen-count)
             (collecting (cons value key)))
        (sort #'> :key #'car)
        (subseq 0 3)))))

(defun remove-one-by-one-random (orig-graph)
  (labels ((recur (graph disconnected)
             (cond
               ((> (length disconnected) 3) nil)
               ((and (= (length disconnected) 3)
                     (> (length (components graph)) 1))
                (->> (components graph)
                  (mapcar #'length)
                  (apply #'*)))
               (t (bind ((candidates (->> (dfs-until-three graph 100)
                                       (mapcar #'cdr))))
                    (iter
                      (for candidate in candidates)
                      (for (start end) = candidate)
                      (for current-graph = (copy-hashtable graph))
                      (setf (gethash start current-graph)
                            (remove end (gethash start current-graph)))
                      (setf (gethash end current-graph)
                            (remove start (gethash end current-graph)))
                      (for result = (recur current-graph (cons candidate disconnected)))
                      (finding result such-that result)))))))
    (recur orig-graph (list))))

(defun to-graph (edges)
  (bind ((graph (make-hash-table)))
    (iter
      (for (start . ends) in edges)
      (iter
        (for end in ends)
        (push end (gethash start graph))
        (push start (gethash end graph))))
    graph))

(defun part-1 (&optional (file-relative-path "src/day-25.in"))
  (bind ((edge-list (read-problem file-relative-path))
         (graph (to-graph edge-list)))
    (remove-one-by-one-random graph)))

;; Too low: 1435

(defun test-1 ()
  (part-1 "src/day-25-test.in"))
