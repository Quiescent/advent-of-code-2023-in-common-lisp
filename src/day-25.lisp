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

(defun remove-three (graph)
  (bind ((dfs-num (make-hash-table))
         (dfs-low (make-hash-table))
         (dfs-parent (make-hash-table))
         (cnt 0)
         (root-children 0)
         (bridges nil)
         (start (iter
                  (for (key value) in-hashtable graph)
                  (return key))))
    (labels ((recur (node)
               (progn
                 (setf (gethash node dfs-num) cnt)
                 (setf (gethash node dfs-low) cnt)
                 (incf cnt)
                 (iter
                   (for vertex in (gethash node graph))
                   (if (null (gethash vertex dfs-num))
                       (progn
                         (setf (gethash vertex dfs-parent) node)
                         (when (eq vertex start)
                           (incf root-children))
                         (recur vertex)
                         (when (> (gethash vertex dfs-low 0)
                                  (gethash node dfs-num 0))
                           (push (cons node vertex) bridges))
                         (setf (gethash node dfs-low) (min (gethash node dfs-low 0)
                                                           (gethash vertex dfs-low 0))))
                       (when (not (eq vertex (gethash node dfs-parent)))
                         (setf (gethash node dfs-low)
                               (min (gethash node dfs-low 0)
                                    (gethash vertex dfs-low 0)))))))))
      (iter
        (for (vertex edges) in-hashtable graph)
        (when (null (gethash vertex dfs-num))
          (setf start vertex)
          (recur start)))
      bridges)))

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

(defun find-bridges-brute-force (graph)
  (bind ((seen (empty-set)))
    (iter outer
      (for (vertex terminals) in-hashtable graph)
      (iter
        (for end in terminals)
        (when (contains? (set vertex end) seen)
          (next-iteration))
        (setf seen (with seen (set vertex end)))
        (for current-graph = (copy-hashtable graph))
        (setf (gethash vertex current-graph)
              (remove end (gethash vertex current-graph)))
        (setf (gethash end current-graph)
              (remove vertex (gethash end current-graph)))
        (for new-components = (components current-graph))
        (when (> (length new-components) 1)
          (in outer (collecting (cons vertex end))))))))

(defun dfs-from-all (graph)
  (bind ((seen-count (make-hash-table :test #'equal))
         (seen-sets (list))
         (current-set (empty-set))
         (seen (make-hash-table)))
    (labels ((recur (node)
               (iter
                 (for other in (gethash node graph))
                 (when (null (gethash other seen))
                   (setf (gethash other seen) t)
                   (for edge = (sort (list node other)
                                     #'string-lessp
                                     :key #'symbol-name))
                   (setf current-set (with current-set edge))
                   (incf (gethash edge seen-count 0))
                   (recur other)))))
      (iter
        (for (node edges) in-hashtable graph)
        (setf seen (make-hash-table))
        (setf current-set (empty-set))
        (recur node)
        (push current-set seen-sets))
      seen-sets)))

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
        (format t "i: ~a~%" i)
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
             ;; (format t "disconnected: ~a~%" disconnected)
             (cond
               ((> (length disconnected) 3) nil)
               ((and (= (length disconnected) 3)
                     (> (length (components graph)) 1))
                (->> (components graph)
                  (mapcar #'length)
                  (apply #'*)))
               (t (bind ((candidates (->> (dfs-until-three graph 1000)
                                       (mapcar #'cdr))))
                    (format t "candidates: ~a~%" candidates)
                    (iter
                      (for candidate in candidates)
                      (for (start end) = candidate)
                      (for current-graph = (copy-hashtable graph))
                      (setf (gethash start current-graph)
                            (remove end (gethash start current-graph)))
                      (setf (gethash end current-graph)
                            (remove start (gethash end current-graph)))
                      (for result = (recur current-graph (cons candidate disconnected)))
                      (finding result such-that result)
                      ;; (push end (gethash start current-graph))
                      ;; (setf start (gethash end current-graph))
                      ))))))
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

(defun remove-one-by-one (orig-graph)
  (labels ((recur (graph disconnected)
             ;; (format t "disconnected: ~a~%" disconnected)
             (cond
               ((> (length disconnected) 3) nil)
               ((and (= (length disconnected) 3)
                     (> (length (components graph)) 1))
                (->> (components graph)
                  (mapcar #'length)
                  (apply #'*)))
               (t (bind ((candidates (->> (dfs-from-all graph)
                                       (reduce #'intersection)
                                       (convert 'list))))
                    (format t "candidates: ~a~%" candidates)
                    (iter
                      (for candidate in candidates)
                      (for (start end) = candidate)
                      (for current-graph = (copy-hashtable graph))
                      (setf (gethash start current-graph)
                            (remove end (gethash start current-graph)))
                      (setf (gethash end current-graph)
                            (remove start (gethash end current-graph)))
                      (for result = (recur current-graph (cons candidate disconnected)))
                      (finding result such-that result)
                      ;; (push end (gethash start current-graph))
                      ;; (setf start (gethash end current-graph))
                      ))))))
    (recur orig-graph (list))))

(defun part-1 (&optional (file-relative-path "src/day-25.in"))
  (bind ((edge-list (read-problem file-relative-path))
         (graph (to-graph edge-list))
         ;; (edges-to-remove (->> (dfs-until-three graph)
         ;;                    (mapcar #'cdr)))
         )
    ;; (format t "edges-to-remove: ~a~%" edges-to-remove)
    ;; (iter
    ;;   (for (start end) in edges-to-remove)
    ;;   (setf (gethash start graph)
    ;;         (remove end (gethash start graph)))
    ;;   (setf (gethash end graph)
    ;;         (remove start (gethash end graph))))
    ;; (->> (components graph)
    ;;   (mapcar #'length)
    ;;   (apply #'*))
    (remove-one-by-one-random graph)))

;; Too low: 1435

(defun test-1 ()
  (part-1 "src/day-25-test.in"))
