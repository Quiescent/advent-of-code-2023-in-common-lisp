(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-3
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-3)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-3.in"))
    (labels ((read-map ()
               (let ((line (read-line f nil nil)))
                 (if (null line)
                     nil
                     (cons line
                           (read-map)))))
             (neighbours (engine-map i-in j-in seen)
               (let ((x 0)
                     (y 0)
                     (max-x 0)
                     (part-numbers (empty-seq)))
                 (iter
                   (for i from (max 0 (1- i-in)) to (min (1+ i-in) (1- (length (aref engine-map 0)))))
                   (iter
                     (for j from (max 0 (1- j-in)) to (min (1+ j-in) (length engine-map)))
                     (when (and (digit-char-p (-> (aref engine-map j) (aref i)))
                                (not (contains? seen (cons i j))))
                       (setf max-x 0)
                       (iter
                         (initially (setf max-x i)
                                    (setf x i)
                                    (setf y j))
                         (while (and (< x (length (aref engine-map 0)))
                                     (digit-char-p (-> (aref engine-map y) (aref x)))))
                         (adjoinf seen (cons x y))
                         (incf x)
                         (incf max-x))
                       (iter
                         (initially (setf x i)
                                    (setf y j))
                         (while (and (>= x 0)
                                     (digit-char-p (-> (aref engine-map y) (aref x)))))
                         (adjoinf seen (cons x y))
                         (decf x))
                       (setf  part-numbers
                              (with-last part-numbers (read-from-string (subseq (aref engine-map y) (1+ x) max-x)))))))
                 (list seen part-numbers)))
             (recur (i j engine-map parts part-numbers)
               (cond
                 ((>= i (length (aref engine-map 0))) (recur 0 (1+ j) engine-map parts part-numbers))
                 ((>= j (length engine-map)) (reduce #'+ part-numbers))
                 (t (bind ((point (-> (aref engine-map j) (aref i)))
                           ((new-parts new-part-numbers) (if (and (not (digit-char-p point))
                                                                          (not (char-equal point #\.)))
                                                                     (neighbours engine-map i j parts)
                                                                     (list parts (empty-seq)))))
                      (recur (1+ i)
                             j
                             engine-map
                             new-parts
                             (seq ($ part-numbers) ($ new-part-numbers))))))))
      (let ((engine-map (cl:map 'vector #'identity (read-map))))
        (recur 0 0 engine-map (empty-set) (empty-seq))))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-3.in"))
    (labels ((read-map ()
               (let ((line (read-line f nil nil)))
                 (if (null line)
                     nil
                     (cons line
                           (read-map)))))
             (neighbours (engine-map i-in j-in seen)
               (let ((x 0)
                     (y 0)
                     (max-x 0)
                     (part-numbers (empty-seq)))
                 (iter
                   (for i from (max 0 (1- i-in)) to (min (1+ i-in) (1- (length (aref engine-map 0)))))
                   (iter
                     (for j from (max 0 (1- j-in)) to (min (1+ j-in) (length engine-map)))
                     (when (and (digit-char-p (-> (aref engine-map j) (aref i)))
                                (not (contains? seen (cons i j))))
                       (setf max-x 0)
                       (iter
                         (initially (setf max-x i)
                                    (setf x i)
                                    (setf y j))
                         (while (and (< x (length (aref engine-map 0)))
                                     (digit-char-p (-> (aref engine-map y) (aref x)))))
                         (adjoinf seen (cons x y))
                         (incf x)
                         (incf max-x))
                       (iter
                         (initially (setf x i)
                                    (setf y j))
                         (while (and (>= x 0)
                                     (digit-char-p (-> (aref engine-map y) (aref x)))))
                         (adjoinf seen (cons x y))
                         (decf x))
                       (setf  part-numbers
                              (with-last part-numbers (read-from-string (subseq (aref engine-map y) (1+ x) max-x)))))))
                 (list seen part-numbers)))
             (recur (i j engine-map parts acc)
               (cond
                 ((>= i (length (aref engine-map 0))) (recur 0 (1+ j) engine-map parts acc))
                 ((>= j (length engine-map)) acc)
                 (t (bind ((point (-> (aref engine-map j) (aref i)))
                           ((new-parts new-part-numbers) (if (char-equal point #\*)
                                                             (neighbours engine-map i j parts)
                                                             (list parts (empty-seq)))))
                      (recur (1+ i)
                             j
                             engine-map
                             new-parts
                             (+ acc (if (= (size new-part-numbers) 2)
                                        (reduce #'* new-part-numbers)
                                        0))))))))
      (let ((engine-map (cl:map 'vector #'identity (read-map))))
        (recur 0 0 engine-map (empty-set) 0)))))
