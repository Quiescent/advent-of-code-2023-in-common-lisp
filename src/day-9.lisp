(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-9
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-9)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-9.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (cons (-> (format nil "(~a)" line) read-from-string)
                                  acc))))))
      (recur nil))))

(defun diffs (xs)
  (when (and xs (cdr xs))
    (cons (- (cadr xs)
             (car xs))
          (diffs (cdr xs)))))

(defun recursive-diffs (xs)
  (bind ((next-diffs (diffs xs)))
    (when (not (every (lambda (x) (= x 0)) next-diffs))
      (cons next-diffs
            (recursive-diffs next-diffs)))))

(defun next-number (xs)
  (bind ((ds (nreverse (recursive-diffs xs))))
    (labels ((recur (ys prev-last)
               (if (null ys)
                   prev-last
                   (bind ((next-last (last (car ys))))
                     (recur (cdr ys)
                            (+ prev-last
                               next-last))))))
      (+ (recur (cdr ds)
                (last (car ds)))
         (last xs)))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (apply #'+ (mapcar #'next-number problem))))

(defun prev-number (xs)
  (bind ((ds (nreverse (recursive-diffs xs))))
    (labels ((recur (ys prev-last)
               (if (null ys)
                   prev-last
                   (bind ((next-last (car (car ys))))
                     (recur (cdr ys)
                            (- next-last
                               prev-last))))))
      (- (car xs)
         (recur (cdr ds)
                (car (car ds)))))))

(defun part-2 ()
  (bind ((problem (read-problem)))
    (apply #'+ (mapcar #'prev-number problem))))
