(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-6
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-6)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-6.in"))
    (labels ((recur ()
               (bind ((line-1 (read-line f nil nil))
                      (line-2 (read-line f nil nil)))
                 (list (car (->> (split " +" line-1)
                              cdr
                              (format nil "(~a)")
                              read-from-string))
                       (car (->> (split " +" line-2)
                              cdr
                              (format nil "(~a)")
                              read-from-string))))))
      (recur))))

(defun ways-to-win (time distance)
  (iter
    (for i from 0 below time)
    (for my-distance = (* (- time i) i))
    (counting (> my-distance distance))))

(defun part-1 ()
  (bind (((times distances) (read-problem)))
    (apply #'* (cl:map 'list #'ways-to-win times distances))))

(defun read-problem-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-6.in"))
    (labels ((recur ()
               (bind ((line-1 (read-line f nil nil))
                      (line-2 (read-line f nil nil)))
                 (list (->> (split " +" line-1)
                         cdr
                         (apply #'concatenate 'string)
                         read-from-string)
                       (->> (split " +" line-2)
                         cdr
                         (apply #'concatenate 'string)
                         read-from-string)))))
      (recur))))

(defun part-2 ()
  (bind (((times distances) (read-problem-2)))
    (ways-to-win times distances)))
