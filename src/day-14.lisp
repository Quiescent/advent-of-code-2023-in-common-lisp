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
                     acc
                     ))))
      (recur nil))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    ))

(defun part-2 ()
  (bind ((problem (read-problem)))
    ))
