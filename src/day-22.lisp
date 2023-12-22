(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-22
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-22)

(defun read-problem (relative-path-name)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp relative-path-name))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     ))))
      (recur nil))))

(defun part-1 (&optional (relative-path-name "src/day-22.in"))
  (bind ((problem (read-problem relative-path-name)))
    ))

(defun test-1 ()
  (part-1 "src/day-22-test.in"))

(defun part-2 (&optional (relative-path-name "src/day-22.in"))
  (bind ((problem (read-problem relative-path-name)))
    ))

(defun test-2 ()
  (part-2 "src/day-22-test.in"))
