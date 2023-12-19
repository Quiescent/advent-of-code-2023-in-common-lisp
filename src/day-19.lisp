(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-19
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-19)

(defun read-problem (&optional (file-relative-path "src/day-19.in"))
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp file-relative-path))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     ))))
      (recur nil))))

(defun part-1 (&optional (file-relative-path "src/day-19.in"))
  (bind ((problem (read-problem file-relative-path)))
    ))

(defun test-1 ()
  (part-1 "src/day-19-test.in"))

(defun part-2 (&optional (file-relative-path "src/day-19.in"))
  (bind ((problem (read-problem file-relative-path)))
    ))

(defun test-2 ()
  (part-2 "src/day-19-test.in"))
