(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-4
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-4)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-4.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (bind (((raw-winning raw-mine) (split " \\| " line))
                            ((card-id raw-winning-nums) (split ": " raw-winning))
                            (winning-nums (convert 'set (->> (format nil "(~a)" raw-winning-nums) read-from-string)))
                            (my-nums (convert 'seq (->> (format nil "(~a)" raw-mine) read-from-string)))
                            (matching-size (size (filter (lambda (my-num) (contains? winning-nums my-num)) my-nums))))
                       (if (> matching-size 0)
                           (recur (+ acc (expt 2 (1- matching-size))))
                           (recur acc)))))))
      (recur 0))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-4.in"))
    (labels ((table ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (bind (((raw-winning raw-mine) (split " \\| " line))
                          ((card-id raw-winning-nums) (split ": " raw-winning))
                          (winning-nums (convert 'set (->> (format nil "(~a)" raw-winning-nums) read-from-string)))
                          (my-nums (convert 'seq (->> (format nil "(~a)" raw-mine) read-from-string)))
                          (matching-size (size (filter (lambda (my-num) (contains? winning-nums my-num)) my-nums))))
                     (cons (cons card-id matching-size) (table))))))
             (process-single (id repeats remaining costs)
               (labels ((recur (i acc xs sub-costs)
                          (if (= i 0)
                              (list acc (with sub-costs id acc))
                              (bind (((sub-id . score) (car xs)))
                                (if (not (null (@ sub-costs id)))
                                    (list (@ sub-costs id) sub-costs)
                                    (bind (((sub-score new-costs) (process-single sub-id
                                                                                  score
                                                                                  (cdr xs)
                                                                                  sub-costs)))
                                      (recur (1- i)
                                             (+ acc sub-score)
                                             (cdr xs)
                                             new-costs)))))))
                 (recur repeats 1 remaining costs)))
             (process-table (acc remaining costs)
               (if (null remaining)
                   acc
                   (bind (((id . current-score) (car remaining))
                          ((sub-score new-costs) (process-single id
                                                                 current-score
                                                                 (cdr remaining)
                                                                 costs)))
                     (process-table (+ acc sub-score)
                                    (cdr remaining)
                                    new-costs)))))
      (process-table 0 (table) (empty-map nil)))))
