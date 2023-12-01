(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-1
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-1)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-1.in"))
    (labels ((recur ()
               (let* ((line (read-line f nil nil)))
                 (if line
                     (let ((digits (->> (cl:map 'list #'digit-char-p line)
                                     (remove nil))))
                       (+ (read-from-string (format nil "~a~a" (car digits) (last digits)))
                          (recur)))
                     0))))
      (recur))))

(defvar search-terms (list "zero"
                           "one"
                           "two"
                           "three"
                           "four"
                           "five"
                           "six"
                           "seven"
                           "eight"
                           "nine"
                           "0"
                           "1"
                           "2"
                           "3"
                           "4"
                           "5"
                           "6"
                           "7"
                           "8"
                           "9"))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-1.in"))
    (labels ((convert-num (val)
               (or (and (= (length val) 1) (digit-char-p (aref val 0)))
                   (cond
                     ((string-equal val "zero") 0)
                     ((string-equal val "one") 1)
                     ((string-equal val "two") 2)
                     ((string-equal val "three") 3)
                     ((string-equal val "four") 4)
                     ((string-equal val "five") 5)
                     ((string-equal val "six") 6)
                     ((string-equal val "seven") 7)
                     ((string-equal val "eight") 8)
                     ((string-equal val "nine") 9)
                     (t (error "boom")))))
             (first-number (line)
               (labels ((recur-first (min-pos val remaining)
                          (if (null remaining)
                              (convert-num val)
                              (let ((pos (search (car remaining) line)))
                                (if (and pos (< pos min-pos))
                                    (recur-first pos (car remaining) (cdr remaining))
                                    (recur-first min-pos val (cdr remaining)))))))
                 (recur-first most-positive-fixnum nil search-terms)))
             (last-number (line)
               (labels ((recur-first (max-pos max-val remaining)
                          (if (null remaining)
                              (convert-num max-val)
                              (let ((pos (search (car remaining) line :from-end t)))
                                (if (and pos (> pos max-pos))
                                    (recur-first pos (car remaining) (cdr remaining))
                                    (recur-first max-pos max-val (cdr remaining)))))))
                 (recur-first most-negative-fixnum nil search-terms)))
             (recur ()
               (let* ((line (read-line f nil nil)))
                 (if (null line)
                     0
                     (+ (* 10 (first-number line))
                        (last-number line)
                        (recur))))))
      (recur))))

;; Done for fun and instructive purposes, later in the day. :)
(defun part-2-alt ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-1.in"))
    (let ((search-regex (create-scanner `(:alternation ,@search-terms))))
      (labels ((convert-num (val)
                 (or (and (= (length val) 1) (digit-char-p (aref val 0)))
                     (cond
                       ((string-equal val "zero") 0)
                       ((string-equal val "one") 1)
                       ((string-equal val "two") 2)
                       ((string-equal val "three") 3)
                       ((string-equal val "four") 4)
                       ((string-equal val "five") 5)
                       ((string-equal val "six") 6)
                       ((string-equal val "seven") 7)
                       ((string-equal val "eight") 8)
                       ((string-equal val "nine") 9)
                       (t (error "boom")))))
               (first-and-last (line)
                 (list
                  (iter
                    (for i from 0 below (length line))
                    (bind (((:values start end) (scan search-regex line :start i)))
                      (when (and start (= i start))
                        (return (subseq line start end)))))
                  (iter
                    (for i from (1- (length line)) downto 0)
                    (bind (((:values start end) (scan search-regex line :start i)))
                      (when (and start (= i start))
                        (return (subseq line start end)))))))
               (recur ()
                 (let* ((line (read-line f nil nil)))
                   (if (null line)
                       0
                       (bind ((matches (->> (first-and-last line)
                                         (mapcar #'convert-num))))
                         (+ (* 10 (car matches))
                            (or (last matches)
                                (car matches))
                            (recur)))))))
        (recur)))))
