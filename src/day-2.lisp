(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-2
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-2)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-2.in"))
    (labels ((valid-game (line)
               (bind (((game draws) (split ": " line))
                      (rounds (split "; " draws))
                      (counts-per-round (mapcar (lambda (round) (mapcar (lambda (draw) (split " " draw))
                                                                        (split ", " round))) rounds)))
                 (if (not (some (lambda (count-per-round)
                                  (iter
                                    (for (num colour) in count-per-round)
                                    (thereis (cond
                                               ((string-equal colour "red")   (> (read-from-string num) 12))
                                               ((string-equal colour "green") (> (read-from-string num) 13))
                                               ((string-equal colour "blue")  (> (read-from-string num) 14))))))
                                (print counts-per-round)))
                     (->> (split " " game)
                       cadr
                       read-from-string)
                     0)))
             (recur (acc)
               (let* ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (+ acc (valid-game line)))))))
      (recur 0))))

;; wrong: 46

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-2.in"))
    (labels ((game-power (line)
               (bind (((game draws) (split ": " line))
                      (rounds (split "; " draws))
                      (counts-per-round (mapcar (lambda (round) (mapcar (lambda (draw) (split " " draw))
                                                                        (split ", " round))) rounds)))
                 (apply #'*
                        (reduce (lambda (acc count-per-round)
                                  (bind (((r g b) acc))
                                   (iter
                                     (for (num colour) in count-per-round)
                                     (for parsed = (read-from-string num))
                                     (cond
                                       ((string-equal colour "red")   (setf r (max parsed r)))
                                       ((string-equal colour "green") (setf g (max parsed g)))
                                       ((string-equal colour "blue")  (setf b (max parsed b)))))
                                    (list r g b)))
                                counts-per-round
                                :initial-value (list 0 0 0)))))
             (recur (acc)
               (let* ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (+ acc (game-power line)))))))
      (recur 0))))
