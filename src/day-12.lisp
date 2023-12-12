(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-12
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-12)

(defun parse-line (line)
  (bind (((map damaged) (split " " line))
         (numbers (->> (split "," damaged)
                    (mapcar #'read-from-string))))
    (cons map numbers)))

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-12.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (cons (parse-line line) acc))))))
      (recur nil))))

(defun length-of-next-run (map start)
  (iter
    (for i from start below (length map))
    (for char = (aref map i))
    (while (char-equal char #\#))
    (counting t into l)
    (finally (return (cons l i)))))

(defun available-length (map start)
  (+ (iter
       (for i from start below (length map))
       (for char = (aref map i))
       (while (or (char-equal char #\?)
                  (char-equal char #\#)))
       (counting t))
     (if (and (< start (length map))
              (char-equal (aref map start) #\#))
         -1
         0)))

(defun can-place-here (map number i)
  (and (<= (+ i number) (length map))
       (or (>= (+ i number) (1- (length map)))
           (not (char-equal (aref map (+ i number)) #\#)))
       (iter
         (for j from i below (+ i number))
         (for char = (aref map j))
         (always (or (char-equal char #\#)
                     (char-equal char #\?))))))

(defun count-arrangements-2 (map numbers i)
  ;; (format t "(list numbers i): ~a~%" (list numbers i))
  (cond
    ((null numbers) 1)
    ((>= i (length map)) 0)
    ((char-equal (aref map i) #\.) (count-arrangements-2 map numbers (1+ i)))
    (t
     (+ (if (can-place-here map (car numbers) i)
            (count-arrangements-2 map
                                  (cdr numbers)
                                  (+ 1 i (car numbers)))
            0)
        (if (char-equal (aref map i) #\?)
            (count-arrangements-2 map
                                  numbers
                                  (1+ i))
            0)))))

(defun count-arrangements (map numbers i)
  ;; (format t "(list numbers i): ~a~%" (list numbers i))
  (cond
    ((null numbers) 1)
    ((>= i (length map)) 0)
    ((char-equal (aref map i) #\.) (count-arrangements map numbers (1+ i)))
    (t (bind (((next . rest) numbers)
              ((len . end) (length-of-next-run map i)))
         ;; (format t "(list len end): ~a~%" (list len end))
         (cond
           ((> next (- (length map) i)) 0)
           ((= len next)
            (count-arrangements map rest (1+ end)))
           (t (bind ((available (available-length map i)))
                ;; (format t "available: ~a~%" available)
                (iter
                  (for spot from i below (+ end available))
                  (for remaining downfrom available)
                  ;; (format t "(list spot remaining): ~a~%" (list spot remaining))
                  (summing (if (and (>= (+ len remaining) next)
                                    (or (>= (+ spot next) (length map))
                                        (not (char-equal #\# (aref map (+ spot next))))))
                               (count-arrangements map rest (1+ (+ spot next)))
                               0))))))))) )

(defun part-1 ()
  (bind ((problem (read-problem))
         (seen (make-hash-table)))
    (iter
      (for (map . numbers) in problem)
      (summing (count-arrangements-2 map numbers 0)))))

;; Wrong: 8337
;; Wrong: 8345

(defun part-2 ()
  (bind ((problem (read-problem)))
    ))
