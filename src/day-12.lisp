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
                     (nreverse acc)
                     (recur (cons (parse-line line) acc))))))
      (recur nil))))

(defun can-place-here (map number i)
  (and (<= (+ i number) (length map))
       (or (> (+ i number) (1- (length map)))
           (not (char-equal (aref map (+ i number)) #\#)))
       (iter
         (for j from i below (+ i number))
         (for char = (aref map j))
         (always (or (char-equal char #\#)
                     (char-equal char #\?))))))

(defvar seen nil)

(defun count-arrangements-2 (map numbers i)
  (or #1=(gethash (cons i numbers) seen)
      (setf #1# (cond
                  ((null numbers) (if (and (< i (length map))
                                           (find #\# map :start i))
                                      0
                                      1))
                  ((>= i (length map)) 0)
                  ((char-equal (aref map i) #\.) (count-arrangements-2 map numbers (1+ i)))
                  (t
                   (+ (if (can-place-here map (car numbers) i)
                          (progn
                            (count-arrangements-2 map
                                                  (cdr numbers)
                                                  (+ 1 i (car numbers))))
                          0)
                      (if (char-equal (aref map i) #\?)
                          (count-arrangements-2 map
                                                numbers
                                                (1+ i))
                          0)))))))

;; To see the arrangements...
(defvar arrangements nil)

(defun count-arrangements-2-list (map numbers i acc)
  (cond
    ((null numbers) (if (and (< i (length map))
                             (find #\# map :start i))
                        0
                        (progn (push (reverse acc) arrangements) 1)))
    ((>= i (length map)) 0)
    ((char-equal (aref map i) #\.) (count-arrangements-2-list map numbers (1+ i) acc))
    (t
     (+ (if (can-place-here map (car numbers) i)
            (progn
              (count-arrangements-2-list map
                                         (cdr numbers)
                                         (+ 1 i (car numbers))
                                         (cons i acc)))
            0)
        (if (char-equal (aref map i) #\?)
            (count-arrangements-2-list map
                                       numbers
                                       (1+ i)
                                       acc)
            0)))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (iter
      (for (map . numbers) in problem)
      (summing (bind ((seen (make-hash-table :test #'equal)))
                 (count-arrangements-2 map numbers 0))))))

(defun duplicate (input)
  (bind (((map . numbers) input))
    (iter
      (for i from 0 below 5)
      (collecting map into maps)
      (when (/= i 4)
        (collecting "?" into maps))
      (collecting numbers into numberss)
      (finally
       (return (cons (apply #'concatenate 'string maps)
                     (apply #'concatenate 'list numberss)))))))

(defun part-2 ()
  (bind ((problem (mapcar #'duplicate (read-problem))))
    (iter
      (for (map . numbers) in problem)
      (summing (bind ((seen (make-hash-table :test #'equal)))
                 (count-arrangements-2 map numbers 0))))))
