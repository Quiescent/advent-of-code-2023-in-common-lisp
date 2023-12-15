(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-15
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-15)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-15.in"))
    (bind ((line (read-line f nil nil)))
      (split "," line))))

(defun hash (step)
  (bind ((result 0))
    (iter
      (for char in-string step)
      (incf result (char-code char))
      (setf result (* result 17))
      (setf result (mod result 256))
      (finally (return result)))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (iter
      (for step in problem)
      (summing (hash step)))))

(defun part-2 ()
  (bind ((problem (read-problem))
         (boxes (make-hash-table :test #'equal)))
    (iter
      (for instruction in problem)
      (match instruction
        ((ppcre "([a-zA-Z]+)=([0-9]+)"
                label
                (read lense))
         (bind ((box-idx (hash label))
                (lenses (gethash box-idx boxes)))
           (if (member label lenses :key #'car :test #'equal)
               (setf (gethash box-idx boxes)
                     (mapcar (lambda (label-lense)
                               (if (string-equal (car label-lense) label)
                                   (cons label lense)
                                   label-lense))
                             lenses))
               (setf (gethash box-idx boxes)
                     (nconc lenses (list (cons label lense)))))))
        ((ppcre "([a-zA-Z]+)-"
                label)
         (bind ((box-idx (hash label))
                (lenses (gethash box-idx boxes)))
           (setf (gethash box-idx boxes)
                 (remove label lenses :key #'car :test #'equal))))))
    (iter
      (for (box-idx lenses) in-hashtable boxes)
      (summing
       (iter
         (for i from 1)
         (for (label . lense) in lenses)
         (summing (* (1+ box-idx) i lense)))))))

;; Purely functional solution for fun.

(defun power-in-box (initial-lenses box-idx)
  (labels ((recur (acc lenses i)
             (if (empty? lenses)
                 acc
                 (recur (+ acc (* (1+ box-idx)
                                  i
                                  (cdr (first lenses))))
                        (less-first lenses)
                        (1+ i)))))
    (recur 0 initial-lenses 1)))

(defun compute-power (boxes)
  (reduce (lambda (acc key box)
            (+ (power-in-box box key)
               acc))
          boxes
          :initial-value 0))

(defun part-2-pf ()
  (bind ((problem (read-problem)))
    (labels ((recur (boxes instructions)
               (if (null instructions)
                   (compute-power boxes)
                   (bind (((instruction . rest) instructions)
                          (label-op (match instruction
                                      ((ppcre "([a-z]+)=([0-9]+)"
                                              label
                                              (read lense))
                                       (cons label lense))
                                      ((ppcre "([a-z]+)-"
                                              label)
                                       (cons label '-))))
                          ((label . op) label-op)
                          (box-idx (hash label))
                          (lenses (@ boxes box-idx))
                          (existing-lense-position (position label lenses :key #'car :test #'equal)))
                     (recur (with boxes
                                  box-idx
                                  (cond
                                    ((eq op '-) (remove label lenses :key #'car :test #'equal))
                                    (existing-lense-position
                                     (with lenses existing-lense-position label-op))
                                    (t (with-last lenses label-op))))
                            rest)))))
      (recur (empty-map (empty-seq)) problem))))
