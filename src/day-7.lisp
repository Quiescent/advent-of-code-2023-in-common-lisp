(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-7
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-7)

(defun hand-type (cards)
  (bind ((content (convert 'bag cards))
         (acc 'high-card))
    (do-bag-pairs (key value content)
      (declare (ignore key))
      (setq acc (cond
                  ((contains? (set 'five-of-a-kind
                                   'four-of-a-kind
                                   'full-house
                                   'two-pair)
                              acc)
                   acc)
                  ((= 5 value) 'five-of-a-kind)
                  ((= 4 value) 'four-of-a-kind)
                  ((= 3 value) (if (eq acc 'two-of-a-kind)
                                   'full-house
                                   'three-of-a-kind))
                  ((= 2 value) (if (eq acc 'three-of-a-kind)
                                   'full-house
                                   (if (eq acc 'two-of-a-kind)
                                       'two-pair
                                       'two-of-a-kind)))
                  (t acc))))
    acc))

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-7.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (bind (((hand score) (split " " line)))
                       (recur (cons (list hand
                                          (hand-type hand)
                                          (read-from-string score))
                                    acc)))))))
      (recur nil))))

(defun card-to-number (card)
  (or (case card
        (#\A 14)
        (#\K 13)
        (#\Q 12)
        (#\J 11)
        (#\T 10))
      (digit-char-p card)))

(defun cards-less-p (cards-1 cards-2)
  (iter outer
    (for i from 0 below (length cards-1))
    (for card-1 = (card-to-number (aref cards-1 i)))
    (for card-2 = (card-to-number (aref cards-2 i)))
    (when (< card-1 card-2)
      (return-from outer t))
    (when (> card-1 card-2)
      (return-from outer nil))
    (finally (return-from outer nil))))

(defun type-to-number (type)
  (case type
    (five-of-a-kind 7)
    (four-of-a-kind 6)
    (full-house 5)
    (three-of-a-kind 4)
    (two-pair 3)
    (two-of-a-kind 2)
    (high-card 1)))

(defun hand-less-p (hand-1 hand-2)
  (bind (((cards-1 type-1 score-1) hand-1)
         ((cards-2 type-2 score-2) hand-2))
    (declare (ignore score-1 score-2))
    (cond
      ((eq type-1 type-2)
       (cards-less-p cards-1 cards-2))
      (t (< (type-to-number type-1)
            (type-to-number type-2))))))

(defun part-1 ()
  (bind ((problem (read-problem))
         (sorted (sort problem #'hand-less-p)))
    (iter
      (for i from 1)
      (for card-type-score in sorted)
      (for score = (caddr card-type-score))
      (summing (* i score)))))

(defun hand-type-2 (cards)
  (bind ((content (convert 'bag cards))
         (acc 'high-card))
    (iter
      (for card in-string "AKQT98765432J")
      (setf acc 'high-card)
      (setf content (convert 'bag (cl:map 'string
                                          (lambda (c)
                                            (if (char-equal c #\J)
                                                card
                                                c))
                                          cards)))
      (for type = (do-bag-pairs (key value content acc)
                    (declare (ignore key))
                    (setq acc (cond
                                ((contains? (set 'five-of-a-kind
                                                 'four-of-a-kind
                                                 'full-house
                                                 'two-pair)
                                            acc)
                                 acc)
                                ((= 5 value) 'five-of-a-kind)
                                ((= 4 value) 'four-of-a-kind)
                                ((= 3 value) (if (eq acc 'two-of-a-kind)
                                                 'full-house
                                                 'three-of-a-kind))
                                ((= 2 value) (if (eq acc 'three-of-a-kind)
                                                 'full-house
                                                 (if (eq acc 'two-of-a-kind)
                                                     'two-pair
                                                     'two-of-a-kind)))
                                (t acc)))))
      (finding type maximizing (type-to-number type)))))

(defun read-problem-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-7.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (bind (((hand score) (split " " line)))
                       (recur (cons (list hand
                                          (hand-type-2 hand)
                                          (read-from-string score))
                                    acc)))))))
      (recur nil))))

(defun card-to-number-2 (card)
  (or (case card
        (#\A 14)
        (#\K 13)
        (#\Q 12)
        (#\J 1)
        (#\T 10))
      (digit-char-p card)))

(defun cards-less-p-2 (cards-1 cards-2)
  (iter outer
    (for i from 0 below (length cards-1))
    (for card-1 = (card-to-number-2 (aref cards-1 i)))
    (for card-2 = (card-to-number-2 (aref cards-2 i)))
    (when (< card-1 card-2)
      (return-from outer t))
    (when (> card-1 card-2)
      (return-from outer nil))
    (finally (return-from outer nil))))

(defun hand-less-p-2 (hand-1 hand-2)
  (bind (((cards-1 type-1 score-1) hand-1)
         ((cards-2 type-2 score-2) hand-2))
    (declare (ignore score-1 score-2))
    (cond
      ((eq type-1 type-2)
       (cards-less-p-2 cards-1 cards-2))
      (t (< (type-to-number type-1)
            (type-to-number type-2))))))

(defun part-2 ()
  (bind ((problem (read-problem-2))
         (sorted (sort problem #'hand-less-p-2)))
    (iter
      (for i from 1)
      (for card-type-score in sorted)
      (for score = (caddr card-type-score))
      (summing (* i score)))))
