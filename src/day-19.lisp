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

(defstruct rule
  (label nil)
  (code nil))

(defstruct part
  (x 0 :type fixnum)
  (m 0 :type fixnum)
  (a 0 :type fixnum)
  (s 0 :type fixnum))

(defun parse-rule (rule-s)
  (match rule-s
    ((ppcre "([xmas])([><])([0-9]+):([a-zAR]+)"
            slot
            (read comparator)
            (read amount)
            (read dest))
     (bind ((accessor (read-from-string (format nil "part-~a" slot))))
       (lambda (part)
         (when (funcall comparator (funcall accessor part) amount)
           dest))))
    ((ppcre "([a-zAR]+)"
            (read dest))
     (lambda (part)
       (declare (ignore part))
       dest))))

(defun parse-code (line)
  (bind ((start-code (position #\{ line))
         (name (read-from-string (subseq line 0 start-code)))
         (code (subseq line (1+ start-code) (1- (length line))))
         (rules-s (split "," code))
         (rules (mapcar #'parse-rule rules-s)))
    (make-rule :label name
               :code rules)))

(defun parse-part (line)
  (bind (((x m a s) (->> (all-matches-as-strings "([0-9]+)" line)
                      (mapcar #'read-from-string))))
    (make-part
     :x x
     :m m
     :a a
     :s s)))

(defun rules-to-hash-map (rules)
  (bind ((table (make-hash-table)))
    (iter
      (for rule in rules)
      (with-slots (label) rule
        (setf (gethash label table) rule)))
    table))

(defun read-problem (&optional (file-relative-path "src/day-19.in"))
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp file-relative-path))
    (labels ((recur-parts (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur-parts (cons (parse-part line) acc)))))
             (recur-rules (acc)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((string-equal line "")
                    (cons (rules-to-hash-map acc) (recur-parts nil)))
                   (t (recur-rules (cons (parse-code line) acc)))))))
      (recur-rules nil))))

(defun run-rules (part rules)
  (bind ((current-rule (gethash 'in rules)))
    (iter outer
      (with-slots (code label) current-rule
        (iter
          (for branch in code)
          (for result = (funcall branch part))
          (when (or (eq result 'r)
                    (eq result 'a))
            (return-from outer result))
          (when result
            (setf current-rule (gethash result rules))
            (finish)))))))

(defun part-1 (&optional (file-relative-path "src/day-19.in"))
  (bind (((rules . parts) (read-problem file-relative-path)))
    (iter
      (for part in parts)
      (when (eq 'A (run-rules part rules))
        (with-slots (x m a s) part
          (summing (+ x m a s)))))))

;; Wrong: 94

(defun test-1 ()
  (part-1 "src/day-19-test.in"))

(defstruct literal-rule
  (label nil)
  (code nil))

(defstruct literal-comparison
  (slot nil)
  (comparator nil)
  (amount 0 :type fixnum)
  (dest nil))

(defun parse-rule-2 (rule-s)
  (match rule-s
    ((ppcre "([xmas])([><])([0-9]+):([a-zAR]+)"
            (read slot)
            (read comparator)
            (read amount)
            (read dest))
     (make-literal-comparison
      :slot slot
      :comparator comparator
      :amount amount
      :dest dest))
    ((ppcre "([a-zAR]+)"
            (read dest))
     dest)))

(defun parse-code-2 (line)
  (bind ((start-code (position #\{ line))
         (name (read-from-string (subseq line 0 start-code)))
         (code (subseq line (1+ start-code) (1- (length line))))
         (rules-s (split "," code))
         (rules (mapcar #'parse-rule-2 rules-s)))
    (make-literal-rule :label name
                       :code rules)))

(defun read-problem-2 (&optional (file-relative-path "src/day-19.in"))
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp file-relative-path))
    (labels ((recur-parts (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur-parts (cons (parse-part line) acc)))))
             (recur-rules (acc)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((string-equal line "")
                    (cons (rules-to-hash-map acc) (recur-parts nil)))
                   (t (recur-rules (cons (parse-code-2 line) acc)))))))
      (recur-rules nil))))

(defun goes-to (branch dest)
  (or (eq branch dest)
      (and (eq (type-of branch) 'literal-comparison)
           (eq (literal-comparison-dest branch) dest))))

(defun rules-ending-a (rules)
  (iter
    (for (label rule) in-hashtable rules)
    (with-slots (code) rule
      (when (iter
              (for branch in code)
              (thereis (goes-to branch 'a)))
        (collecting rule)))))

(defun rule-going-to (rules target-dest)
  (iter outer
    (for (label rule) in-hashtable rules)
    (with-slots (code) rule
      (when (iter
              (for branch in code)
              (thereis (or (eq (last code) target-dest)
                           (and (eq (type-of branch) 'literal-comparison)
                                (with-slots (dest) branch
                                  (eq dest target-dest))))))
        (return-from outer rule)))))

(defun invert-comparator (comparator)
  (if (eq comparator '>)
      '<
      '>))

(defmethod literal-comparison-invert ((c literal-comparison))
  (with-slots (slot comparator amount dest) c
    (bind ((inverted-comparator (invert-comparator comparator)))
      (make-literal-comparison
       :slot slot
       :comparator inverted-comparator
       :amount (if (eq inverted-comparator '<) (1+ amount) (1- amount))
       :dest dest))))

(defun rule-restriction (rule target-dest)
  (with-slots (code) rule
    (bind ((individual-branch (iter outer
                                (for branch in code)
                                (when (eq (type-of branch) 'literal-comparison)
                                  (with-slots (dest) branch
                                    (when (eq dest target-dest)
                                      (return-from outer branch)))))))
      (or individual-branch
          (iter
            (for branch in (butlast code))
            (collecting (literal-comparison-invert branch)))))))

(defun trace-back (rule rules)
  (labels ((recur (rule restrictions)
             ;; (format t "Trace-back rule: ~a~%" rule)
             (with-slots (label code) rule
               (if (eq label 'in)
                   (cons restrictions rule)
                   (bind ((coming-from (rule-going-to rules label))
                          (restriction (rule-restriction coming-from label)))
                     ;; (format t "Trace-back coming-from: ~a~%" (literal-rule-label coming-from))
                     ;; (format t "Trace-back restriction: ~a~%" restriction)
                     (recur coming-from (if (listp restriction)
                                            (append restriction restrictions)
                                            (cons restriction restrictions))))))))
    (recur rule nil)))

;; Inclusive range
(defstruct prange
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defstruct part-range
  (x-range (make-prange :start 0 :end 0) :type prange)
  (m-range (make-prange :start 0 :end 0) :type prange)
  (a-range (make-prange :start 0 :end 0) :type prange)
  (s-range (make-prange :start 0 :end 0) :type prange))

(defvar full-prange
  (make-prange
   :start 1
   :end 4000))

(defvar full-part-range
  (make-part-range
   :x-range full-prange
   :m-range full-prange
   :a-range full-prange
   :s-range full-prange))

(defmethod literal-comparison-to-part-range ((l literal-comparison))
  (with-slots (slot comparator amount) l
    (bind ((range (if (eq comparator '<)
                      (make-prange
                       :start (prange-start full-prange)
                       :end (1- amount))
                      (make-prange
                       :start (1+ amount)
                       :end (prange-end full-prange)))))
      (make-part-range
       :x-range (if (eq slot 'x) range full-prange)
       :m-range (if (eq slot 'm) range full-prange)
       :a-range (if (eq slot 'a) range full-prange)
       :s-range (if (eq slot 's) range full-prange)))))

(defmethod prange-intersect ((pr1 prange) (pr2 prange))
  (bind ((s1 (prange-start pr1))
         (e1 (prange-end pr1))
         (s2 (prange-start pr2))
         (e2 (prange-end pr2)))
    (cond
      ;; Contained
      ((and (>= s1 s2)
            (<= e1 e2)
            ; (print 1)
            )
       (make-prange
        :start s1
        :end e1))

      ;; On left
      ((and (<= s1 s2)
            (<= e1 e2)
            ; (print 2)
            )
       (make-prange
        :start s2
        :end e1))

      ;; On right
      ((and (<= s1 e2)
            (>= e1 e2)
            ; (print 3)
            )
       (make-prange
        :start s1
        :end e2))

      ;; Contains
      ((and (< s1 s2)
            (> e1 e2)
            ;(print 4)
            )
       (make-prange
        :start s2
        :end e2))

      ;; No intersection.  Shouldn't happen.
      (t (error "No intersection")))))

(defmethod part-range-intersect ((p1 part-range) (p2 part-range))
  (make-part-range
   :x-range (prange-intersect (part-range-x-range p1) (part-range-x-range p2))
   :m-range (prange-intersect (part-range-m-range p1) (part-range-m-range p2))
   :a-range (prange-intersect (part-range-a-range p1) (part-range-a-range p2))
   :s-range (prange-intersect (part-range-s-range p1) (part-range-s-range p2))))

(defun ranges (restrictions-start)
  ;; (format t "restrictions-start: ~a~%" restrictions-start)
  (labels ((recur (restrictions ranges)
             ;; (format t "(car restrictions): ~a~%" (car restrictions))
             ;; (format t "ranges: ~a~%" ranges)
             (if (null restrictions) ranges
                 (recur (cdr restrictions)
                        (mapcar (lambda (range)
                                  (part-range-intersect range
                                                        (literal-comparison-to-part-range (car restrictions))))
                                ranges)))))
    (recur restrictions-start (list full-part-range))))

(defun remove-invalid (pranges)
  (remove-if (lambda (range)
               (or (null range)
                   (with-slots (start end) range
                     (= start end))))
             pranges))

(defmethod prange-intersect-with-2 ((pr1 prange) (pr2 prange))
  (bind ((s1 (prange-start pr1))
         (e1 (prange-end pr1))
         (s2 (prange-start pr2))
         (e2 (prange-end pr2)))
    (->> (cond
           ;; Contained
           ((and (>= s1 s2)
                 (<= e1 e2)
                 ;; (print 1)
                 )
            (list pr1
                  (make-prange
                   :start s2
                   :end (max 1 (1- s1)))
                  (make-prange
                   :start (min 4000 (1+ e1))
                   :end e2)))

           ;; On left
           ((and (< s1 s2)
                 (<= e1 e2)
                 ;; (print 2)
                 )
            (remove nil
                    (list (make-prange
                           :start s1
                           :end (max 1 (1- s2)))
                          (make-prange
                           :start s2
                           :end e1)
                          (when (< e1 e2)
                            (make-prange
                             :start (min 4000 (1+ e1))
                             :end e2))
                          )))

           ;; On right
           ((and (<= s1 e2)
                 (> e1 e2)
                 ;; (print 3)
                 )
            (remove nil (list
                         (make-prange
                          :start (min 4000 (1+ e2))
                          :end e1)
                         (when (> s1 s2)
                           (make-prange
                            :start s2
                            :end (max 1 (1- s1))))
                         (make-prange
                          :start s1
                          :end e2))))

           ;; Contains
           ((and (< s1 s2)
                 (> e1 e2)
                 ;; (print 4)
                 )
            (list pr2
                  (make-prange
                   :start s1
                   :end (max 1 (1- s2)))
                  (make-prange
                   :start (min 4000 (1+ e2))
                   :end e1)
                  ))

           ;; No intersection
           (t (list nil pr1)))
      remove-invalid)))

(defmethod prange-overlaps-p ((pr1 prange) (pr2 prange))
  (bind ((s1 (prange-start pr1))
         (e1 (prange-end pr1))
         (s2 (prange-start pr2))
         (e2 (prange-end pr2)))
    (or (and (>= s1 s2)
             (<= s1 e2))
        (and (<= e1 e2)
             (>= e1 s2)))))

(defmethod part-range-intersect-with-2 ((p1 part-range) (p2 part-range))
  (bind ((p1-x (part-range-x-range p1))
         (p1-m (part-range-m-range p1))
         (p1-a (part-range-a-range p1))
         (p1-s (part-range-s-range p1))
         (p2-x (part-range-x-range p2))
         (p2-m (part-range-m-range p2))
         (p2-a (part-range-a-range p2))
         (p2-s (part-range-s-range p2)))
    (if (and (prange-overlaps-p p1-x p2-x)
             (prange-overlaps-p p1-m p2-m)
             (prange-overlaps-p p1-a p2-a)
             (prange-overlaps-p p1-s p2-s))
        (progn
          (format t "Does overlap (list p1 p2): ~a~%" (list p1 p2))
          (iter outer
           (for x-range in (prange-intersect-with-2 p1-x p2-x))
           (iter
             (for m-range in (prange-intersect-with-2 p1-m p2-m))
             (iter
               (for a-range in (prange-intersect-with-2 p1-a p2-a))
               (iter
                 (for s-range in (prange-intersect-with-2 p1-s p2-s))
                 (in outer
                     (collecting (make-part-range
                                  :x-range x-range
                                  :m-range m-range
                                  :a-range a-range
                                  :s-range s-range))))))))
        (list p1))))

(defun intersect-all-ranges (part-ranges)
  (bind ((ranges-as-array (coerce part-ranges 'vector)))
    (iter outer
      (for i from 0 below (length part-ranges))
      (bind ((non-intersecting (list (aref ranges-as-array i))))
        (iter
          (format t "non-intersecting: ~a~%" non-intersecting)
          (for j from 0 below (length part-ranges))
          (for other = (aref ranges-as-array j))
          (when (/= i j)
            (setf non-intersecting
                  (iter
                    (for outside in non-intersecting)
                    (appending (part-range-intersect-with-2 outside other))))))
        (summing (apply #'+ (mapcar #'part-range-size non-intersecting)))))))

(defmethod prange-size ((p prange))
  (with-slots (start end) p
    (1+ (- start end))))

(defmethod part-range-size ((p part-range))
  (with-slots (x-range m-range a-range s-range) p
    (* (prange-size x-range)
       (prange-size m-range)
       (prange-size a-range)
       (prange-size s-range))))

;; IntervalMap is
;;  ((prange . (IntervalMap|prange)))

(defstruct part-interval
  (range (make-prange :start 0 :end 0) :type prange)
  (value nil))

(defun interval-map-insert (map remaining-dimensions)
  (format t "insert: ~a~%" remaining-dimensions)
  (if (null remaining-dimensions)
      (if (numberp map) (1+ map) 1)
      (bind (((to-insert . rest) remaining-dimensions))
        (if (null map)
            (list (make-part-interval
                   :range to-insert
                   :value (interval-map-insert nil rest)))
            (bind ((intersected nil)
                   (with-intersections
                       (iter
                         (for p-interval in map)
                         (with-slots (range value) p-interval
                           (bind (((overlap . non-overlaps) (prange-intersect-with-2 to-insert range)))
                             (declare (ignore non-overlaps))
                             (when overlap
                               (collecting (make-part-interval
                                            :range overlap
                                            :value (interval-map-insert value rest)))
                               (setf intersected t)))))))
              (if (not intersected)
                  (cons (make-part-interval
                         :range to-insert
                         :value (interval-map-insert nil rest))
                        with-intersections)
                  with-intersections))))))

(defun insert-all (ranges)
  (bind ((map nil))
    (iter
      (for range in ranges)
      (with-slots (x-range m-range a-range s-range) range
        (bind ((dimensions (list x-range m-range a-range s-range)))
          (setf map (interval-map-insert map dimensions))))
      (format t "tick~%"))
    map))

(defun map-size (map)
  (if (numberp map)
      map
      (iter
        (for p-interval in map)
        (with-slots (range value) p-interval
          (with-slots (start end) range
            (summing (* (1+ (- end start))
                        (map-size value))))))))

(defun part-2 (&optional (file-relative-path "src/day-19.in"))
  (bind (((rules . parts) (read-problem-2 file-relative-path))
         (terminals (rules-ending-a rules))
         (all-ranges (->> (mapcar (lambda (terminal)
                                    (trace-back terminal rules))
                                  terminals)
                       (mapcar #'car)
                       (mapcar #'ranges)
                       (mapcar #'car)))
         (map (insert-all (subseq all-ranges 0))))
    (declare (ignore parts))
    ;; (format t "map: ~a~%" map)
    ;; (format t "all-ranges: ~a~%" all-ranges)
    ;; (format t "terminal: ~a~%" (nth 4 terminals))
    ;; (ranges (car (trace-back (nth 4 terminals) rules)))
    ;; (apply #'+ (mapcar #'part-range-size intersected))
    (map-size map)
    (mapcar #'part-interval-range map)))

(defun test-2 ()
  (part-2 "src/day-19-test.in"))
