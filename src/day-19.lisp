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

(defun trace-forward (rules)
  (bind ((all-restriction-sets nil))
    (labels ((recur (rule restrictions)
               (bind ((reversed nil))
                (iter
                  (for branch in (literal-rule-code rule))
                  (when (eq (type-of branch) 'literal-comparison)
                    (push (literal-comparison-invert branch) reversed))
                  (cond
                    ((eq branch 'r) nil)
                    ((eq branch 'a) (push (append reversed restrictions) all-restriction-sets))
                    ((eq (type-of branch) 'literal-comparison)
                     (with-slots (dest) branch
                       (if (eq dest 'a)
                           (push (cons branch (append (cdr reversed) restrictions)) all-restriction-sets)
                           (when (not (eq dest 'r))
                             (recur (gethash dest rules)
                                    (cons branch (append (cdr reversed) restrictions)))))))
                    (t (recur (gethash branch rules)
                              (append reversed restrictions))))))))
      (recur (gethash 'in rules) nil)
      all-restriction-sets)))

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
       pr1)

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
       pr2)

      ;; No intersection.  Shouldn't happen.
      (t nil))))

(defmethod part-range-intersect ((p1 part-range) (p2 part-range))
  (bind ((x (prange-intersect (part-range-x-range p1) (part-range-x-range p2)))
         (m (prange-intersect (part-range-m-range p1) (part-range-m-range p2)))
         (a (prange-intersect (part-range-a-range p1) (part-range-a-range p2)))
         (s (prange-intersect (part-range-s-range p1) (part-range-s-range p2))))
    (when (not (some #'null (list x m a s)))
     (make-part-range
      :x-range x
      :m-range m
      :a-range a
      :s-range s))))

(defun ranges (restrictions-start)
  (labels ((recur (restrictions range)
             (if (null restrictions)
                 range
                 (recur (cdr restrictions)
                        (part-range-intersect range
                                              (literal-comparison-to-part-range (car restrictions)))))))
    (recur restrictions-start full-part-range)))

(defmethod prange-size ((p prange))
  (with-slots (start end) p
    (1+ (- end start))))

(defmethod part-range-size ((p part-range))
  (with-slots (x-range m-range a-range s-range) p
    (* (prange-size x-range)
       (prange-size m-range)
       (prange-size a-range)
       (prange-size s-range))))

(defun part-2 (&optional (file-relative-path "src/day-19.in"))
  (bind (((rules . parts) (read-problem-2 file-relative-path))
         (rule-sets (trace-forward rules))
         (all-ranges (mapcar #'ranges rule-sets)))
    (declare (ignore parts))
    (->> (mapcar #'part-range-size all-ranges) (apply #'+))))

(defun test-2 ()
  (part-2 "src/day-19-test.in"))
