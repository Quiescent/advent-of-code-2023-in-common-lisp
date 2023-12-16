(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-5
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-5)

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-5.in"))
    (labels ((read-group (group)
               (bind ((line (read-line f nil nil)))
                 (if (or (null line) (string-equal line ""))
                     (bind ((table (-> (coerce group 'vector)
                                     (sort #'< :key #'cadr)))
                            (starts (->> (cl:map 'list #'cadr table)
                                      (convert 'set))))
                       (lambda (x)
                         (bind ((i (rank starts x)))
                           (if (= i -1)
                               x
                               (bind (((destination source amount) (aref table i)))
                                 (if (and (>= x source)
                                          (<= x (+ source amount)))
                                     (+ destination (- x source))
                                     x))))))
                     (read-group (cons (-> (format nil "(~a)" line) (read-from-string))
                                       group)))))
             (read-break ()
               (progn
                 (read-line f nil nil)
                 (read-line f nil nil)))
             (read-break-1 ()
               (read-line f nil nil))
             (read-problem ()
               (bind ((seeds (->> (subseq (read-line f nil nil) 6)
                               (format nil "(~a)")
                               read-from-string))
                      (_ (read-break))
                      (soil        (read-group nil))
                      (_ (read-break-1))
                      (fertilizer  (read-group nil))
                      (_ (read-break-1))
                      (water       (read-group nil))
                      (_ (read-break-1))
                      (light       (read-group nil))
                      (_ (read-break-1))
                      (temperature (read-group nil))
                      (_ (read-break-1))
                      (humidity    (read-group nil))
                      (_ (read-break-1))
                      (location    (read-group nil)))
                 (iter outer
                   (for xs on seeds by #'cddr)
                   (for start = (car xs))
                   (for end = (cadr xs))
                   (format t "(list start end): ~a~%" (list start end))
                   (iter
                     (for x from start to (+ start end))
                     (in outer (minimizing (convert-seed x
                                                         soil
                                                         fertilizer
                                                         water
                                                         light
                                                         temperature
                                                         humidity
                                                         location)))))))
             (convert-once (x ranges)
               (funcall ranges x))
             (convert-seed (seed
                            soil
                            fertilizer
                            water
                            light
                            temperature
                            humidity
                            location)
               (-> (convert-once seed soil)
                 (convert-once fertilizer)
                 (convert-once water)
                 (convert-once light)
                 (convert-once temperature)
                 (convert-once humidity)
                 (convert-once location))))
      (read-problem))))

;; Found by leaving it for long enough... /shrug 2008785

;; Note: was on the right track with the intersecting sets solution I
;; had and just messed up too many times :/

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-5.in"))
    (labels ((read-group (group)
               (bind ((line (read-line f nil nil)))
                 (if (or (null line) (string-equal line ""))
                     group
                     (read-group (cons (-> (format nil "(~a)" line) (read-from-string))
                                       group)))))
             (read-break ()
               (progn
                 (read-line f nil nil)
                 (read-line f nil nil)))
             (read-break-1 ()
               (read-line f nil nil))
             (read-problem ()
               (bind ((seeds (->> (subseq (read-line f nil nil) 6)
                               (format nil "(~a)")
                               read-from-string))
                      (_ (read-break))
                      (soil        (read-group nil))
                      (_ (read-break-1))
                      (fertilizer  (read-group nil))
                      (_ (read-break-1))
                      (water       (read-group nil))
                      (_ (read-break-1))
                      (light       (read-group nil))
                      (_ (read-break-1))
                      (temperature (read-group nil))
                      (_ (read-break-1))
                      (humidity    (read-group nil))
                      (_ (read-break-1))
                      (location    (read-group nil)))
                 (apply #'min
                        (mapcar (lambda (seed) (convert-seed seed
                                                             soil
                                                             fertilizer
                                                             water
                                                             light
                                                             temperature
                                                             humidity
                                                             location))
                                seeds))))
             (convert-once (x ranges)
               (bind ((containing
                       (find-if (lambda (range)
                                  (bind (((destination source amount) range))
                                    (and (>= x source)
                                         (<= x (+ source amount)))))
                                ranges)))
                 (if (null containing)
                     x
                     (bind (((destination source amount) containing))
                       (+ destination (- x source))))))
             (convert-seed (seed
                            soil
                            fertilizer
                            water
                            light
                            temperature
                            humidity
                            location)
               (-> (convert-once seed soil)
                 ;; print
                 (convert-once fertilizer)
                 ;; print
                 (convert-once water)
                 ;; print
                 (convert-once light)
                 ;; print
                 (convert-once temperature)
                 ;; print
                 (convert-once humidity)
                 ;; print
                 (convert-once location)
                 ;; print
                 )))
      (read-problem))))

;; Alternative solution

(defstruct range
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (sub 0 :type fixnum))

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-5.in"))
    (labels ((recur (acc current-mapping)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((null line) (nreverse (cons (nreverse current-mapping) acc)))
                   ((string-equal line "") (recur (cons (nreverse current-mapping) acc) nil))
                   (t (match line
                        ((ppcre "(-?[0-9]+) (-?[0-9]+) (-?[0-9]+)"
                                (read dest-start)
                                (read source-start)
                                (read range-length))
                         (recur acc (cons (make-range :start source-start
                                                      :end (+ source-start range-length)
                                                      :sub (- source-start dest-start))
                                          current-mapping)))
                        (_ (recur acc current-mapping))))))))
      (cons (->> (read-line f nil nil) (split " ") cdr (mapcar #'read-from-string))
            (progn (read-line f nil nil)
                   (recur nil nil))))))

(defmethod range-contains ((r range) seed)
  (with-slots (start end) r
    (and (>= seed start)
         (<= seed end))))

(defmethod range-transform ((r range) seed)
  (with-slots (sub) r
    (- seed sub)))

(defun part-1-alt ()
  (bind (((seeds . categories) (read-problem)))
    (iter
      (for seed in seeds)
      (minimize
       (iter
         (for category in categories)
         (for range = (find-if (lambda (range) (range-contains range seed)) category))
         (when range
           (setf seed (range-transform range seed)))
         (finally (return seed)))))))

(defun seeds-to-ranges (seeds)
  (when seeds
    (cons (make-range :start (car seeds)
                      :end (+ (car seeds) (cadr seeds))
                      :sub 0)
          (seeds-to-ranges (cddr seeds)))))

(defmethod range-intersection ((this range) (other range))
  (bind ((this-start (range-start this))
         (this-end (range-end this))
         (other-start (range-start other))
         (other-end (range-end other))
         (other-sub (range-sub other)))
    (cond

      ;; Contained
      ((and (>= this-start other-start)
            (<= this-end other-end))
       (cons nil
             (make-range :start (- this-start other-sub)
                         :end (- this-end other-sub)
                         :sub other-sub)))

      ;; Overlaps on either side
      ((and (< this-start other-start)
            (> this-end other-end))
       (cons (list (make-range :start this-start
                               :end (1- other-start)
                               :sub other-sub)
                   (make-range :start (1+ other-start)
                               :end this-end
                               :sub other-sub))
             (make-range :start (- other-start other-sub)
                         :end (- other-end other-sub)
                         :sub other-sub)))

      ;; Overlaps at start
      ((and (< this-start other-start)
            (>= this-end other-start)
            (<= this-end other-end))
       (cons (list (make-range :start this-start
                               :end (1- other-start)
                               :sub other-sub))
             (make-range :start (- other-start other-sub)
                         :end (- this-end other-sub)
                         :sub other-sub)))

      ;; Overlaps at end
      ((and (<= this-start other-end)
            (>= this-start other-start)
            (> this-end other-end))
       (cons (list (make-range :start (+ other-end 1)
                               :end this-end
                               :sub other-sub))
             (make-range :start (- this-start other-sub)
                         :end (- other-end other-sub)
                         :sub other-sub)))

      ;; No intersection
      (t (cons (list this) nil)))))

(defun intersect-all (range category)
  (if (null category)
      (list range)
      (bind (((outside . intersected) (range-intersection range (car category))))
        (bind ((rest (reduce (lambda (acc next-out)
                               (nconc acc (intersect-all next-out (cdr category))))
                             outside
                             :initial-value nil)))
         (if intersected (cons intersected rest) rest)))))

(defun transform-ranges (ranges categories)
  (if (null categories)
      ranges
      (bind (((category . rest) categories)
             (new-ranges (reduce (lambda (acc range)
                                   (nconc acc (intersect-all range category)))
                                 ranges
                                 :initial-value nil)))
        (transform-ranges new-ranges rest))))

(defun part-2-alt ()
  (bind (((seeds . categories) (read-problem))
         (in-ranges (seeds-to-ranges seeds)))
    (->> (transform-ranges in-ranges categories)
      (mapcar #'range-start)
      (apply #'min))))
