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
                 (convert-once fertilizer)
                 (convert-once water)
                 (convert-once light)
                 (convert-once temperature)
                 (convert-once humidity)
                 (convert-once location))))
      (read-problem))))

;; Found by leaving it for long enough... /shrug 2008785

(defun part-2-faster ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-5.in"))
    (labels ((read-group (group)
               (bind ((line (read-line f nil nil)))
                 (if (or (null line) (string-equal line ""))
                     (reverse group)
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
                      (location    (read-group nil))
                      (current-ranges (list (find 56 ;; 0
                                                  location :key #'car
                                                  )))
                      (next-ranges nil))
                 (format t "current-ranges: ~a~%" current-ranges)
                 (iter
                   (for other-ranges in (list humidity temperature light water fertilizer soil))
                   (iter
                     (for range in other-ranges)
                     (format t "range: ~a~%" range)
                     (iter
                       (for current-range in current-ranges)
                       (for i = (intersect-ranges range current-range))
                       (when i
                         (format t "(list range current-range i): ~a~%" (list range current-range i))
                         (push i next-ranges))))
                   (format t "current-ranges: ~a~%" current-ranges)
                   (setf current-ranges next-ranges
                         next-ranges    nil))
                 (convert-seed (print (cadr (car (print current-ranges))))
                               soil
                               fertilizer
                               water
                               light
                               temperature
                               humidity
                               location)
                 ;; (iter outer
                 ;;   (for (d s a) in current-ranges)
                 ;;   (iter
                 ;;     (for x from s below (+ s a))
                 ;;     (in outer (minimizing (convert-seed (cadr (car current-ranges))
                 ;;                                         soil
                 ;;                                         fertilizer
                 ;;                                         water
                 ;;                                         light
                 ;;                                         temperature
                 ;;                                         humidity
                 ;;                                         location)))))
                 ))
             (intersect-ranges (range-1 range-2)
               (bind (((d1 s1 x1) range-1)
                      ((d2 s2 x2) range-2)
                      (start-2 s2)
                      (end-2   (+ s2 x2))
                      (start-1 d1)
                      (end-1   (+ d1 x1)))
                 (cond
                   ((>= start-2 start-1) (if (<= start-2 end-1)
                                             (if (<= end-2 end-1)
                                                 (list start-2 s1 (- end-2 start-2))
                                                 (list start-2 s1 (- end-1 start-2)))
                                             nil))
                   ((< start-2 start-1) (if (>= end-2 start-1)
                                            (if (<= end-2 end-1)
                                                (list start-1 s1 (- end-2 start-1))
                                                (list start-1 s1 (- end-1 start-1)))
                                            nil))
                   (t nil))))
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
                       (format t "containing: ~a~%" containing)
                       (format t "candidates ~a~%"(remove-if-not (lambda (range)
                                         (bind (((destination source amount) range))
                                           (and (>= x source)
                                                (<= x (+ source amount)))))
                                       ranges))
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
                 print
                 (convert-once fertilizer)
                 print
                 (convert-once water)
                 print
                 (convert-once light)
                 print
                 (convert-once temperature)
                 print
                 (convert-once humidity)
                 print
                 (convert-once location)
                 print
                 )))
      (read-problem))))

;; 2154440019
;; 288460629

(defun part-2-faster-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-5.in"))
    (labels ((read-group (group)
               (bind ((line (read-line f nil nil)))
                 (if (or (null line) (string-equal line ""))
                     (reverse group)
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
                      (location    (read-group nil))
                      (current-ranges (iter
                                        (for xs on seeds by #'cddr)
                                        (collecting (list (car xs)
                                                          (+ (car xs) (cadr xs))))))
                      (prev-current-ranges)
                      (next-ranges nil))
                 (iter
                   (for other-ranges in (list soil fertilizer water light temperature humidity location))
                   (iter
                     (while (not (equal current-ranges prev-current-ranges)))
                     (setf prev-current-ranges current-ranges)
                     (for current-range = (pop current-ranges))
                     (iter
                       (for range in other-ranges)
                       (format t "range: ~a~%" range)
                       (for (rem i) = (intersect-ranges range current-range))
                       (when i
                         (format t "(list range current-range i): ~a~%" (list range current-range i))
                         (push i next-ranges))
                       (when rem
                         (format t "(list range current-range rem): ~a~%" (list range current-range rem))
                         (push rem current-ranges))))
                   (format t "current-ranges: ~a~%" next-ranges)
                   (setf current-ranges next-ranges
                         next-ranges    nil))
                 (iter
                   (for (beg end) in current-ranges)
                   (minimizing beg))))
             (intersect-ranges (range-1 range-2)
               (bind (((d1 s1 x1)      range-1)
                      ((start-2 end-2) range-2)
                      (start-1 s1)
                      (end-1   (+ s1 x1)))
                 (cond
                   ((>= start-2 start-1) (if (<= start-2 end-1)
                                             (if (<= end-2 end-1)
                                                 (list nil
                                                       (list start-2 end-2))
                                                 (list (list (1+ end-1) end-2)
                                                       (list start-2 end-1)))
                                             (list range-2 nil)))
                   ((< start-2 start-1) (if (>= end-2 start-1)
                                            (if (<= end-2 end-1)
                                                (list (list start-2 (1- start-1))
                                                      (list start-1 end-2))
                                                (list (list start-1 end-1)
                                                      (list start-2 (1- start-1))
                                                      (list (1+ end-1) end-2)))
                                            (list range-2 nil)))
                   (t nil))))
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
                       (format t "containing: ~a~%" containing)
                       (format t "candidates ~a~%"(remove-if-not (lambda (range)
                                                                   (bind (((destination source amount) range))
                                                                     (and (>= x source)
                                                                          (<= x (+ source amount)))))
                                                                 ranges))
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
                 print
                 (convert-once fertilizer)
                 print
                 (convert-once water)
                 print
                 (convert-once light)
                 print
                 (convert-once temperature)
                 print
                 (convert-once humidity)
                 print
                 (convert-once location)
                 print
                 )))
      (read-problem))))

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

