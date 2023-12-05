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

