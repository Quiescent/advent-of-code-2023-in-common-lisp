(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-20
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-20)

(defstruct module
  (output nil)
  (inputs nil)
  (outputs nil)
  (type nil)
  (label nil))

(defun parse-line (line)
  (match line
    ((ppcre "%([a-zA-Z]+) -> (.*)"
            (read label)
            destinations)
     (make-module
      :output nil
      :inputs nil
      :outputs (->> (split ", " destinations)
                 (mapcar #'read-from-string))
      :type 'flip-flop
      :label label))
    ((ppcre "&([a-zA-Z]+) -> (.*)"
            (read label)
            destinations)
     (make-module
      :output nil
      :inputs nil
      :outputs (->> (split ", " destinations)
                 (mapcar #'read-from-string))
      :type 'conjunction
      :label label))
    ((ppcre "([a-zA-Z]+) -> (.*)"
            (read label)
            destinations)
     (make-module
      :output nil
      :inputs nil
      :outputs (->> (split ", " destinations)
                 (mapcar #'read-from-string))
      :type label
      :label label))))

(defstruct module-input
  (source nil)
  (value nil))

(defun modules-to-hashmap (modules)
  (bind ((module-map (make-hash-table)))
    (iter
      (for module in modules)
      (with-slots (label) module
        (setf (gethash label module-map) module)))
    (iter
      (for module in modules)
      (with-slots (outputs label) module
        (iter
          (for output in outputs)
          (when (null (gethash output module-map))
            (setf (gethash output module-map)
                  (make-module
                   :inputs nil
                   :outputs nil
                   :type output
                   :label output)))
          (when (not (eq 'output output))
            (push (make-module-input
                   :source label
                   :value nil)
                  (module-inputs (gethash output module-map)))))))
    module-map))

(defun read-problem (relative-path-name)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp relative-path-name))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (modules-to-hashmap acc)
                     (recur (cons (parse-line line)
                                  acc))))))
      (recur nil))))

(defstruct module-signal
  (value nil)
  (dest-label nil)
  (source-label nil))

(defun send-signals (circuit)
  (bind ((queue (empty-seq)))
    (labels ((push-button ()
               (setf queue (with-last queue (make-module-signal
                                             :value nil
                                             :dest-label 'broadcaster
                                             :source-label 'button)))
               (iter
                 (while (not (empty? queue)))
                 (for signal = (first queue))
                 ;; (format t "signal: ~a~%" signal)
                 (for signal-value = (module-signal-value signal))
                 (counting signal-value into high-pulses)
                 (counting (not signal-value) into low-pulses)
                 (setf queue (less-first queue))
                 (for new-to-process = (process-signal signal))
                 (setf queue (concat queue (convert 'seq new-to-process)))
                 (finally (return (cons high-pulses low-pulses)))))
             (process-signal (signal)
               (with-slots (value dest-label source-label) signal
                 (bind ((dest (gethash dest-label circuit)))
                   ;; (format t "dest: ~a~%" dest)
                   (case (module-type dest)
                     (flip-flop (when (not value)
                                  (setf (module-output dest) (not (module-output dest)))
                                  (mapcar (lambda (output)
                                            (make-module-signal
                                             :value (module-output dest)
                                             :dest-label output
                                             :source-label dest-label))
                                          (module-outputs dest))))
                     (conjunction (progn
                                    (iter
                                      (for input in (module-inputs dest))
                                      (when (eq (module-input-source input) source-label)
                                        (setf (module-input-value input) value)
                                        (finish)))
                                    (setf (module-output dest)
                                          (not (every #'module-input-value (module-inputs dest))))
                                    (mapcar (lambda (output)
                                              (make-module-signal
                                               :value (module-output dest)
                                               :dest-label output
                                               :source-label dest-label))
                                            (module-outputs dest))))
                     (broadcaster (mapcar (lambda (output)
                                            (make-module-signal
                                             :value nil
                                             :dest-label output
                                             :source-label dest-label))
                                          (module-outputs dest))))))))
      (iter
        (for i from 1 to 1000)
        (for (high-pulses . low-pulses) = (push-button))
        (summing high-pulses into total-high-pulses)
        (summing low-pulses into total-low-pulses)
        (format t "(list high-pulses low-pulses): ~a~%" (list high-pulses low-pulses))
        (finally (return (* total-low-pulses total-high-pulses)))))))

(defun part-1 (&optional (relative-path-name "src/day-20.in"))
  (bind ((problem (read-problem relative-path-name)))
    (send-signals problem)))

(defun test-1 ()
  (part-1 "src/day-20-test.in"))

(defun send-signals-2 (circuit)
  (bind ((queue (empty-seq))
         (delivered nil))
    (labels ((push-button ()
               (setf queue (with-last queue (make-module-signal
                                             :value nil
                                             :dest-label 'broadcaster
                                             :source-label 'button)))
               (iter
                 (while (not (empty? queue)))
                 (for signal = (first queue))
                 ;; (format t "signal: ~a~%" signal)
                 (for signal-value = (module-signal-value signal))
                 (counting signal-value into high-pulses)
                 (counting (not signal-value) into low-pulses)
                 (setf queue (less-first queue))
                 (for new-to-process = (process-signal signal))
                 (setf queue (concat queue (convert 'seq new-to-process)))
                 (finally (return (cons high-pulses low-pulses)))))
             (process-signal (signal)
               (with-slots (value dest-label source-label) signal
                 (bind ((dest (gethash dest-label circuit)))
                   ;; (format t "dest: ~a~%" dest)
                   (when (and (eq dest-label 'rx)
                              (not value))
                     (setf delivered t))
                   (case (module-type dest)
                     (flip-flop (when (not value)
                                  (setf (module-output dest) (not (module-output dest)))
                                  (mapcar (lambda (output)
                                            (make-module-signal
                                             :value (module-output dest)
                                             :dest-label output
                                             :source-label dest-label))
                                          (module-outputs dest))))
                     (conjunction (progn
                                    (iter
                                      (for input in (module-inputs dest))
                                      (when (eq (module-input-source input) source-label)
                                        (setf (module-input-value input) value)
                                        (finish)))
                                    (setf (module-output dest)
                                          (not (every #'module-input-value (module-inputs dest))))
                                    (mapcar (lambda (output)
                                              (make-module-signal
                                               :value (module-output dest)
                                               :dest-label output
                                               :source-label dest-label))
                                            (module-outputs dest))))
                     (broadcaster (mapcar (lambda (output)
                                            (make-module-signal
                                             :value nil
                                             :dest-label output
                                             :source-label dest-label))
                                          (module-outputs dest))))))))
      (iter
        (for i from 1 to 100000000000)
        (push-button)
        (when delivered
          (return i))))))

(defun trace-back (circuit)
  (bind ((trace-back nil)
         (seen (make-hash-table)))
    (labels ((recur (node)
               (format t "node: ~a~%" node)
               (when (not (gethash node seen))
                 (setf (gethash node seen) t)
                 (bind ((module (gethash node circuit)))
                   (iter
                     (for input in (module-inputs module))
                     (recur (module-input-source input))
                     (push (module-input-source input) trace-back))))))
      (recur 'rx)
      trace-back)))

(defun part-2 (&optional (relative-path-name "src/day-20.in"))
  (bind ((problem (read-problem relative-path-name)))
    (trace-back problem)
    ;; (send-signals-2 problem)
    ))

(defun test-2 ()
  (part-2 "src/day-20-test.in"))
