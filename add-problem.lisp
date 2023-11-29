(require 'asdf)

(defun main ()
  "Create a problem file.

Reads the problem number from argv.  In SBCL, that's *posix-argv*,
apparently."
  (destructuring-bind (program day) *posix-argv*
    (declare (ignore program))
    (when (null day)
      (format *error-output*
              "Too few arguments!~%Expected: (day).~%Got: ~a~%"
              (cdr *posix-argv*))
      (exit :code 1))
    (let* ((n-day (read-from-string day))
           (package-name (format nil "day-~a" n-day))
           (file-name (format nil "~a.lisp" package-name))
           (input-file (format nil "~a.in" package-name)))
      (when (not (numberp n-day))
        (format *error-output*
                "Day is not a number!~%Got ~a~%"
                (cdr *posix-argv*)))
      (with-open-file (file (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp
                                                           (format nil
                                                                   "src/~a"
                                                                   file-name))
                            :direction :output
                            :if-does-not-exist :create)
        (format file
                "(eval-when (:compile-toplevel :load-toplevel :execute)~%  (defun shadowing-import-from-fset ()~%    `(:shadowing-import-from~%     :fset~%      ,@(let ((symbols nil))~%        (do-external-symbols (symbol (find-package 'fset) symbols)~%          (push symbol symbols))))))~%~%")
        (format file
                "(defpackage ~a~%  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)~%  #.(shadowing-import-from-fset)~%  (:shadowing-import-from :arrow-macros :->>)~%  (:shadowing-import-from :arrow-macros :->))~%"
                package-name)
        (format file
                "(in-package ~a)~%~%"
                package-name))
      (with-open-file (file (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp
                                                           (format nil
                                                                   "src/~a"
                                                                   input-file))
                            :direction :output
                            :if-does-not-exist :create)
        (format file ""))
      (uiop:run-program (format nil
                                "emacs --batch --script add-problem.el ~a"
                                package-name)))))

(eval-when (:execute)
  (main))
