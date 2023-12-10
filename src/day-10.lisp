(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-10
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-10)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp "src/day-10.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (coerce (nreverse acc) 'vector)
                     (recur (cons line acc))))))
      (recur nil))))

(defun find-start (grid)
  (iter outer
    (for y from 0 below (length grid))
    (iter
      (for x from 0 below (length (aref grid 0)))
      (when (char-equal #\S
                        (-> (aref grid y)
                          (aref x)))
        (return-from outer (cons x y))))))

(defun pipe-connects (from to direction)
  (cond
    ((char-equal to #\S)
     (case direction
       (left
        (case from
          (#\- t)
          (#\7 t)
          (#\J t)))
       (right
        (case from
          (#\- t)
          (#\F t)
          (#\L t)))
       (up
        (case from
          (#\| t)
          (#\L t)
          (#\J t)))
       (down
        (case from
          (#\| t)
          (#\7 t)
          (#\F t)))))
    ((char-equal from #\S)
     (case direction
       (left
        (case to
          (#\- t)
          (#\F t)
          (#\L t)))
       (right
        (case to
          (#\- t)
          (#\J t)
          (#\7 t)))
       (up
        (case to
          (#\| t)
          (#\7 t)
          (#\F t)))
       (down
        (case to
          (#\| t)
          (#\L t)
          (#\J t)))))
    (t (case from
         (#\| (case to
                (#\| (or (eq direction 'up)
                         (eq direction 'down)))
                (#\L (eq direction 'down))
                (#\J (eq direction 'down))
                (#\F (eq direction 'up))
                (#\7 (eq direction 'up))))
         (#\- (case to
                (#\- (or (eq direction 'left)
                         (eq direction 'right)))
                (#\J (eq direction 'right))
                (#\7 (eq direction 'right))
                (#\F (eq direction 'left))
                (#\L (eq direction 'left))))
         (#\L (case to
                (#\- (eq direction 'right))
                (#\J (eq direction 'right))
                (#\7 (or (eq direction 'right)
                         (eq direction 'up)))
                (#\| (eq direction 'up))
                (#\F (eq direction 'up))))
         (#\J (case to
                (#\- (eq direction 'left))
                (#\L (eq direction 'left))
                (#\F (or (eq direction 'up)
                         (eq direction 'left)))
                (#\7 (eq direction 'up))
                (#\| (eq direction 'up))))
         (#\7 (case to
                (#\| (eq direction 'down))
                (#\L (or (eq direction 'left)
                         (eq direction 'down)))
                (#\J (eq direction 'down))
                (#\F (eq direction 'left))
                (#\- (eq direction 'left))))
         (#\F (case to
                (#\- (eq direction 'right))
                (#\J (or (eq direction 'down)
                         (eq direction 'right)))
                (#\7 (eq direction 'right))
                (#\L (eq direction 'down))
                (#\| (eq direction 'down))))))))

(defun loop-length (grid)
  (bind (((start-x . start-y) (find-start grid)))
    (labels ((recur (acc x y from)
               (format t "(list x y acc): ~a~%" (list x y acc))
               (if (and from
                        (= x start-x)
                        (= y start-y))
                   acc
                   (bind ((left  (and (> x 0) (-> (aref grid y) (aref (1- x)))))
                          (right (and (< x (1- (length (aref grid 0)))) (-> (aref grid y) (aref (1+ x)))))
                          (up    (and (> y 0) (-> (aref grid (1- y)) (aref x))))
                          (down  (and (< y (1- (length grid))) (-> (aref grid (1+ y)) (aref x)))))
                     (cond
                       
                       ((and left
                             (pipe-connects (-> (aref grid y) (aref x)) left 'left)
                             (or (not (eq from 'left)) (null from)))
                        (recur (1+ acc) (1- x) y 'right))
                       
                       ((and right
                             (pipe-connects (-> (aref grid y) (aref x)) right 'right)
                             (or (not (eq from 'right)) (null from)))
                        (recur (1+ acc) (1+ x) y 'left))
                       
                       ((and up
                             (pipe-connects (-> (aref grid y) (aref x)) up 'up)
                             (or (not (eq from 'up)) (null from)))
                        (recur (1+ acc) x (1- y) 'down))
                       
                       ((and down
                             (pipe-connects (-> (aref grid y) (aref x)) down 'down)
                             (or (not (eq from 'down)) (null from)))
                        (recur (1+ acc) x (1+ y) 'up)))))))
      (recur 0 start-x start-y nil))))

(defun part-1 ()
  (bind ((problem (read-problem)))
    (floor (loop-length problem) 2)))

(defun loop-tiles (grid)
  (bind (((start-x . start-y) (find-start grid)))
    (labels ((recur (acc x y from)
               (if (and from
                        (= x start-x)
                        (= y start-y))
                   acc
                   (bind ((left  (and (> x 0) (-> (aref grid y) (aref (1- x)))))
                          (right (and (< x (1- (length (aref grid 0)))) (-> (aref grid y) (aref (1+ x)))))
                          (up    (and (> y 0) (-> (aref grid (1- y)) (aref x))))
                          (down  (and (< y (1- (length grid))) (-> (aref grid (1+ y)) (aref x)))))
                     (cond
                       
                       ((and left
                             (pipe-connects (-> (aref grid y) (aref x)) left 'left)
                             (or (not (eq from 'left)) (null from)))
                        (recur (with acc (cons x y)) (1- x) y 'right))
                       
                       ((and right
                             (pipe-connects (-> (aref grid y) (aref x)) right 'right)
                             (or (not (eq from 'right)) (null from)))
                        (recur (with acc (cons x y)) (1+ x) y 'left))
                       
                       ((and up
                             (pipe-connects (-> (aref grid y) (aref x)) up 'up)
                             (or (not (eq from 'up)) (null from)))
                        (recur (with acc (cons x y)) x (1- y) 'down))
                       
                       ((and down
                             (pipe-connects (-> (aref grid y) (aref x)) down 'down)
                             (or (not (eq from 'down)) (null from)))
                        (recur (with acc (cons x y)) x (1+ y) 'up)))))))
      (recur (empty-set) start-x start-y nil))))

(defun pipe-edge-connected-to-edge (grid loop)
  (->> (mapcar (lambda (coord)
                 (bind (((x . y) coord))
                   (remove nil
                           (list (and (/= x 0)
                                      (not (contains? loop (cons (1- x) y)))
                                      (not (connects-to-edge grid loop (1- x) y))
                                      (cons coord 'left))
                                 (and (/= x (1- (length (aref grid 0))))
                                      (not (contains? loop (cons (1+ x) y)))
                                      (not (connects-to-edge grid loop (1+ x) y))
                                      (cons coord 'right))
                                 (and (/= y 0)
                                      (not (contains? loop (cons x (1- y))))
                                      (not (connects-to-edge grid loop x (1- y)))
                                      (cons coord 'up))
                                 (and (/= y (1- (length grid)))
                                      (not (contains? loop (cons x (1+ y))))
                                      (not (connects-to-edge grid loop x (1+ y)))
                                      (cons coord 'down))))))
               (convert 'list loop))
    (remove nil)
    car))

(defun not-in-loop (from to x y direction loop)
  (not (contains? loop (cons x y))))

(defun connects-to-edge (grid loop x-in y-in)
  (labels ((recur (to-check seen)
             (if (null to-check)
                 (values t seen)
                 (bind ((((x y from) . rest) to-check)
                        (left  (and (> x 0) (-> (aref grid y) (aref (1- x)))))
                        (right (and (< x (1- (length (aref grid 0)))) (-> (aref grid y) (aref (1+ x)))))
                        (up    (and (> y 0) (-> (aref grid (1- y)) (aref x))))
                        (down  (and (< y (1- (length grid))) (-> (aref grid (1+ y)) (aref x))))
                        (new-seen (with seen (cons x y))))
                   ;; (format t "(cons x y): ~a~%" (cons x y))
                   (if (or (= x 0)
                           (= y 0)
                           (= x (1- (length (aref grid 0))))
                           (= y (1- (length grid))))
                       (values nil (with seen (cons x y)))
                       (recur
                        (concatenate 'list
                                     (remove nil
                                             (list (and left
                                                        (not (contains? seen (cons (1- x) y)))
                                                        (does-not-cross (-> (aref grid y) (aref x))
                                                                        left
                                                                        (1- x)
                                                                        y
                                                                        'left
                                                                        loop)
                                                        (list (1- x) y 'right))
                                                   
                                                   (and right
                                                        (not (contains? seen (cons (1+ x) y)))
                                                        (does-not-cross (-> (aref grid y) (aref x))
                                                                        right
                                                                        (1+ x)
                                                                        y
                                                                        'right
                                                                        loop)
                                                        (list (1+ x) y 'left))
                                                   
                                                   (and up
                                                        (not (contains? seen (cons x (1- y))))
                                                        (does-not-cross (-> (aref grid y) (aref x))
                                                                        up
                                                                        x
                                                                        (1- y)
                                                                        'up
                                                                        loop)
                                                        (list x (1- y) 'down))
                                                   
                                                   (and down
                                                        (not (contains? seen (cons x (1+ y))))
                                                        (does-not-cross (-> (aref grid y) (aref x))
                                                                        down
                                                                        x
                                                                        (1+ y)
                                                                        'down
                                                                        loop)
                                                        (list x (1+ y) 'up))))
                                     rest)
                        new-seen))))))
    (recur (list (list x-in y-in nil))
           (empty-set))))

(defun trace-loop (grid loop edge-connections from start-x start-y)
  (labels ((recur (x y from seen)
             (cond
               ((member (cons (cons x y) from) edge-connections) nil)
               ((and (= x start-x) (= y start-y)) t)
               (t
                (bind ((new-seen (with seen (cons x y)))
                       (from-tile (-> (aref grid y) (aref x)))
                       ((n-x n-y n-dir) (remove nil
                                                (list (and (contains? (cons (1- x) y) loop)
                                                           (not (contains? seen (cons (1- x) y)))
                                                           (list (1- x) y (move-on-pipe from-tile
                                                                                        (-> (aref grid y)
                                                                                          (aref (1- x))))))
                                                      (and (contains? (cons (1+ x) y) loop)
                                                           (not (contains? seen (cons (1+ x) y)))
                                                           (list (1+ x) y (move-on-pipe from-tile
                                                                                        (-> (aref grid y)
                                                                                          (aref (1+ x))))))
                                                      (and (contains? (cons x (1- y)) loop)
                                                           (not (contains? seen (cons x (+ y))))
                                                           (list x (1- y) (move-on-pipe from-tile
                                                                                        (-> (aref grid (1- y))
                                                                                          (aref x)))))
                                                      (and (contains? (cons x (1+ y)) loop)
                                                           (not (contains? seen (cons x (1+ y))))
                                                           (list x (1+ y) (move-on-pipe from-tile
                                                                                        (-> (aref grid (1+ y))
                                                                                          (aref x)))))))))
                  (recur n-x n-y))))))
    (recur start-x start-y from (empty-set))))

(defun part-2 ()
  (bind ((problem (read-problem))
         (outside (empty-set))
         (inside (empty-set))
         (loop (loop-tiles problem))
         (edge-connection (pipe-edge-connected-to-edge problem loop)))
    (print (pipe-edge-connected-to-edge problem loop))
    ;; (iter
    ;;   (for y from 0 below (length problem))
    ;;   (iter
    ;;     (for x from 0 below (length (aref problem 0)))
    ;;     (format t "NEW~%")
    ;;     (when (and (not (contains? inside  (cons x y)))
    ;;                (not (contains? outside (cons x y)))
    ;;                (not (contains? loop (cons x y))))
    ;;       (bind (((:values is-inside tiles) (connects-to-edge problem loop x y)))
    ;;         (if is-inside
    ;;             (setf inside  (union (set-difference inside loop) tiles))
    ;;             (setf outside (union outside tiles)))))))
    ;; (format t "outside: ~a~%" outside)
    ;; (format t "inside: ~a~%" inside)
    ;; (size inside)
    ))
