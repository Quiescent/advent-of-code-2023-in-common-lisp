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
      (recur (set (cons start-x start-y)) start-x start-y nil))))

(defun pipe-edge-connected-to-edge (grid loop)
  (->> (mapcar (lambda (coord)
                 (bind (((x . y) coord))
                   (remove nil
                           (list (and (not (contains? loop (cons (1- x) y)))
                                      (or (= x 0)
                                          (not (connects-to-edge grid loop (1- x) y)))
                                      (cons coord #c(-1 0)))
                                 (and (not (contains? loop (cons (1+ x) y)))
                                      (or (= x (1- (length (aref grid 0))))
                                          (not (connects-to-edge grid loop (1+ x) y)))
                                      (cons coord #c(1 0)))
                                 (and (not (contains? loop (cons x (1- y))))
                                      (or (= y 0)
                                          (not (connects-to-edge grid loop x (1- y))))
                                      (cons coord #c(0 -1)))
                                 (and (not (contains? loop (cons x (1+ y))))
                                      (or (= y (1- (length grid)))
                                          (not (connects-to-edge grid loop x (1+ y))))
                                      (cons coord #c(0 1)))))))
               (convert 'list loop))
    (remove nil)
    car))

(defun not-in-loop (x y loop)
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
                                                        (not-in-loop (1- x) y loop)
                                                        (list (1- x) y 'right))
                                                   
                                                   (and right
                                                        (not (contains? seen (cons (1+ x) y)))
                                                        (not-in-loop (1+ x) y loop)
                                                        (list (1+ x) y 'left))
                                                   
                                                   (and up
                                                        (not (contains? seen (cons x (1- y))))
                                                        (not-in-loop x (1- y) loop)
                                                        (list x (1- y) 'down))
                                                   
                                                   (and down
                                                        (not (contains? seen (cons x (1+ y))))
                                                        (not-in-loop x (1+ y) loop)
                                                        (list x (1+ y) 'up))))
                                     rest)
                        new-seen))))))
    (recur (list (list x-in y-in nil))
           (empty-set))))

(defun clockwise-90 (x)
  (* x #c(0 1)))

(defun anticlockwise-90 (x)
  (* x #c(0 -1)))

(defun move-on-pipe (next-tile move dir)
  (case next-tile
    (#\| dir)
    (#\- dir)
    (#\L (case move
           (left (clockwise-90 dir))
           (right (error "invalid"))
           (up (error "invalid"))
           (down (anticlockwise-90 dir))))
    (#\J (case move
           (left (error "invalid"))
           (right (anticlockwise-90 dir))
           (up (error "invalid"))
           (down (clockwise-90 dir))))
    (#\7 (case move
           (left (error "invalid"))
           (right (clockwise-90 dir))
           (up (anticlockwise-90 dir))
           (down (error "invalid"))))
    (#\F (case move
           (left (anticlockwise-90 dir))
           (right (error "invalid"))
           (up (clockwise-90 dir))
           (down (error "invalid"))))))

(defvar on-outside-edge nil)

(defun trace-loop (grid loop outside-edges start-dir start-x start-y)
  (labels ((recur (x y dir prev-x prev-y)
             (cond
               ((and prev-x prev-y (= x start-x) (= y start-y)) nil)
               ((contains? outside-edges (cons (cons x y) dir)) t)
               (t
                (bind ((from (-> (aref grid y) (aref x)))
                       (next (car (remove nil
                                          (list (and (contains? loop (cons (1- x) y))
                                                     (not (and (eq (1- x) prev-x)
                                                               (eq y prev-y)))
                                                     (pipe-connects from
                                                                    (-> (aref grid y)
                                                                      (aref (1- x)))
                                                                    'left)
                                                     (list (1- x) y (move-on-pipe (-> (aref grid y)
                                                                                    (aref (1- x)))
                                                                                  'left
                                                                                  dir)))
                                                (and (contains? loop (cons (1+ x) y))
                                                     (not (and (eq (1+ x) prev-x)
                                                               (eq y prev-y)))
                                                     (pipe-connects from
                                                                    (-> (aref grid y)
                                                                      (aref (1+ x)))
                                                                    'right)
                                                     (list (1+ x) y (move-on-pipe (-> (aref grid y)
                                                                                    (aref (1+ x)))
                                                                                  'right
                                                                                  dir)))
                                                (and (contains? loop (cons x (1- y)))
                                                     (not (and (eq x  prev-x)
                                                               (eq (1- y) prev-y)))
                                                     (pipe-connects from
                                                                    (-> (aref grid (1- y))
                                                                      (aref x))
                                                                    'up)
                                                     (list x (1- y) (move-on-pipe (-> (aref grid (1- y))
                                                                                    (aref x))
                                                                                  'up
                                                                                  dir)))
                                                (and (contains? loop (cons x (1+ y)))
                                                     (not (and (eq x prev-x)
                                                               (eq (1+ y) prev-y)))
                                                     (pipe-connects from
                                                                    (-> (aref grid (1+ y))
                                                                      (aref x))
                                                                    'down)
                                                     (list x (1+ y) (move-on-pipe (-> (aref grid (1+ y))
                                                                                    (aref x))
                                                                                  'down
                                                                                  dir)))))) ))
                  (if next
                      (bind (((n-x n-y n-dir) next))
                        (recur n-x n-y n-dir x y))
                      nil))))))
    (or (gethash (list start-x start-y start-dir) on-outside-edge)
        (bind ((result (recur start-x start-y start-dir nil nil)))
          (if result
              (progn
                (setf (gethash (list start-x start-y start-dir) on-outside-edge) t)
                result)
              nil)))))

(defun initial-direction (entry-direction tile)
  (case entry-direction
    (left
     (case tile
       (#\| #c(1 0))
       (#\- (error "invalid"))
       (#\L (error "invalid"))
       (#\J #c(0 1))
       (#\7 #c(0 -1))
       (#\F (error "invalid"))))
    (right
     (case tile
       (#\| #c(-1 0))
       (#\- (error "invalid"))
       (#\L #c(0 1))
       (#\J (error "invalid"))
       (#\7 (error "invalid"))
       (#\F #c(0 -1))))
    (up
     (case tile
       (#\| (error "invalid"))
       (#\- #c(0 1))
       (#\L #c(0 1))
       (#\J #c(0 1))
       (#\7 (error "invalid"))
       (#\F (error "invalid"))))
    (down
     (case tile
       (#\| (error "invalid"))
       (#\- #c(0 -1))
       (#\L (error "invalid"))
       (#\J (error "invalid"))
       (#\7 #c(0 -1))
       (#\F #c(0 -1))))))

(defun check-inside-with-squeezes (grid loop outside-edges inside outside x-in y-in)
  (labels ((recur (to-check seen)
             (if (empty? to-check)
                 (values t seen)
                 (bind ((next (first to-check))
                        ((x . y) next)
                        (new-seen (with seen (cons x y))))
                   (cond
                     ((contains? inside next)  (values t seen))
                     ((contains? outside next) (values nil seen))
                     (t (bind ((rest (less-first to-check))
                               (left  (and (> x 0) (-> (aref grid y) (aref (1- x)))))
                               (right (and (< x (1- (length (aref grid 0)))) (-> (aref grid y) (aref (1+ x)))))
                               (up    (and (> y 0) (-> (aref grid (1- y)) (aref x))))
                               (down  (and (< y (1- (length grid))) (-> (aref grid (1+ y)) (aref x)))))
                          (if (or (= x 0)
                                  (= y 0)
                                  (= x (1- (length (aref grid 0))))
                                  (= y (1- (length grid))))
                              (values nil (with seen (cons x y)))
                              (cond
                                ((and left (contains? loop (cons (1- x) y)))
                                 (values (not (trace-loop grid
                                                          loop
                                                          outside-edges
                                                          (initial-direction 'left
                                                                             (-> (aref grid y)
                                                                               (aref (1- x))))
                                                          (1- x)
                                                          y))
                                         new-seen))
                                ((and right (contains? loop (cons (1+ x) y)))
                                 (values (not (trace-loop grid
                                                          loop
                                                          outside-edges
                                                          (initial-direction 'right
                                                                             (-> (aref grid y)
                                                                               (aref (1+ x))))
                                                          (1+ x)
                                                          y))
                                         new-seen))
                                ((and up (contains? loop (cons x (1- y))))
                                 (values (not (trace-loop grid
                                                          loop
                                                          outside-edges
                                                          (initial-direction 'up
                                                                             (-> (aref grid (1- y))
                                                                               (aref x)))
                                                          x
                                                          (1- y)))
                                         new-seen))
                                ((and down (contains? loop (cons x (1+ y))))
                                 (values (not (trace-loop grid
                                                          loop
                                                          outside-edges
                                                          (initial-direction 'down
                                                                             (-> (aref grid (1+ y))
                                                                               (aref x)))
                                                          x
                                                          (1+ y)))
                                         new-seen))
                                (t (recur
                                    (concat rest
                                            (remove nil
                                                    (seq (and left
                                                              (not (contains? seen (cons (1- x) y)))
                                                              (not-in-loop (1- x) y loop)
                                                              (cons (1- x) y))
                                                         
                                                         (and right
                                                              (not (contains? seen (cons (1+ x) y)))
                                                              (not-in-loop (1+ x) y loop)
                                                              (cons (1+ x) y))
                                                         
                                                         (and up
                                                              (not (contains? seen (cons x (1- y))))
                                                              (not-in-loop x (1- y) loop)
                                                              (cons x (1- y)))
                                                         
                                                         (and down
                                                              (not (contains? seen (cons x (1+ y))))
                                                              (not-in-loop x (1+ y) loop)
                                                              (cons x (1+ y))))))
                                    new-seen)))))))))))
    (recur (seq (cons x-in y-in))
           (empty-set))))

(defun replace-start (grid)
  (bind (((x . y) (find-start grid))
         (left  (and (> x 0) (-> (aref grid y) (aref (1- x)))))
         (right (and (< x (1- (length (aref grid 0)))) (-> (aref grid y) (aref (1+ x)))))
         (up    (and (> y 0) (-> (aref grid (1- y)) (aref x))))
         (down  (and (< y (1- (length grid))) (-> (aref grid (1+ y)) (aref x)))))
    (cond
      ((and left (pipe-connects #\S left 'left))
       (cond
         ((and right (pipe-connects #\S right 'right))
          (setf (aref (aref grid y) x) #\-))
         ((and up (pipe-connects #\S up 'up))
          (setf (aref (aref grid y) x) #\J))
         ((and down (pipe-connects #\S down 'down))
          (setf (aref (aref grid y) x) #\7))))
      ((and right (pipe-connects #\S right 'right))
       (cond
         ((and up (pipe-connects #\S up 'up))
          (setf (aref (aref grid y) x) #\L))
         ((and down (pipe-connects #\S down 'down))
          (setf (aref (aref grid y) x) #\F))))
      ((and up (pipe-connects #\S up 'up)
            down (pipe-connects #\S down 'down))
       (setf (aref (aref grid y) x) #\|)))))

(defun part-2 ()
  (bind ((problem (read-problem))
         (loop (loop-tiles problem))
         (on-outside-edge (make-hash-table :test #'equal))
         (outside (empty-set))
         (inside (empty-set))
         (edge-connections (convert 'set (pipe-edge-connected-to-edge problem loop))))
    (replace-start problem)
    (iter
      (for y from 0 below (length problem))
      (iter
        (for x from 0 below (length (aref problem 0)))
        (format t "(cons x y): ~a~%" (cons x y))
        (when (and (not (contains? inside  (cons x y)))
                   (not (contains? outside (cons x y)))
                   (not (contains? loop (cons x y))))
          (bind (((:values is-inside tiles) (check-inside-with-squeezes problem
                                                                        loop
                                                                        edge-connections
                                                                        inside
                                                                        outside
                                                                        x
                                                                        y)))
            (if is-inside
                (setf inside  (union (set-difference inside loop) tiles))
                (setf outside (union (set-difference outside loop) tiles)))))))
    (format t "inside: ~a~%" inside)
    (size inside)))
