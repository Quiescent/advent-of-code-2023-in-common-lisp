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
                                      (cons coord #c(-1 0)))
                                 (and (/= x (1- (length (aref grid 0))))
                                      (not (contains? loop (cons (1+ x) y)))
                                      (not (connects-to-edge grid loop (1+ x) y))
                                      (cons coord #c(1 0)))
                                 (and (/= y 0)
                                      (not (contains? loop (cons x (1- y))))
                                      (not (connects-to-edge grid loop x (1- y)))
                                      (cons coord #c(0 -1)))
                                 (and (/= y (1- (length grid)))
                                      (not (contains? loop (cons x (1+ y))))
                                      (not (connects-to-edge grid loop x (1+ y)))
                                      (cons coord #c(0 1)))))))
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
                                                        (not-in-loop (-> (aref grid y) (aref x))
                                                                        left
                                                                        (1- x)
                                                                        y
                                                                        'left
                                                                        loop)
                                                        (list (1- x) y 'right))
                                                   
                                                   (and right
                                                        (not (contains? seen (cons (1+ x) y)))
                                                        (not-in-loop (-> (aref grid y) (aref x))
                                                                        right
                                                                        (1+ x)
                                                                        y
                                                                        'right
                                                                        loop)
                                                        (list (1+ x) y 'left))
                                                   
                                                   (and up
                                                        (not (contains? seen (cons x (1- y))))
                                                        (not-in-loop (-> (aref grid y) (aref x))
                                                                        up
                                                                        x
                                                                        (1- y)
                                                                        'up
                                                                        loop)
                                                        (list x (1- y) 'down))
                                                   
                                                   (and down
                                                        (not (contains? seen (cons x (1+ y))))
                                                        (not-in-loop (-> (aref grid y) (aref x))
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

;; (defun move-on-pipe (from-tile to-tile dir)
;;   (case from-tile
;;     (#\| (case to-tile
;;            (#\| dir)
;;            (#\- (error "invalid"))
;;            (#\L (case dir
;;                   (left 'down)
;;                   (right 'up)))
;;            (#\J (case dir
;;                   (left 'up)
;;                   (right 'down)))
;;            (#\7 (case dir
;;                   (left 'down)
;;                   (right 'up)))
;;            (#\F (case dir
;;                   (left 'up)
;;                   (right 'down)))))
;;     (#\- (case to-tile
;;            (#\| (error "invalid"))
;;            (#\- dir)
;;            (#\L (error "invalid"))
;;            (#\J (case dir
;;                   (up 'left)
;;                   (down 'right)))
;;            (#\7 (case dir
;;                   (up 'left)
;;                   (down 'right)))
;;            (#\F (case dir
;;                   (up 'left)
;;                   (down 'right)))))
;;     (#\L (case to-tile
;;            (#\| (error "invalid"))
;;            (#\- (or (case dir
;;                       (left 'down)
;;                       (right 'up))
;;                     dir))
;;            (#\L (error "invalid"))
;;            (#\J (case dir
;;                   (up 'left)
;;                   (down 'right)
;;                   (left 'right)
;;                   (right 'left)))
;;            (#\7 (or (case dir
;;                       (up 'right)
;;                       (down 'left))
;;                     dir))
;;            (#\F (error "invalid"))))
;;     (#\J (case to-tile
;;            (#\| (or (case dir
;;                       (up 'left)
;;                       (down 'right))
;;                     dir))
;;            (#\- (error "invalid"))
;;            (#\L (error "invalid"))
;;            (#\J (error "invalid"))
;;            (#\7 (case dir
;;                   (left 'down)
;;                   (right 'up)
;;                   (up 'down)
;;                   (down 'up)))
;;            (#\F (or (case dir
;;                       (up 'left)
;;                       (down 'right))
;;                     dir))))
;;     (#\7 (case to-tile
;;            (#\| dir)
;;            (#\- (error "invalid"))
;;            (#\L (or (case dir
;;                       (left 'down)
;;                       (right 'up))
;;                     dir))
;;            (#\J (case dir
;;                   (left 'up)
;;                   (right 'down)
;;                   (up 'down)
;;                   (down 'up)))
;;            (#\7 (error "invalid"))
;;            (#\F (error "invalid"))))
;;     (#\F (case to-tile
;;            (#\| dir)
;;            (#\- (error "invalid"))
;;            (#\L (case dir
;;                   (up 'down)
;;                   (down 'up)
;;                   (right 'up)
;;                   (left 'down)))
;;            (#\J (or (case dir
;;                       (right 'down)
;;                       (left 'up))
;;                     dir))
;;            (#\7 (error "invalid"))
;;            (#\F (error "invalid"))))))

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

(defun trace-loop (grid loop edge-connections start-dir start-x start-y)
  ;; (print "NEW")
  (labels ((recur (x y dir seen)
             ;; (format t "(list x y dir): ~a~%" (list x y dir))
             (cond
               ((member (cons (cons x y) dir) edge-connections :test #'equal) nil)
               (t
                (bind ((new-seen (with seen (cons x y)))
                       (from (-> (aref grid y) (aref x)))
                       (next (car
                              (remove nil
                                      (list (and (contains? loop (cons (1- x) y))
                                                 (not (contains? seen (cons (1- x) y)))
                                                 (pipe-connects from
                                                                (-> (aref grid y)
                                                                  (aref (1- x)))
                                                                'left)
                                                 (list (1- x) y (move-on-pipe (-> (aref grid y)
                                                                                (aref (1- x)))
                                                                              'left
                                                                              dir)))
                                            (and (contains? loop (cons (1+ x) y))
                                                 (not (contains? seen (cons (1+ x) y)))
                                                 (pipe-connects from
                                                                (-> (aref grid y)
                                                                  (aref (1+ x)))
                                                                'right)
                                                 (list (1+ x) y (move-on-pipe (-> (aref grid y)
                                                                                (aref (1+ x)))
                                                                              'right
                                                                              dir)))
                                            (and (contains? loop (cons x (1- y)))
                                                 (not (contains? seen (cons x (1- y))))
                                                 (pipe-connects from
                                                                (-> (aref grid (1- y))
                                                                  (aref x))
                                                                'up)
                                                 (list x (1- y) (move-on-pipe (-> (aref grid (1- y))
                                                                                (aref x))
                                                                              'up
                                                                              dir)))
                                            (and (contains? loop (cons x (1+ y)))
                                                 (not (contains? seen (cons x (1+ y))))
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
                        (recur n-x n-y n-dir new-seen))
                      t))))))
    (recur start-x start-y start-dir (empty-set))))

(defun check-inside-with-squeezes (grid loop edge-connections x-in y-in)
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
                       (cond
                         ((and left
                               (not (contains? seen (cons (1- x) y)))
                               (contains? loop (cons (1- x) y)))
                          (values (trace-loop grid
                                              loop
                                              edge-connections
                                              #c(1 0)
                                              (1- x)
                                              y)
                                  new-seen))
                         ((and right
                               (not (contains? seen (cons (1+ x) y)))
                               (contains? loop (cons (1+ x) y)))
                          (values (trace-loop grid
                                              loop
                                              edge-connections
                                              #c(-1 0)
                                              (1+ x)
                                              y)
                                  new-seen))
                         ((and up
                               (not (contains? seen (cons x (1- y))))
                               (contains? loop (cons x (1- y))))
                          (values (trace-loop grid
                                              loop
                                              edge-connections
                                              #c(0 1)
                                              x
                                              (1- y)) new-seen))
                         ((and down
                               (not (contains? seen (cons x (1+ y))))
                               (contains? loop (cons x (1+ y))))
                          (values (trace-loop grid
                                              loop
                                              edge-connections
                                              #c(0 -1)
                                              x
                                              (1+ y))
                                  new-seen))
                         (t (recur
                             (concatenate 'list
                                          (remove nil
                                                  (list (and left
                                                             (not (contains? seen (cons (1- x) y)))
                                                             (not-in-loop (-> (aref grid y) (aref x))
                                                                          left
                                                                          (1- x)
                                                                          y
                                                                          'left
                                                                          loop)
                                                             (list (1- x) y 'right))
                                                   
                                                        (and right
                                                             (not (contains? seen (cons (1+ x) y)))
                                                             (not-in-loop (-> (aref grid y) (aref x))
                                                                          right
                                                                          (1+ x)
                                                                          y
                                                                          'right
                                                                          loop)
                                                             (list (1+ x) y 'left))
                                                   
                                                        (and up
                                                             (not (contains? seen (cons x (1- y))))
                                                             (not-in-loop (-> (aref grid y) (aref x))
                                                                          up
                                                                          x
                                                                          (1- y)
                                                                          'up
                                                                          loop)
                                                             (list x (1- y) 'down))
                                                   
                                                        (and down
                                                             (not (contains? seen (cons x (1+ y))))
                                                             (not-in-loop (-> (aref grid y) (aref x))
                                                                          down
                                                                          x
                                                                          (1+ y)
                                                                          'down
                                                                          loop)
                                                             (list x (1+ y) 'up))))
                                          rest)
                             new-seen))))))))
    (recur (list (list x-in y-in nil))
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
         (outside (empty-set))
         (inside (empty-set))
         (edge-connection (pipe-edge-connected-to-edge problem loop)))
    (replace-start problem)
    (print edge-connection)
    ;; (trace-loop problem loop edge-connection #c(-1 0) 2 3)
    ;; (bind (((:values is-inside tiles) (check-inside-with-squeezes problem loop edge-connection 4 3)))
    ;;   (format t "(list is-inside tiles): ~a~%" (list is-inside tiles)))
    (iter
      (for y from 0 below (length problem))
      (iter
        (for x from 0 below (length (aref problem 0)))
        (format t "(cons x y): ~a~%" (cons x y))
        ;; (format t "NEW~%")
        (when (and (not (contains? inside  (cons x y)))
                   (not (contains? outside (cons x y)))
                   (not (contains? loop (cons x y))))
          (bind (((:values is-inside tiles) (check-inside-with-squeezes problem loop edge-connection x y)))
            (if is-inside
                (setf inside  (union (set-difference inside loop) tiles))
                (setf outside (union (set-difference outside loop) tiles)))))))
    (format t "outside: ~a~%" outside)
    (format t "inside: ~a~%" inside)
    (size inside)))
