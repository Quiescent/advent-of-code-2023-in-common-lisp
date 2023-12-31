(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-24
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-24)

(defstruct coord
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defstruct velocity
  (vx 0 :type fixnum)
  (vy 0 :type fixnum)
  (vz 0 :type fixnum))

(defstruct hail-stone
  (coord (make-coord :x 0 :y 0 :z 0) :type coord)
  (velocity (make-velocity :vx 0 :vy 0 :vz 0) :type velocity))

(defun parse-line (line)
  (bind (((pos vel) (split " @ " line))
         ((x y z) (->> (split ", " pos)
                    (mapcar #'read-from-string)))
         ((vx vy vz) (->> (split ", " vel)
                       (mapcar #'read-from-string))))
    (make-hail-stone
     :coord (make-coord
             :x x
             :y y
             :z z)
     :velocity (make-velocity
                :vx vx
                :vy vy
                :vz vz))))

(defun read-problem (file-relative-path)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp file-relative-path))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (nreverse acc)
                     (recur (cons (parse-line line)
                                  acc))))))
      (recur nil))))

(defun vars (vx cx vy cy)
  (bind ((c (- (* cx vy)
               (* cy vx)))
         (a vy)
         (b (- vx)))
    (list a b c)))

;; No intersection: a1b2 - a2b1= 0

;; Top term x: c1b2 - b1c2
;; Bottom term x: a1b2 - b1a2

;; Top term y: a1c2 - c1a2
;; Bottom term y: a1b2 - b1a2

(defun line-intersection (vx1 x1 vx2 x2 vy1 y1 vy2 y2)
  (bind (((a1 b1 c1) (vars vx1 x1 vy1 y1))
         ((a2 b2 c2) (vars vx2 x2 vy2 y2)))
    (if (= 0 (- (* a1 b2)
                (* a2 b1)))
        nil
        (cons (/ (- (* c1 b2)
                    (* b1 c2))
                 (- (* a1 b2)
                    (* b1 a2)))
              (/ (- (* a1 c2)
                    (* c1 a2))
                 (- (* a1 b2)
                    (* b1 a2)))))))

(defun in-test-area (c)
  (and (>= c 200000000000000)
       (<= c 400000000000000))
  ;; (and (>= c 7)
  ;;      (<= c 27))
  )

(defmethod collides-in-area ((s1 hail-stone) (s2 hail-stone))
  (with-slots (coord velocity) s1
    (with-slots (x y) coord
      (with-slots (vx vy) velocity
        (bind ((o-coord (hail-stone-coord s2))
               (o-x (coord-x o-coord))
               (o-y (coord-y o-coord))
               (o-velocity (hail-stone-velocity s2))
               (o-vx (velocity-vx o-velocity))
               (o-vy (velocity-vy o-velocity))
               (i-point (line-intersection vx x o-vx o-x
                                           vy y o-vy o-y)))
          (and i-point
               (bind (((x-coll . y-coll) i-point)
                      (time-1 (floor (/ (- x-coll x) vx)))
                      (time-2 (floor (/ (- x-coll o-x) o-vx))))
                 (and (> time-1 0)
                      (> time-2 0)
                      (in-test-area x-coll)
                      (in-test-area y-coll)))))))))

(defun count-collisions (hail-stones)
  (iter
    (for stones on hail-stones)
    (for hail-stone = (car stones))
    (summing
     (iter
       (for other-hail-stone in (cdr stones))
       (counting (collides-in-area hail-stone other-hail-stone))))))

(defun part-1 (&optional (file-relative-path "src/day-24.in"))
  (bind ((hail-stones (read-problem file-relative-path)))
    (count-collisions hail-stones)))

;; Too high: 19651

(defun test-1 ()
  (part-1 "src/day-24-test.in"))

(defun intersect-3d (x1 y1 z1 vx1 vy1 vz1
                     x2 y2 z2 vx2 vy2 vz2)
  (bind ((intersection-xy (line-intersection vx1 x1 vx2 x2 vy1 y1 vy2 y2)))
    (when intersection-xy
      (bind (((x . y) intersection-xy)
             (t1 (/ (- x x1) vx1))
             (t2 (/ (- x x2) vx2))
             (z-col-1 (+ z1 (* vz1 t1)))
             (z-col-2 (+ z2 (* vz2 t2))))
        (when (and (> t1 0)
                   (= (floor t1) t1)
                   (> t2 0)
                   (= (floor t2) t2)
                   (= z-col-1 z-col-2))
          (list x y z-col-1))))))

(defun all-collide (hail-stones vx-delta vy-delta vz-delta)
  (bind ((collision-point)
         (first-stone (aref hail-stones 0))
         ((x1 y1 z1 vx1 vy1 vz1)
          (with-slots (coord velocity) first-stone
            (with-slots (x y z) coord
              (with-slots (vx vy vz) velocity
                (list x y z vx vy vz))))))
    (iter
      (for stone in-vector hail-stones from 1)
      (for collision = (with-slots (coord velocity) stone
                         (with-slots (x y z) coord
                           (with-slots (vx vy vz) velocity
                             (intersect-3d x1 y1 z1
                                           (- vx1 vx-delta)
                                           (- vy1 vy-delta)
                                           (- vz1 vz-delta)
                                           x y z
                                           (- vx vx-delta)
                                           (- vy vy-delta)
                                           (- vz vz-delta))))))
      (when (null collision-point)
        (setf collision-point collision))
      (always (equal collision collision-point))
      (finally (return collision-point)))))

(defun all-collide-xy (hail-stones vx-delta vy-delta)
  (bind ((collision-point)
         (first-stone (aref hail-stones 0))
         ((x1 y1 vx1 vy1)
          (with-slots (coord velocity) first-stone
            (with-slots (x y) coord
              (with-slots (vx vy) velocity
                (list x y vx vy))))))
    (iter
      (for stone in-vector hail-stones from 1)
      (for collision = (with-slots (coord velocity) stone
                         (with-slots (x y) coord
                           (with-slots (vx vy) velocity
                             (line-intersection (- vx1 vx-delta)
                                                x1
                                                (- vx vx-delta)
                                                x
                                                (- vy1 vy-delta)
                                                y1
                                                (- vy vy-delta)
                                                y)))))
      (when (null collision-point)
        (setf collision-point collision))
      (always (equal collision collision-point))
      (finally (return collision-point)))))

(defun intersect-z (x-col y-col
                    x1 vx1 x2 vx2
                    y1 vy1 y2 vy2
                    z1 vz1 z2 vz2)
  (bind ((t1 (or (and (/= vx1 0) (/ (- x-col x1) vx1))
                 (and (/= vy1 0) (/ (- y-col y1) vy1))))
         (t2 (or (and (/= vx2 0) (/ (- x-col x2) vx2))
                 (and (/= vy2 0) (/ (- y-col y2) vy2))))
         (z-col-1 (+ z1 (* vz1 t1)))
         (z-col-2 (+ z2 (* vz2 t2))))
    (when (and (> t1 0)
               (= (floor t1) t1)
               (> t2 0)
               (= (floor t2) t2)
               (= z-col-1 z-col-2))
      (list x-col y-col z-col-1))))

(defun all-collide-z (hail-stones x-col y-col vx-delta vy-delta vz-delta)
  (bind ((collision-point)
         (first-stone (aref hail-stones 0))
         ((x1 y1 z1 vx1 vy1 vz1)
          (with-slots (coord velocity) first-stone
            (with-slots (x y z) coord
              (with-slots (vx vy vz) velocity
                (list x y z vx vy vz))))))
    (iter
      (for stone in-vector hail-stones from 1)
      (for collision = (with-slots (coord velocity) stone
                         (with-slots (x y z) coord
                           (with-slots (vx vy vz) velocity
                             (intersect-z x-col y-col
                                          x1    (- vx1 vx-delta)
                                          x     (- vx  vx-delta)
                                          y1    (- vy1 vy-delta)
                                          y     (- vy  vy-delta)
                                          z1    (- vz1 vz-delta)
                                          z     (- vz  vz-delta))))))
      (when (null collision-point)
        (setf collision-point collision))
      (always (equal collision collision-point))
      (finally (return collision-point)))))

(defun scan-velocities-for-all-collision (hail-stones-list)
  (bind ((hail-stones (coerce hail-stones-list 'vector)))
    (iter outer
      (for vx from -400 to 400)
      (iter
        (for vy from -400 to 400)
        (for collision-xy = (all-collide-xy hail-stones vx vy))
        (when (not collision-xy)
          (next-iteration))
        (for (x . y) = collision-xy)
        (iter
          (for vz from -400 to 400)
          (for collision-point = (all-collide-z hail-stones x y vx vy vz))
          (in outer (finding collision-point such-that collision-point)))))))

(defun part-2 (&optional (file-relative-path "src/day-24.in"))
  (bind ((hail-stones (read-problem file-relative-path)))
    (apply #'+ (scan-velocities-for-all-collision hail-stones))))

(defun test-2 ()
  (part-2 "src/day-24-test.in"))
