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
          (when (not i-point)
            (format t "No collision~%~%"))
          (and i-point
               (bind (((x-coll . y-coll) i-point)
                      (time-1 (floor (/ (- x-coll x) vx)))
                      (time-2 (floor (/ (- x-coll o-x) o-vx))))
                 (format t "x-coll: ~a~%" (coerce x-coll 'float))
                 (format t "y-coll: ~a~%" (coerce y-coll 'float))
                 (format t "time: ~a~%" time-1)
                 (format t "time: ~a~%" time-2)
                 (format t "in: ~a~%" (and (> time-1 0)
                                           (> time-2 0)
                                           (in-test-area x-coll)
                                           (in-test-area y-coll)))
                 (format t "~%" )
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

;; Idea: The stone velocity must divide into the components of all the
;; hail stones.  So I need to find the lowest prime factor of their
;; velocities shared by all stones.

(defun prime-factors (x)
  (iter
    (for i from 2 to x)
    (when (= (mod x i) 0)
      (collecting i))))

;; Might need to acconut for offset of hailstone.
(defun find-velocity (hail-stones)
  (iter
    (for stone in hail-stones)
    (with-slots (velocity) stone
      (with-slots (vx vy vz) velocity
        (collecting vx into vxs)
        (collecting vy into vys)
        (collecting vz into vzs)))
    (finally
     (return (make-velocity
              :vx (print (apply #'lcm vxs))
              :vy (print (apply #'lcm vys))
              :vz (print (apply #'lcm vzs)))))))

(defun find-start (hail-stones)
  (find-velocity hail-stones))

(defun part-2 (&optional (file-relative-path "src/day-24.in"))
  (bind ((hail-stones (read-problem file-relative-path)))
    (find-start hail-stones)))

(defun test-2 ()
  (part-2 "src/day-24-test.in"))
