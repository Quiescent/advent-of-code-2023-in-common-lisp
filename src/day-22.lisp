(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
     :fset
      ,@(let ((symbols nil))
        (do-external-symbols (symbol (find-package 'fset) symbols)
          (push symbol symbols))))))

(defpackage day-22
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package day-22)

(defstruct 3d-coord
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defstruct brick
  (start (make-3d-coord :x 0 :y 0 :z 0) :type 3d-coord)
  (end (make-3d-coord :x 0 :y 0 :z 0) :type 3d-coord)
  (label 0 :type fixnum))

(defun parse-line (line i)
  (match line
    ((ppcre "([0-9]+),([0-9]+),([0-9]+)~([0-9]+),([0-9]+),([0-9]+)"
            (read x1)
            (read y1)
            (read z1)
            (read x2)
            (read y2)
            (read z2))
     (make-brick
      :start (make-3d-coord :x x1
                            :y y1
                            :z z1)
      :end (make-3d-coord :x x2
                          :y y2
                          :z z2)
      :label i))))

(defun read-problem (relative-path-name)
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2023-in-common-lisp relative-path-name))
    (labels ((recur (acc i)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (cons (parse-line line i)
                                  acc)
                            (1+ i))))))
      (recur nil 1))))

(defmethod place-brick ((b brick) map)
  (with-slots (start end) b
    (with-slots (x y z) start
      (bind ((x2 (3d-coord-x end))
             (y2 (3d-coord-y end))
             (z2 (3d-coord-z end)))
        (iter
          (for xi from (min x x2) to (max x x2))
          (iter
            (for yi from (min y y2) to (max y y2))
            (iter
              (for zi from (min z z2) to (max z z2))
              (setf (gethash (list xi yi zi) map) b))))))))

(defmethod remove-brick ((b brick) map)
  (with-slots (start end) b
    (with-slots (x y z) start
      (bind ((x2 (3d-coord-x end))
             (y2 (3d-coord-y end))
             (z2 (3d-coord-z end)))
        (iter
          (for xi from (min x x2) to (max x x2))
          (iter
            (for yi from (min y y2) to (max y y2))
            (iter
              (for zi from (min z z2) to (max z z2))
              (remhash (list xi yi zi) map))))))))

(defun place-bricks (bricks)
  (bind ((grid (make-hash-table :test #'equal)))
    (iter
      (for brick in bricks)
      (place-brick brick grid))
    grid))

(defun low-points (brick)
  (with-slots (start end) brick
    (with-slots (x y z) start
      (bind ((x2 (3d-coord-x end))
             (y2 (3d-coord-y end))
             (z2 (3d-coord-z end))
             (min-z (min z z2)))
        (iter outer
          (for xi from (min x x2) to (max x x2))
          (iter
            (for yi from (min y y2) to (max y y2))
            (in outer (collecting (make-3d-coord :x xi :y yi :z min-z)))))))))

(defmethod point-one-down ((p 3d-coord))
  (with-slots (x y z) p
    (make-3d-coord :x x :y y :z (max 1 (1- z)))))

(defmethod point-one-up ((p 3d-coord))
  (with-slots (x y z) p
    (make-3d-coord :x x :y y :z (1+ z))))

(defmethod point-to-list ((p 3d-coord))
  (with-slots (x y z) p
    (list x y z)))

(defmethod no-brick-below ((p 3d-coord) map)
  (and (/= (3d-coord-z p) 1)
       (not (gethash (point-to-list (point-one-down p)) map))))

(defun fall-one-step (map bricks)
  (bind ((bricks-that-fell nil))
    (iter
      (for brick in bricks)
      (when (member brick bricks-that-fell)
        (next-iteration))
      (for low-points = (low-points brick))
      (for can-fall = (every (lambda (point)
                               (no-brick-below point map))
                             low-points))
      (when can-fall
        (push brick bricks-that-fell)
        (remove-brick brick map)
        (setf (brick-start brick) (point-one-down (brick-start brick))
              (brick-end brick) (point-one-down (brick-end brick)))
        (place-brick brick map)))
    bricks-that-fell))

(defun settle-bricks (map bricks)
  (iter
    (for settled = (fall-one-step map bricks))
    (while settled)))

(defmethod more-than-one-below ((b brick) map)
  (with-slots (start end) b
    (with-slots (x y z) start
      (bind ((x2 (3d-coord-x end))
             (y2 (3d-coord-y end))
             (z2 (3d-coord-z end))
             (min-z (min z z2))
             (found (make-hash-table)))
        (> (hash-table-count
            (iter
              (for xi from (min x x2) to (max x x2))
              (iter
                (for yi from (min y y2) to (max y y2))
                (for new-point = (list xi yi (1- min-z)))
                (for brick-at-point = (gethash new-point map))
                (when brick-at-point
                  (setf (gethash brick-at-point found) t)))
              (finally (return found))))
           1)))))

(defmethod brick-can-disintegrate ((b brick) map bricks)
  (with-slots (start end) b
    (with-slots (x y z) start
      (bind ((x2 (3d-coord-x end))
             (y2 (3d-coord-y end))
             (z2 (3d-coord-z end))
             (max-z (max z z2)))
        (iter outer
          (for xi from (min x x2) to (max x x2))
          (iter
            (for yi from (min y y2) to (max y y2))
            (for new-point = (list xi yi (1+ max-z)))
            (when (and (gethash new-point map)
                       (not (more-than-one-below (gethash new-point map) map)))
              (return-from outer nil)))
          (finally (return-from outer t)))))))

(defun part-1 (&optional (relative-path-name "src/day-22.in"))
  (bind ((problem (read-problem relative-path-name))
         (map (place-bricks problem)))
    (settle-bricks map problem)
    (iter
      (for brick in problem)
      (counting (brick-can-disintegrate brick map problem)))))

(defun test-1 ()
  (part-1 "src/day-22-test.in"))

#+nil
(bind ((map (make-hash-table :test #'equal))
       (brick (make-brick
               :start (make-3d-coord :x 1 :y 1 :z 1)
               :end (make-3d-coord :x 1 :y 3 :z 1)))
       (brick-2 (make-brick
                 :start (make-3d-coord :x 1 :y 1 :z 3)
                 :end (make-3d-coord :x 1 :y 3 :z 3)))
       (bricks (list brick brick-2)))
  (place-brick brick map)
  (place-brick brick-2 map)
  (fall-one-step map bricks)
  (fall-one-step map bricks)
  map
  ;; (settle-bricks map bricks)
  (iter
    (for brick in bricks)
    (counting (brick-can-disintegrate brick map bricks)))
  )

(defun count-bricks-that-fall (map bricks)
  (bind ((bricks-that-fell nil))
    (iter
      (for brick in bricks)
      (when (member brick bricks-that-fell)
        (next-iteration))
      (for low-points = (low-points brick))
      (for can-fall = (every (lambda (point)
                               (no-brick-below point map))
                             low-points))
      (counting can-fall)
      (when can-fall
        (push brick bricks-that-fell)))))

(defun copy-hashtable (table)
  (bind ((new-table (make-hash-table :test #'equal)))
    (iter
      (for (key value) in-hashtable table)
      (setf (gethash key new-table) value))
    new-table))

(defmethod bricks-below ((b brick) map)
  (with-slots (start end) b
    (with-slots (x y z) start
      (bind ((x2 (3d-coord-x end))
             (y2 (3d-coord-y end))
             (z2 (3d-coord-z end))
             (min-z (min z z2))
             (found (make-hash-table)))
        (iter
          (for xi from (min x x2) to (max x x2))
          (iter
            (for yi from (min y y2) to (max y y2))
            (for new-point = (list xi yi (1- min-z)))
            (for brick-at-point = (gethash new-point map))
            (when brick-at-point
              (setf (gethash brick-at-point found) t))))
        (iter
          (for (key value) in-hashtable found)
          (collecting key))))))

(defmethod bricks-supported ((in-b brick) map)
  (bind ((remaining (seq in-b))
         (bricks-already-cascaded (make-hash-table)))
    (setf (gethash in-b bricks-already-cascaded) t)
    (iter
      (while (not (empty? remaining)))
      (when (not (first-iteration-p))
        (counting t))
      (for b = (first remaining))
      (setf remaining (less-first remaining))
      (for start = (brick-start b))
      (for end = (brick-end b))
      (for x = (3d-coord-x start))
      (for y = (3d-coord-y start))
      (for z = (3d-coord-z start))
      (for x2 = (3d-coord-x end))
      (for y2 = (3d-coord-y end))
      (for z2 = (3d-coord-z end))
      (for max-z = (max z z2))
      (iter
        (for xi from (min x x2) to (max x x2))
        (iter
          (for yi from (min y y2) to (max y y2))
          (for new-point = (list xi yi (1+ max-z)))
          (for brick-at-point = (gethash new-point map))
          (and brick-at-point
               (not (gethash brick-at-point bricks-already-cascaded))
               (bind ((bricks-below (bricks-below brick-at-point map)))
                 (when (every (lambda (brick-below)
                                (gethash brick-below bricks-already-cascaded))
                              bricks-below)
                   (setf (gethash brick-at-point bricks-already-cascaded) t)
                   (setf remaining (with-last remaining brick-at-point))))))))))

(defun part-2 (&optional (relative-path-name "src/day-22.in"))
  (bind ((bricks (read-problem relative-path-name))
         (map (place-bricks bricks)))
    (settle-bricks map bricks)
    (iter
      (for brick in bricks)
      (summing (bricks-supported brick map)))))

;; Too low: 1191
;; Too low: 3308

(defun test-2 ()
  (part-2 "src/day-22-test.in"))
