(ql:quickload "png-read")
(ql:quickload "cl-json")

(defconstant +components+ 4)
(defconstant +half-pool+ 2)

(defvar *target* nil)
(defvar *canvas* nil)
(defvar *allbox* nil)
(defvar *boxnum* 0)

(defvar *program* nil)

(defvar *image-w* 0)
(defvar *image-h* 0)
(defvar *surface* 0)
(defvar *problem* 0)
(defvar *parent* nil)

(defvar *pnm* "canvas.pnm")
(defvar *txt* "result.txt")

(defvar *best* most-positive-fixnum)

(defparameter *lcut-price* 7)
(defparameter *pcut-price* 10)

(declaim (ftype function print-pos))
(defstruct (pos (:print-function (lambda (b s d) (print-pos b s d))))
  x y)

(defun print-pos (b s d)
  (declare (ignore d))
  (format s "<P:~A,~A>" (pos-x b) (pos-y b)))

(defun pos-new (x y)
  (make-pos :x x :y y))

(defun pos-add-x (p x)
  (make-pos :x (+ (pos-x p) x) :y (pos-y p)))

(defun pos-set-x (p x)
  (make-pos :x x :y (pos-y p)))

(defun pos-add-y (p y)
  (make-pos :x (pos-x p) :y (+ (pos-y p) y)))

(defun pos-set-y (p y)
  (make-pos :x (pos-x p) :y y))

(defun pos-eq (p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

(defun pos-fn (p1 p2 fn)
  (make-pos :x (funcall fn (pos-x p1) (pos-x p2))
	    :y (funcall fn (pos-y p1) (pos-y p2))))

(defun pos-min (p1 p2)
  (pos-fn p1 p2 #'min))

(defun pos-max (p1 p2)
  (pos-fn p1 p2 #'max))

(defun pos-sub (p1 p2)
  (pos-fn p1 p2 #'-))

(defun pos-add (p1 p2)
  (pos-fn p1 p2 #'+))

(defun surface (p)
  (* (pos-x p) (pos-y p)))

(defparameter *zero* (make-pos :x 0 :y 0))

(declaim (ftype function print-shape))
(defstruct (shape (:print-function (lambda (b s d) (print-shape b s d))))
  pos size)

(defun print-shape (b s d)
  (declare (ignore d))
  (format s "<SHAPE POS:~A SIZE:~A>"
	  (shape-pos b) (shape-size b)))

(defun shape-x (s)
  (pos-x (shape-pos s)))

(defun shape-y (s)
  (pos-y (shape-pos s)))

(defun shape-w (s)
  (pos-x (shape-size s)))

(defun shape-h (s)
  (pos-y (shape-size s)))

(defun pos-in-shape (p s)
  (and (<= 0 (- (pos-x p) (shape-x s)) (1- (shape-w s)))
       (<= 0 (- (pos-y p) (shape-y s)) (1- (shape-h s)))))

(declaim (ftype function print-box))
(defstruct (box (:print-function (lambda (b s d) (print-box b s d))))
  id shape color parent children)

(defun print-box (b s d)
  (declare (ignore d))
  (format s "<BOX ID:~A CHILDREN:~A ~A>"
	  (box-id b) (length (box-children b)) (box-shape b)))

(defun box-pos (box)
  (shape-pos (box-shape box)))

(defun box-diff (box1 box2)
  (pos-sub (box-pos box1) (box-pos box2)))

(defun box-size (box)
  (shape-size (box-shape box)))

(defun box-surface (box)
  (surface (box-size box)))

(defun make-color (r g b a)
  (vector r g b a))

(defun add-color (dst src)
  (dotimes (i +components+)
    (incf (elt dst i) (elt src i))))

(defun div-color (color div)
  (dotimes (i +components+ color)
    (setf (elt color i) (round (/ (elt color i) div)))))

(defparameter *white* (make-color 255 255 255 255))

(defun empty-canvas ()
  (make-array (list *image-w* *image-h*) :initial-element *white*))

(defun get-color (data x y)
  (let ((color nil))
    (dotimes (i +components+)
      (push (aref data x y i) color))
    (apply #'make-color (nreverse color))))

(defun convert-image (png)
  (let ((data (png-read:image-data png))
	(new-image (empty-canvas)))
    (dotimes (y *image-h* new-image)
      (dotimes (x *image-w*)
	(setf (aref new-image x y)
	      (get-color data x y))))))

(defun component-distance (c1 c2 i)
  (expt (- (svref c1 i) (svref c2 i)) 2))

(defun color-distance (c1 c2)
  (let ((sum 0.0d0))
    (dotimes (i +components+)
      (incf sum (component-distance c1 c2 i)))
    (sqrt sum)))

(defun similarity (&optional (p1 *target*) (p2 *canvas*))
  (let ((score 0.0d0))
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(incf score (color-distance (aref p1 x y) (aref p2 x y)))))
    (round (* 0.005 score))))

(defun make-empty-program ()
  nil)

(defun full-canvas-shape ()
  (make-shape :pos *zero* :size (make-pos :x *image-w* :y *image-h*)))

(defun full-canvas-box ()
  (make-box :id (list *boxnum*) :color *white* :shape (full-canvas-shape)))

(defun empty-allbox ()
  (make-box :children (list (full-canvas-box))))

(defun save-color (out x y)
  (dotimes (i (1- +components+))
    (format out "~A " (svref (aref *canvas* x y) i))))

(defun save-canvas ()
  (with-open-file (out *pnm* :direction :output :if-exists :supersede)
    (format out "P3~%~A ~A 255~%" *image-w* *image-h*)
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(save-color out x y)))))

(defun box-cost (box)
  (/ *surface* (surface (shape-size (box-shape box)))))

(defun color-score (cmd)
  (if (fourth cmd) 0 5))

(defun command-cost (cmd)
  (round (* (box-cost (second cmd))
	    (case (first cmd)
	      (:swap 3)
	      (:merge 1)
	      (:lcut *lcut-price*)
	      (:pcut *pcut-price*)
	      (:color (color-score cmd))
	      (otherwise 0)))))

(defun cost ()
  (reduce #'+ (mapcar #'command-cost *program*)))

(defun score ()
  (+ (cost) (similarity)))

(defun find-box (id &optional (node *allbox*))
  (cond ((equalp id (box-id node)) node)
	(t (dolist (i (box-children node))
	     (let ((result (find-box id i)))
	       (when (box-p result)
		 (return-from find-box result)))))))

(defun box-by-pos (pos &optional (node *allbox*))
  (let ((children (box-children node)))
    (cond ((null children) node)
	  (t (dolist (b children)
	       (when (pos-in-shape pos (box-shape b))
		 (return-from box-by-pos (box-by-pos pos b))))))))

(defun split-shape (shape num set add)
  (let ((pos (shape-pos shape))
	(size (shape-size shape)))
    (list (make-shape :pos pos :size (funcall set size num))
	  (make-shape :pos (funcall add pos num)
		      :size (funcall add size (- num))))))

(defun split-shape-x (shape x)
  (split-shape shape (- x (shape-x shape)) #'pos-set-x #'pos-add-x))

(defun split-shape-y (shape y)
  (split-shape shape (- y (shape-y shape)) #'pos-set-y #'pos-add-y))

(defun extend-id (parent id-postfix)
  (append (box-id parent) (list id-postfix)))

(defun make-child (id-postfix parent shape)
  (make-box :id (extend-id parent id-postfix) :parent parent :shape shape))

(defun l-split (parent axis num)
  (let ((id -1) (parent-shape (box-shape parent)))
    (mapcar (lambda (shape) (make-child (incf id) parent shape))
	    (cond ((eq 'x axis) (split-shape-x parent-shape num))
		  ((eq 'y axis) (split-shape-y parent-shape num))))))

(defun split-shape-4 (shape x y)
  (let ((slice (split-shape-y shape y)))
    (append (split-shape-x (first slice) x)
	    (nreverse (split-shape-x (second slice) x)))))

(defun p-split (parent x y)
  (let ((id -1) (parent-shape (box-shape parent)))
    (mapcar (lambda (shape) (make-child (incf id) parent shape))
	    (split-shape-4 parent-shape x y))))

(defun lcut-sanity-check (box axis num)
  (let ((p (box-pos box))
	(s (box-shape box)))
    (cond
      ((eq axis 'x) (and (< (pos-x p) num (1- (+ (pos-x p) (shape-w s))))))
      ((eq axis 'y) (and (< (pos-y p) num (1- (+ (pos-y p) (shape-h s))))))
      (t nil))))

(defun pcut-sanity-check (box x y)
  (and (lcut-sanity-check box 'x x)
       (lcut-sanity-check box 'y y)))

(define-condition cmd-error (error) ())

(defun lcut (box axis num)
  (when (consp (box-children box))
    (error "box already have children"))
  (unless (lcut-sanity-check box axis num)
    (error 'cmd-error))
  (setf (box-children box) (l-split box axis num))
  (push `(:lcut ,box ,axis ,num) *program*)
  (box-children box))

(defun pcut (box x y)
  (when (consp (box-children box))
    (error "box already have children"))
  (unless (pcut-sanity-check box x y)
    (error 'cmd-error))
  (setf (box-children box) (p-split box x y))
  (push `(:pcut ,box ,x ,y) *program*)
  (box-children box))

(defun set-pixel (shape x y color)
  (let ((tx (+ (shape-x shape) x))
	(ty (- *image-w* (+ (shape-y shape) y 1))))
    (setf (aref *canvas* tx ty) color)))

(defun get-pixel (shape x y &optional (img *canvas*))
  (let ((tx (+ (shape-x shape) x))
	(ty (- *image-w* (+ (shape-y shape) y 1))))
    (aref img tx ty)))

(defun color (box color &optional disabled)
  (let* ((shape (box-shape box)))
    (push `(:color ,box ,color ,disabled) *program*)
    (when (not disabled)
      (dotimes (y (shape-h shape))
	(dotimes (x (shape-w shape))
	  (set-pixel shape x y color))))))

(defun displace-box (box diff)
  (let ((shape (box-shape box)))
    (setf (shape-pos shape) (pos-add (shape-pos shape) diff))
    (mapc (lambda (c) (displace-box c diff)) (box-children box))))

(defun replace-parent (box1 box2 diff new-parent)
  (when new-parent
    (nsubstitute box2 box1 (box-children new-parent))
    (setf (box-parent box2) new-parent))
  (displace-box box2 diff))

(defun swap-pixels (box1 box2)
  (let ((shape1 (box-shape box1))
	(shape2 (box-shape box2)))
    (dotimes (y (shape-h shape1))
      (dotimes (x (shape-w shape1))
	(let ((color1 (get-pixel shape1 x y))
	      (color2 (get-pixel shape2 x y)))
	  (set-pixel shape1 x y color2)
	  (set-pixel shape2 x y color1))))))

(defun swap-sanity-check (box1 box2)
  (pos-eq (box-size box1) (box-size box2)))

(defun swap (box1 box2)
  (unless (swap-sanity-check box1 box2)
    (error 'cmd-error))
  (push `(:swap ,box1 ,box2) *program*)
  (let ((parent1 (box-parent box1))
	(parent2 (box-parent box2))
	(diff1 (box-diff box1 box2))
	(diff2 (box-diff box2 box1)))
    (replace-parent box1 box2 diff1 parent1)
    (replace-parent box2 box1 diff2 parent2))
  (swap-pixels box1 box2))

(defun register-merge-cmd (box1 box2)
  (push (if (> (box-surface box1)
	       (box-surface box2))
	    `(:merge ,box1 ,box2)
	    `(:merge ,box2 ,box1))
	*program*))

(defun remove-from-parent (box)
  (let ((parent (box-parent box)))
    (when parent
      (setf (box-children parent) (delete box (box-children parent))))))

(defun merge-shape (shape1 shape2)
  (let* ((pos (pos-min (shape-pos shape1) (shape-pos shape2)))
	 (corner1 (pos-add (shape-pos shape1) (shape-size shape1)))
	 (corner2 (pos-add (shape-pos shape2) (shape-size shape2)))
	 (top (pos-max corner1 corner2)))
    (make-shape :pos pos :size (pos-sub top pos))))

(defun merge-box (box1 box2)
  (make-box
   :id (list (incf *boxnum*))
   :shape (merge-shape (box-shape box1) (box-shape box2))))

(defun merge-vertical-sanity-check (pos1 pos2 size1 size2)
  (and (= (pos-x pos1) (pos-x pos2))
       (= (pos-x size1) (pos-x size2))
       (if (< (pos-y pos1) (pos-y pos2))
	   (= (- (pos-y pos2) (pos-y pos1)) (pos-y size1))
	   (= (- (pos-y pos1) (pos-y pos2)) (pos-y size2)))))

(defun merge-horizontal-sanity-check (pos1 pos2 size1 size2)
  (and (= (pos-y pos1) (pos-y pos2))
       (= (pos-y size1) (pos-y size2))
       (if (< (pos-x pos1) (pos-x pos2))
	   (= (- (pos-x pos2) (pos-x pos1)) (pos-x size1))
	   (= (- (pos-x pos1) (pos-x pos2)) (pos-x size2)))))

(defun merge-sanity-check (box1 box2)
  (let ((pos1 (box-pos box1))
	(pos2 (box-pos box2))
	(size1 (box-size box1))
	(size2 (box-size box2)))
    (or (merge-vertical-sanity-check pos1 pos2 size1 size2)
	(merge-horizontal-sanity-check pos1 pos2 size1 size2))))

(defun box-merge (box1 box2)
  (unless (merge-sanity-check box1 box2)
    (error 'cmd-error))
  (register-merge-cmd box1 box2)
  (remove-from-parent box1)
  (remove-from-parent box2)
  (let ((new-box (merge-box box1 box2)))
    (push new-box (box-children *allbox*))
    new-box))

(defun format-id (box)
  (format nil "~{~A~^.~}" (box-id box)))

(defun format-color (color)
  (format nil "~{~A~^,~}" (coerce color 'list)))

(defun format-lcut-command (out cmd)
  (format out "cut [~A] [~A] [~A]~%"
	  (format-id (second cmd))
	  (third cmd) (fourth cmd)))

(defun format-pcut-command (out cmd)
  (format out "cut [~A] [~A,~A]~%"
	  (format-id (second cmd))
	  (third cmd) (fourth cmd)))

(defun format-swap-command (out cmd)
  (format out "swap [~A] [~A]~%"
	  (format-id (second cmd))
	  (format-id (third cmd))))

(defun format-color-command (out cmd)
  (when (not (fourth cmd))
    (format out "color [~A] [~A]~%"
	    (format-id (second cmd))
	    (format-color (third cmd)))))

(defun format-merge-command (out cmd)
  (format out "merge [~A] [~A]~%"
	  (format-id (second cmd))
	  (format-id (third cmd))))

(defun save-command (out cmd)
  (case (first cmd)
    (:lcut (format-lcut-command out cmd))
    (:pcut (format-pcut-command out cmd))
    (:swap (format-swap-command out cmd))
    (:merge (format-merge-command out cmd))
    (:color (format-color-command out cmd))
    (otherwise nil)))

(defun save-program ()
  (with-open-file (out *txt* :direction :output :if-exists :supersede)
    (mapc (lambda (cmd) (save-command out cmd)) (reverse *program*))))

(defun average-color (shape)
  (let ((count 0) (avg-color (make-color 0 0 0 0)))
    (dotimes (y (shape-h shape))
      (dotimes (x (shape-w shape))
	(add-color avg-color (get-pixel shape x y *target*))
	(incf count)))
    (div-color avg-color count)))

(defun mosaic-chunk (x y)
  (make-pos :x (floor (/ *image-w* x)) :y (floor (/ *image-h* y))))

(defun calc-color (box x y)
  (average-color (make-shape :pos (box-pos box) :size (mosaic-chunk x y))))

(defun background-box ()
  (first (box-children *allbox*)))

(defun background-color ()
  (let ((parent (background-box)))
    (color parent (calc-color parent *image-w* *image-h*))))

(defun horizontal-shreader (x y)
  (let ((stepx (floor (/ *image-w* x)))
	(stepy (floor (/ *image-h* y))))
    (background-color)
    (defun slice-horizontal-problem (i w box)
      (color box (calc-color box x y))
      (when (<= w (- *image-w* stepx))
	(let ((children (lcut box 'x w)))
	  (slice-horizontal-problem (1+ i) (+ w stepx) (second children)))))
    (defun slice-vertical-problem (i h box)
      (when (<= h (- *image-h* stepy))
	(let ((children (lcut box 'y h)))
	  (setf box (first children))
	  (slice-vertical-problem (1+ i) (+ h stepy) (second children))))
      (slice-horizontal-problem 0 stepx box))
    ; now run the slicer
    (slice-vertical-problem 0 stepy (background-box))))

(defun box-by-x-y (x y)
  (box-by-pos (pos-new x y)))

(defun vertical-weaver-with-lcut (p-w p-h)
  (let ((stepx (floor (/ *image-w* p-w)))
	(stepy (floor (/ *image-h* p-h))))
    (loop for y from 0 to (- *image-h* stepy) by stepy do
      (loop for x from 0 to (- *image-w* stepx) by stepx do
	(let (children1 children2 product)
	  (when (> y 0)
	    (setf children1 (lcut (box-by-x-y x y) 'y y)))
	  (when (> x 0)
	    (setf children2 (lcut (box-by-x-y x y) 'x x)))
	  (let ((box (box-by-x-y x y)))
	    (color box (calc-color box p-w p-h)))
	  (when children2
	    (setf product (apply #'box-merge children2)))
	  (when children1
	    (if product
		(box-merge product (first children1))
		(apply #'box-merge children1))))))))

(defun vertical-weaver-with-pcut (p-w p-h)
  (let ((stepx (floor (/ *image-w* p-w)))
	(stepy (floor (/ *image-h* p-h))))
    (loop for y from 0 to (- *image-h* stepy) by stepy do
      (loop for x from 0 to (- *image-w* stepx) by stepx do
	(let (children product)
	  (when (and (> y 0) (= x 0))
	    (setf children (lcut (box-by-x-y x y) 'y y)))
	  (when (and (> x 0) (= y 0))
	    (setf children (lcut (box-by-x-y x y) 'x x)))
	  (when (and (> x 0) (> y 0))
	    (setf children (pcut (box-by-x-y x y) x y)))
	  (let ((box (box-by-x-y x y)))
	    (color box (calc-color box p-w p-h)))
	  (when (and children (or (= x 0) (= y 0)))
	    (apply #'box-merge children))
	  (when (and (> x 0) (> y 0))
	    (push (box-merge (first children) (second children)) product)
	    (push (box-merge (third children) (fourth children)) product)
	    (box-merge (first product) (second product))))))))

(defun rect-color (box x y)
  (average-color (make-shape :pos (box-pos box) :size (pos-new x y))))

(defun vertical-pillars-of-fame (p-w p-h)
  (let ((stepx (floor (/ *image-w* p-w)))
	(stepy (floor (/ *image-h* p-h))))
    (loop for y from 0 to (- *image-h* stepy) by stepy do
      (let (vchildren product)
	(when (> y 0)
	  (setf vchildren (lcut (background-box) 'y y))
	  (setf product (second vchildren)))
	(loop for x from 0 to (- *image-w* stepx) by stepx do
	  (let (hchildren)
	    (when (> x 0)
	      (let ((box (or product (background-box))))
		(setf hchildren (lcut box 'x x))))
	    (let ((box (box-by-x-y x y)))
	      (color box (rect-color box stepx stepy)))
	    (when hchildren
	      (setf product (apply #'box-merge hchildren)))))
	(when (> y 0)
	  (box-merge (first vchildren) product))))))

(defun run-mosaic-program-solver (x y)
  (vertical-pillars-of-fame x y))
;  (vertical-weaver-with-pcut x y))
;  (vertical-weaver-with-lcut x y))
;  (horizontal-shreader x y))

(defun random-elt (l)
  (elt l (random (length l))))

(defun random-jerk (num)
  (+ num (* (1+ (random 100)) (- (* 2 (random 2)) 1))))

(defun mutate-number (victim)
  (setf (first victim) (random-jerk (first victim))))

(defun mutate-bool (victim)
  (setf (first victim) (not (first victim))))

(defun clamp (num)
  (max 0 (min 255 num)))

(defun mutate-color (victim)
  (let ((index (random 3)))
    (setf (elt victim index) (clamp (random-jerk (elt victim index))))
    victim))

(defun flip-coin ()
  (= 0 (random 2)))

(defun pick-color-arg (c)
  (let ((disabled (fourth c)))
    (if (or disabled (flip-coin))
	(rest (rest (rest c)))
	(third c))))

(defun pick-pcut-arg (c)
  (if (flip-coin)
      (rest (rest c))
      (rest (rest (rest c)))))

(defun random-tweak-program (program)
  (let ((places nil))
    (dolist (i program)
      (case (first i)
	(:color (push (pick-color-arg i) places))
	(:pcut (push (pick-pcut-arg i) places))
	(:lcut (push (rest (rest (rest i))) places))
	(otherwise nil)))
    (let ((victim (random-elt places)))
      (cond ((vectorp victim)
	     (mutate-color victim))
	    ((numberp (first victim))
	     (mutate-number victim))
	    (t (mutate-bool victim))))
    program))

(defun copy-color (color-cmd)
  (setf (third color-cmd) (copy-seq (third color-cmd))))

(defun copy-and-reverse-program (program)
  (let ((copy nil))
    (dolist (i program copy)
      (let ((subcopy (copy-list i)))
	(push subcopy copy)
	(when (eq (first subcopy) :color)
	  (copy-color subcopy))))))

(defun find-from-other (box)
  (find-box (box-id box)))

(defun execute-cmd (cmd)
  (let ((box (find-from-other (second cmd))))
    (case (first cmd)
      (:color (color box (third cmd) (fourth cmd)))
      (:lcut (lcut box (third cmd) (fourth cmd)))
      (:pcut (pcut box (third cmd) (fourth cmd)))
      (:swap (swap box (find-from-other (third cmd))))
      (:merge (box-merge box (find-from-other (third cmd))))
      (otherwise (error "implement execute cmd")))))

(declaim (ftype function read-canvas read-allbox))

(defun get-time ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defun execute-program (program &optional on-best)
  (let* ((*boxnum* 0)
	 (*canvas* (read-canvas))
	 (*allbox* (read-allbox))
	 (*program* (make-empty-program)))
    (dolist (i program)
      (execute-cmd i))
    (let ((score (score)))
      (when (< score *best*)
	(format t "BEST(~A):~A @ ~A~%" *problem* score (get-time))
	(when on-best (funcall on-best))
	(setf *best* score)
	(save-program)
	(save-canvas))
      (list score *program*))))

(defun new-mutation (program)
  (random-tweak-program
   (copy-and-reverse-program program)))

(defun create-program-pool (program)
  (let ((pool nil))
    (dotimes (i (* 2 +half-pool+))
      (push (list (score) program) pool))
    pool))

(defun execute-program-safe (x)
  (handler-case (execute-program x)
    (cmd-error (var)
      (declare (ignore var))
      (list most-positive-fixnum nil))))

(defun execute-contestant (x)
  (execute-program-safe (new-mutation (second x))))

(defun optimize-program (program)
  (let ((*best* most-positive-fixnum)
	(pool (create-program-pool program)))
    (loop (setf pool (sort pool #'> :key #'first))
	  (dotimes (i +half-pool+)
	    (setf (elt pool i) (copy-list (elt pool (+ i +half-pool+))))
	    (let ((done (execute-contestant (elt pool i))))
	      (setf (elt pool i) done))))))

(defun read-json (n)
  (with-open-file (problem (format nil "../problems/~A.initial.json" n))
    (json:decode-json problem)))

(defun make-shape-from-frame (pos top)
  (make-shape :pos pos :size (pos-sub top pos)))

(defun list-to-pos (l)
  (make-pos :x (first l) :y (second l)))

(defun json-place-to-pos (p key)
  (list-to-pos (rest (assoc key p))))

(defun json-block-id (p)
  (list (parse-integer (rest (assoc :block-id p)))))

(defun parse-json-into (allbox)
  (dolist (p (rest (third (read-json *problem*))))
    (let ((id (json-block-id p))
	  (pos (json-place-to-pos p :bottom-left))
	  (top (json-place-to-pos p :top-right)))
      (setf *boxnum* (max *boxnum* (first id)))
      (push (make-box :id id :shape (make-shape-from-frame pos top))
	    (box-children allbox)))))

(defun parse-json ()
  (let ((allbox (empty-allbox)))
    (parse-json-into allbox)
    allbox))

(defun update-program (program ticks)
  (let* ((candidates (box-children *allbox*))
	 (box1 (random-elt candidates)))
    (dotimes (i (1+ (random (min 10 (max 1 (floor ticks 10))))))
      (let ((box2 (random-elt candidates)))
	(push `(:swap ,box1 ,box2) program)
	(setf box1 box2)))
    program))

(defun run-swaper-solver ()
  (let* ((ticks 0)
	 (*best* (score))
	 (good (make-empty-program))
	 (brave (update-program good 0))
	 (update (lambda ()
		   (setf good brave)
		   (setf ticks 0))))
    (loop (execute-program brave update)
	  (setf brave (update-program good ticks))
	  (when (> ticks 1000)
	    (format t "GIVE UP SWAPER~%")
	    (return-from run-swaper-solver good))
	  (incf ticks))))

(defun box-merge-cells (size)
  (loop for y from 0 to (1- *image-h*) by (pos-y size) do
    (loop for x from (pos-x size) to (1- *image-w*) by (pos-x size) do
      (box-merge (box-by-pos (make-pos :x x :y y))
		 (box-by-pos (make-pos :x 0 :y y))))))

(defun box-merge-slices (size)
  (loop for y from (pos-y size) to (1- *image-h*) by (pos-y size) do
    (box-merge (box-by-pos (make-pos :x 0 :y y))
	       (box-by-pos (make-pos :x 0 :y 0)))))

(defun load-program (input-file)
  (let ((*parent* (background-box)))
    (load input-file)))

(defun run-later-solver (input-file x y)
  (let ((size (box-size (box-by-pos (make-pos :x 0 :y 0)))))
    ;(run-swaper-solver)
    (box-merge-cells size)
    (box-merge-slices size)
    (if (not (probe-file input-file))
	(run-mosaic-program-solver x y)
	(load-program input-file))))

(defun final-problem ()
  (> *problem* 35))

(defun later-problem ()
  (and (< *problem* 36)
       (> *problem* 25)))

(defun load-canvas ()
  (let* ((file (format nil "../problems/~A.initial.png" *problem*))
	 (png (png-read:read-png-file file)))
    (convert-image png)))

(defun read-canvas ()
  (if (or (later-problem)
	  (final-problem))
      (load-canvas)
      (empty-canvas)))

(defun read-allbox ()
  (if (later-problem)
      (parse-json)
      (empty-allbox)))

(defun process-cut (box args)
  (if (symbolp (second args))
      (lcut box (second args) (first (third args)))
      (pcut box (first (second args)) (second (second args)))))

(defun process-color (box args)
  (color box (coerce (second args) 'vector)))

(defun find-last-box (id)
  (incf (first id) (first (box-id *parent*)))
  (find-box id))

(defun process-cmd (fn box args)
  (funcall fn box (find-last-box (second args))))

(defun process (&rest args)
  (let ((box (find-last-box (second args))))
    (case (first args)
      (cut (process-cut box (rest args)))
      (swap (process-cmd #'swap box (rest args)))
      (merge (process-cmd #'box-merge box (rest args)))
      (color (process-color box (rest args)))
      (otherwise nil))))

(defun choose-solver (input-file x y)
  (when (final-problem)
    (setf *lcut-price* 2)
    (setf *pcut-price* 3))
  (cond ((later-problem)
	 (run-later-solver input-file x y))
	((probe-file input-file)
	 (load-program input-file))
	(t (run-mosaic-program-solver x y))))

(defun painter (i &optional (x 8) (y 8))
  (let* ((*boxnum* 0)
	 (*problem* i)
	 (file (format nil "../problems/~A.png" i))
	 (png (png-read:read-png-file file))
	 (*pnm* (format nil "canvas/~A.pnm" i))
	 (*txt* (format nil "result/~A.txt" i))
	 (*program* (make-empty-program))
	 (*image-w* (png-read:width png))
	 (*image-h* (png-read:height png))
	 (*surface* (* *image-w* *image-h*))
	 (*target* (convert-image png))
	 (*canvas* (read-canvas))
	 (*allbox* (read-allbox)))
    (choose-solver (format nil "input/~A.lisp" i) x y)
    (format t "N(~A,~A): SCORE:~A~%" x y (score))
    (save-program)
    (save-canvas)
    (when *program*
      (optimize-program *program*))
    (score)))

(defun top-level (&optional (i 10) (x 8) (y 8))
  (handler-case (painter i x y)
    (condition (var) (format t "ERROR: ~A~%" var)))
  (uiop:quit 0))

(defun paint-all (i &key (lo 2.0) (hi 12.0) (step 0.1))
  (let ((chunk-size nil)
	(best most-positive-fixnum))
    (loop for size from lo to hi by step do
      (let ((score (painter i size size)))
	(when (< score best)
	  (setf chunk-size size)
	  (setf best score))))
    (format t "BEST SCORE:~A N(~A)~%" best chunk-size)
    (painter i chunk-size chunk-size)))

(defun paint-all-files ()
  (loop for i from 1 to 25 do
    (format t "P:~A " i)
    (paint-all i)))

(defun main ()
  (apply #'top-level (mapcar #'parse-integer (rest sb-ext:*posix-argv*))))

(defun build ()
  (sb-ext:save-lisp-and-die
   "./run" :compression t :executable t
   :toplevel (function main)))

(defun profile-paint ()
  (sb-profile:profile lcut)
  (sb-profile:profile pcut)
  (sb-profile:profile box-merge)
  (sb-profile:profile swap)
  (sb-profile:profile color)
  (sb-profile:profile cost)
  (sb-profile:profile score)
  (sb-profile:profile painter)
  (sb-profile:profile similarity)
  (sb-profile:profile read-canvas)
  (sb-profile:profile read-allbox)
  (sb-profile:profile save-canvas)
  (sb-profile:profile save-program)
  (sb-profile:profile execute-cmd)
  (sb-profile:profile execute-program)
  (sb-profile:profile optimize-program)
  (sb-profile:profile component-distance)
  (sb-profile:reset)
  (painter 10 7.5 7.5)
  (sb-profile:report))
