(ql:quickload "png-read")

(defconstant +components+ 4)

(defvar *target* nil)
(defvar *canvas* nil)
(defvar *allbox* nil)
(defvar *boxnum* 0)

(defvar *program* nil)

(defvar *image-w* 0)
(defvar *image-h* 0)
(defvar *surface* 0)

(defvar *pnm* "canvas.pnm")
(defvar *txt* "result.txt")

(defstruct pos x y)

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

(defstruct shape pos size)

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

(defstruct box id shape color parent children)

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
  (expt (- (aref c1 i) (aref c2 i)) 2))

(defun similarity (&optional (p1 *target*) (p2 *canvas*))
  (let ((score 0.0d0))
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(let ((sum 0.0d0))
	  (dotimes (i +components+)
	    (incf sum (component-distance (aref p1 x y) (aref p2 x y) i)))
	  (incf score (sqrt sum)))))
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
    (format out "~A " (elt (aref *canvas* x y) i))))

(defun save-canvas ()
  (with-open-file (out *pnm* :direction :output :if-exists :supersede)
    (format out "P3~%~A ~A 255~%" *image-w* *image-h*)
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(save-color out x y)))))

(defun box-cost (box)
  (/ *surface* (surface (shape-size (box-shape box)))))

(defun command-cost (cmd)
  (round (* (box-cost (second cmd))
	    (case (first cmd)
	      (:swap 3)
	      (:lcut 7)
	      (:pcut 10)
	      (:color 5)
	      (:merge 1)
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

(defun lcut (box axis num)
  (when (consp (box-children box)) (error "box already have children"))
  (setf (box-children box) (l-split box axis num))
  (push `(:lcut ,box ,axis ,num) *program*)
  (box-children box))

(defun pcut (box x y)
  (when (consp (box-children box)) (error "box already have children"))
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

(defun color (box color)
  (let* ((shape (box-shape box)))
    (push `(:color ,box ,color) *program*)
    (dotimes (y (shape-h shape))
      (dotimes (x (shape-w shape))
	(set-pixel shape x y color)))))

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

(defun swap (box1 box2)
  (when (not (pos-eq (box-size box1) (box-size box2)))
    (error "box sizes for swap is not equal"))
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
    (setf (box-children parent) (delete box (box-children parent)))))

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

(defun box-merge (box1 box2)
  (when (not (is-good-merge box1 box2))
    (error "boxes for merge are not good"))
  (register-merge-cmd box1 box2)
  (remove-from-parent box1)
  (remove-from-parent box2)
  (push (merge-box box1 box2)
	(box-children *allbox*)))

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
  (format out "color [~A] [~A]~%"
	  (format-id (second cmd))
	  (format-color (third cmd))))

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

(defun mosaic-chunk (n)
  (make-pos :x (floor (/ *image-w* n)) :y (floor (/ *image-h* n))))

(defun calc-color (box n)
  (average-color (make-shape :pos (box-pos box) :size (mosaic-chunk n))))

(defun run-mosaic-program-solver (n)
  (let ((stepx (floor (/ *image-w* n)))
	(stepy (floor (/ *image-h* n))))
    (defun slice-horizontal-problem (i w box)
      (color box (calc-color box n))
      (when (< i (1- (floor n)))
	(let ((children (lcut box 'x w)))
	  (slice-horizontal-problem (1+ i) (+ w stepx) (second children)))))
    (defun slice-vertical-problem (i h box)
      (when (< i (1- (floor n)))
	(let ((children (lcut box 'y h)))
	  (setf box (first children))
	  (slice-vertical-problem (1+ i) (+ h stepy) (second children))))
      (slice-horizontal-problem 0 stepx box))
    ; now run the slicer
    (slice-vertical-problem 0 stepy (find-box '(0)))))

(defun painter (i n)
  (let* ((file (format nil "../problems/~A.png" i))
	 (png (png-read:read-png-file file))
	 (*pnm* (format nil "canvas/~A.pnm" i))
	 (*txt* (format nil "result/~A.txt" i))
	 (*program* (make-empty-program))
	 (*image-w* (png-read:width png))
	 (*image-h* (png-read:height png))
	 (*surface* (* *image-w* *image-h*))
	 (*target* (convert-image png))
	 (*canvas* (empty-canvas))
	 (*allbox* (empty-allbox))
	 (*boxnum* 0))
    (run-mosaic-program-solver n)
;    (format t "N(~A): SCORE:~A~%" n (score))
    (save-program)
    (save-canvas)
    (score)))

(defun paint-all (i)
  (let ((chunk-size nil)
	(best most-positive-fixnum))
    (loop for n from 20 to 120 do
      (let* ((size (/ n 10.0))
	     (score (painter i size)))
	(when (< score best)
	  (setf chunk-size size)
	  (setf best score))))
    (format t "BEST SCORE:~A N(~A)~%" best chunk-size)
    (painter i chunk-size)))

(defun paint-all-files ()
  (loop for i from 1 to 25 do
    (format t "P:~A " i)
    (paint-all i)))
