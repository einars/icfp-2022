(ql:quickload "png-read")

(defconstant +components+ 4)

(defvar *target* nil)
(defvar *canvas* nil)
(defvar *allbox* nil)

(defvar *program* nil)

(defvar *image-w* 0)
(defvar *image-h* 0)
(defvar *surface* 0)

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

(defstruct box id shape color parent children)

(defun box-size (box)
  (shape-size (box-shape box)))

(defun make-color (r g b a)
  (vector r g b a))

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
  (make-box :id 0 :color *white* :shape (full-canvas-shape)))

(defun empty-allbox ()
  (make-box :children (list (full-canvas-box))))

(defun save-color (out x y)
  (dotimes (i (1- +components+))
    (format out "~A " (elt (aref *canvas* x y) i))))

(defun save-canvas ()
  (with-open-file (out "canvas.pnm" :direction :output :if-exists :supersede)
    (format out "P3~%~A ~A 255~%" *image-w* *image-h*)
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(save-color out x y)))))

(defun box-cost (box)
  (/ *surface* (surface (shape-size (box-shape box)))))

(defun command-cost (cmd)
  (round (* (box-cost (second cmd))
	    (case (first cmd)
	      (:lcut 7)
	      (:color 5)
	      (otherwise 0)))))

(defun cost ()
  (reduce #'+ (mapcar #'command-cost *program*)))

(defun score ()
  (+ (cost) (similarity)))

(defun find-child (id node)
  (let ((id-one (first id))
	(children (box-children node)))
    (first (member-if (lambda (x) (= id-one (box-id x))) children))))

(defun find-box (id &optional (node *allbox*))
  (if (consp id)
      (find-box (rest id) (find-child id node))
      node))

(defun box-to-id-reverse (box)
  (if (not (null box))
      (cons (box-id box) (box-to-id-reverse (box-parent box)))
      nil))

(defun box-to-id (box)
  (nreverse (box-to-id-reverse box)))

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

(defun lsplit (parent axis num)
  (let ((id -1) (parent-shape (box-shape parent)))
    (mapcar (lambda (s) (make-box :id (incf id) :parent parent :shape s))
	    (cond ((eq 'x axis) (split-shape-x parent-shape num))
		  ((eq 'y axis) (split-shape-y parent-shape num))))))

(defun lcut (box axis num)
  (when (consp (box-children box)) (error "box already have children"))
  (setf (box-children box) (lsplit box axis num))
  (push `(:lcut ,box ,axis ,num) *program*)
  (box-children box))

(defun set-pixel (shape x y color)
  (let ((tx (+ (shape-x shape) x))
	(ty (- *image-w* (+ (shape-y shape) y 1))))
    (setf (aref *canvas* tx ty) color)))

(defun get-pixel (shape x y)
  (let ((tx (+ (shape-x shape) x))
	(ty (- *image-w* (+ (shape-y shape) y 1))))
    (aref *canvas* tx ty)))

(defun color (box color)
  (let* ((shape (box-shape box)))
    (push `(:color ,box ,color) *program*)
    (dotimes (y (shape-h shape))
      (dotimes (x (shape-w shape))
	(set-pixel shape x y color)))))

(defun replace-in-parent (box1 box2)
  (nsubstitute box2 box1 (box-children (box-parent box1))))

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
  (replace-in-parent box1 box2)
  (replace-in-parent box2 box1)
  (swap-pixels box1 box2))

(defun format-id (box)
  (format nil "~{~A~^.~}" (box-to-id box)))

(defun format-color (color)
  (format nil "~{~A~^,~}" (coerce color 'list)))

(defun format-cut-command (out cmd)
  (format out "cut [~A] [~A] [~A]~%"
	  (format-id (second cmd))
	  (third cmd) (fourth cmd)))

(defun format-color-command (out cmd)
  (format out "color [~A] [~A]~%"
	  (format-id (second cmd))
	  (format-color (third cmd))))

(defun save-command (out cmd)
  (case (first cmd)
    (:lcut (format-cut-command out cmd))
    (:color (format-color-command out cmd))
    (otherwise nil)))

(defun save-program ()
  (with-open-file (out "result.txt" :direction :output :if-exists :supersede)
    (mapc (lambda (cmd) (save-command out cmd)) (nreverse *program*))))

(defun slice-horizontal-problem-1 (n w box p)
  (when (> w 0)
    (let ((children (lcut box 'x w)))
      (setf box (second children))
      (slice-horizontal-problem-1 (1+ n) (- w 40) (first children) p)))
  (when (evenp (+ n p)) (color box *white*)))

(defun slice-vertical-problem-1 (n h box)
  (when (< h 400)
    (let ((children (lcut box 'y h)))
      (setf box (first children))
      (slice-vertical-problem-1 (1+ n) (+ 40 h) (second children))))
  (slice-horizontal-problem-1 0 319 box (mod n 2)))

(defun run-test-program-problem-1 ()
  (color (find-box '(0)) (make-color 0 74 173 255))
  (lcut (find-box '(0)) 'x 357)
  (lcut (find-box '(0 0)) 'y 43)
  (color (find-box '(0 0 1)) (make-color 0 0 0 255))
  (slice-vertical-problem-1 0 81 (find-box '(0 0 1)))
  (color (find-box '(0 0 1 0 1))
	 (make-color 0 74 173 255)))

(defun painter (file)
  (let* ((png (png-read:read-png-file file))
	 (*program* (make-empty-program))
	 (*image-w* (png-read:width png))
	 (*image-h* (png-read:height png))
	 (*surface* (* *image-w* *image-h*))
	 (*target* (convert-image png))
	 (*canvas* (empty-canvas))
	 (*allbox* (empty-allbox)))
    (run-test-program-problem-1)
    (format t "SCORE:~A~%" (score))
    (save-program)
    (save-canvas)))
