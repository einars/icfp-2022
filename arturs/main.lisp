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

(defun box-cost (id)
  (let ((box (find-box id)))
    (round (/ *surface* (surface (shape-size (box-shape box)))))))

(defun command-cost (cmd)
  (* (box-cost (second cmd))
     (case (first cmd)
       (:lcut 7)
       (:color 5)
       (otherwise 0))))

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

(defun split-shape (shape num set add)
  (let ((pos (shape-pos shape))
	(size (shape-size shape)))
    (list (make-shape :pos pos :size (funcall set size num))
	  (make-shape :pos (funcall add pos num)
		      :size (funcall add size (- num))))))

(defun split-shape-x (shape x)
  (split-shape shape x #'pos-set-x #'pos-add-x))

(defun split-shape-y (shape y)
  (split-shape shape y #'pos-set-y #'pos-add-y))

(defun lsplit (parent axis num)
  (let ((id -1) (parent-shape (box-shape parent)))
    (mapcar (lambda (s) (make-box :id (incf id) :parent parent :shape s))
	    (cond ((eq :x axis) (split-shape-x parent-shape num))
		  ((eq :y axis) (split-shape-y parent-shape num))))))

(defun lcut (id axis num)
  (let ((box (find-box id)))
    (when (consp (box-children box)) (error "box already have children"))
    (setf (box-children box) (lsplit box axis num))
    (push `(:lcut ,id ,axis ,num) *program*)))

(defun set-pixel (shape x y color)
  (let ((tx (+ (shape-x shape) x))
	(ty (- *image-w* (+ (shape-y shape) y 1))))
    (setf (aref *canvas* tx ty) color)))

(defun color (id color)
  (let* ((box (find-box id))
	 (shape (box-shape box)))
    (push `(:color ,id ,color) *program*)
    (dotimes (y (shape-h shape))
      (dotimes (x (shape-w shape))
	(set-pixel shape x y color)))))

(defun run-test-program-problem-1 ()
  (color '(0) (make-color 0 74 173 255))
  (lcut '(0) :x 357)
  (lcut '(0 0) :y 43)
  (color '(0 0 1) (make-color 128 128 128 255)))

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
    (save-canvas)))
