(ql:quickload "png-read")

(defconstant +components+ 4)

(defvar *target* nil)
(defvar *canvas* nil)

(defvar *program* nil)
(defvar *cost* 0)

(defvar *image-w* 0)
(defvar *image-h* 0)

(defun make-color (r g b a)
  (vector r g b a))

(defparameter *black* (make-color 0 0 0 0))

(defun empty-canvas ()
  (make-array (list *image-w* *image-h*) :initial-element *black*))

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
  (let ((score 0.0))
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(let ((sum 0.0))
	  (dotimes (i +components+)
	    (incf sum (component-distance (aref p1 x y) (aref p2 x y) i)))
	  (incf score (sqrt sum)))))
    (* 0.005 score)))

(defun make-empty-program ()
  nil)

(defun painter (file)
  (let* ((png (png-read:read-png-file file))
	 (*program* (make-empty-program))
	 (*image-w* (png-read:width png))
	 (*image-h* (png-read:height png))
	 (*target* (convert-image png))
	 (*canvas* (empty-canvas))
	 (*cost* 0))
    (format t "~A~%" (similarity))))
