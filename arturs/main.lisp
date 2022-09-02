(ql:quickload "png-read")

(defconstant +components+ 4)

(defvar *target* nil)
(defvar *canvas* nil)

(defvar *program* nil)

(defvar *image-w* 0)
(defvar *image-h* 0)

(defun black-canvas ()
  (make-array (list *image-w* *image-h* +components+) :initial-element 0))

(defun pow-2 (x)
  (* x x))

(defun component-distnace (p1 p2 x y i)
  (pow-2 (- (aref p1 x y i)
	    (aref p2 x y i))))

(defun similarity (&optional (p1 *target*) (p2 *canvas*))
  (let ((score 0.0))
    (dotimes (y *image-h*)
      (dotimes (x *image-w*)
	(let ((sum 0.0))
	  (dotimes (i +components+)
	    (incf sum (expt (- (aref p1 x y i) (aref p2 x y i)) 2)))
	  (incf score (sqrt sum)))))
    (* 0.005 score)))

(defun make-empty-program ()
  nil)

(defun painter (file)
  (let* ((png (png-read:read-png-file file))
	 (*program* (make-empty-program))
	 (*target* (png-read:image-data png))
	 (*image-w* (png-read:width png))
	 (*image-h* (png-read:height png))
	 (*canvas* (black-canvas)))
    (format t "~A~%" (similarity))))
