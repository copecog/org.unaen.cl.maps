;;;; org.unaen.cl.maps/maps.lisp

(uiop:define-package #:org.unaen.cl.maps
  (:documentation "Simple Set source for maps.")
  (:shadow #:map)
  (:use #:common-lisp)
  (:export #:*map-test*
           #:map
           #:map-add
           #:map-get))

(in-package #:org.unaen.cl.maps)

#| ---------- map ---------------------------------------------------------- |#
(defparameter *map-test* 'equal)

(defclass map ()
  ((stor :initform (make-hash-table :test *map-test*) ;Initialize first (or default dimension).
         :accessor stor
         :documentation "Internal storage for the map.")
   (dimension :initarg :dimension
              :initform 1
              :reader dimension
              :documentation "The number of input objects that are going to be mapped to the single output object."))
  (:documentation "My own dumb multi-dimensional map implementation."))

(defun map (&key (dimension 1))
  (declare (type (integer 1 *) dimension))
  (make-instance 'map
                 :dimension dimension))

(defgeneric map-add (to-object from-object/objects-list map-instance)
  (:documentation "Add a mapping from a tuple of elements to a single element in the form of a lisp list of objects to a single lisp object."))

(defmethod map-add :before (to-object from-object/s (map-inst map))
  (when (/= (typecase from-object/s
	      (cons (list-length from-object/s))
	      (atom 1))
            (dimension map-inst))
    (error "Incorrect number of from-objects.")))

(labels ((map-add-rec (to-object from-objects-remaining nth-dimension)
	   "Recurse from-objects as hash table keys, setting or creating hash-tables for each dimension along the way."
	   (let ((nth-elt   (first  from-objects-remaining))
		 (nth+1-elt (second from-objects-remaining)))
	     (if nth+1-elt
		 (let ((nth+1-dimension (gethash nth-elt
						 nth-dimension)))
		   (etypecase nth+1-dimension
		     (hash-table (map-add-rec to-object
					      (rest from-objects-remaining)
					      nth+1-dimension))
		     (null       (map-add-rec to-object
					      (rest from-objects-remaining)
					      (setf (gethash nth-elt
							     nth-dimension)
						    (make-hash-table :test *map-test*))))))
		 (setf (gethash nth-elt
				nth-dimension)
		       to-object)))))

  (defmethod map-add (to-object from-object (map-inst map))
    (map-add-rec to-object
		 `(,from-object)
		 (stor map-inst)))

  (defmethod map-add (to-object (from-objects cons) (map-inst map))
    (map-add-rec to-object
		 from-objects
		 (stor map-inst))))


(defgeneric map-get (from-object/objects-list map-instance)
  (:documentation "Get the output object mapping for a respective object or objects list."))

(defmethod map-get :before (from-obj/s (map-inst map))
  (when (/= (typecase from-obj/s
              (cons (list-length from-obj/s))
              (atom 1))
            (dimension map-inst))
    (error "Incorrect number of from-objects.")))

(labels ((map-get-rec (from-objects-remaining nth-dimension)
	   "Recurse from-objects as hash table keys until reaching a non-nil to-object."
	   (let* ((nth-elt         (first   from-objects-remaining))
		  (nth+1-elt       (second  from-objects-remaining))
		  (nth+1-dimension (gethash nth-elt
					    nth-dimension)))
	     (if nth+1-elt
		 (etypecase nth+1-dimension
		   (hash-table (map-get-rec (rest from-objects-remaining)
					    nth+1-dimension))
		   (null       'nil))
		 nth+1-dimension))))

  (defmethod map-get (from-object (map-inst map))
    (map-get-rec (list from-object)
		 (stor map-inst)))

  (defmethod map-get ((from-objects cons) (map-inst map))
    (map-get-rec from-objects
		 (stor map-inst))))

