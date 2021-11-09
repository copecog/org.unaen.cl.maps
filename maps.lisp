;;;; org.unaen.cl.maps/maps.lisp

(uiop:define-package #:org.unaen.cl.maps
  (:documentation "Simple Set source for maps.")
  (:shadow #:map)
  (:use #:common-lisp)
  (:export #:*map-test*
           #:map
	   #:mapp
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

(defgeneric mapp (object)
  (:method ((map map))
    t)
  (:method (object)
    nil))

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
	   (let ((nth-elt (first from-objects-remaining))
		 (nth+1-elt (second from-objects-remaining)))
	     (if nth+1-elt
		 (let ((nth+1-dimension (gethash nth-elt
						 nth-dimension)))
		   (etypecase nth+1-dimension
		     (hash-table (map-add-rec to-object
					      (rest from-objects-remaining)
					      nth+1-dimension))
		     (null (map-add-rec to-object
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
	   (let* ((nth-elt (first from-objects-remaining))
		  (nth+1-elt (second from-objects-remaining))
		  (nth+1-dimension (gethash nth-elt
					    nth-dimension)))
	     (if nth+1-elt
		 (etypecase nth+1-dimension
		   (hash-table (map-get-rec (rest from-objects-remaining)
					    nth+1-dimension))
		   (null 'nil))
		 nth+1-dimension))))

  (defmethod map-get (from-object (map-inst map))
    (map-get-rec (list from-object)
		 (stor map-inst)))

  (defmethod map-get ((from-objects cons) (map-inst map))
    (map-get-rec from-objects
		 (stor map-inst))))

(defmethod map-find-element (element (map map) &key test)
  "Given an object and map stor and dimension with keyword test function, find and return first object for which (test element object) is true."  
  (unless test
    (setf test #'equal))
  (with-slots ((stor stor) (dimension dimension)) map
    (catch 'found (map-find-element-rec element stor dimension test))))

(defun map-find-element-rec (element stor dimension test)
  (declare (type hash-table stor)
	   (type (integer 0 *) dimension)
	   (type function test))
  (cond ((>= dimension 2) (loop :for object :being :the :hash-values :of stor
				:do (map-find-element-rec element object (1- dimension) test)))
	((=  dimension 1) (loop :for object :being :the :hash-values :of stor
				:do (when (funcall test element object) (throw 'found object))))))
				      
(defun %map-find-element (element stor dimension test)
  (declare (type hash-table stor)
	   (type (integer 0 *) dimension)
	   (type function test))
  (labels ((test-or-recursion ()
	     (cond ((>= dimension 2) #'(lambda (object); object passed will be next dimension hash-table
					 (%map-find-element element object (1- dimension) test)))
		   ((=  dimension 1) #'(lambda (object)
					 (when (funcall test element object)
					   (throw 'found object))))))
	   (loop-current-dimension (test)
	     (loop :for object :being :the :hash-values :of stor
		   :do (funcall test object))))
    (loop-current-dimension (test-or-recursion))))
