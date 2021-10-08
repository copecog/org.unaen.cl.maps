;;;; org.unaen.cl.maps/maps.lisp

(uiop:define-package #:org.unaen.cl.maps
  (:documentation "Simple Set source for maps.")
  (:shadow #:map)
  (:use #:common-lisp)
  (:export #:*map-test*
           #:map
           #:map-new
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

(defun map-new (&key (dimension 1))
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

(defmethod map-add (to-object from-object (map-inst map))
  (map-add-2 to-object
             (list from-object)
             map-inst))

(defmethod map-add (to-object (from-objects cons) (map-inst map))
  (map-add-2 to-object
             from-objects
             map-inst))

(defun map-add-2 (to-object from-objects map-inst)
  (labels ((map-add-rec (nth-dimension from-objects-remaining)
             (let ((nth-elt (first from-objects-remaining))
                   (nth+1-elt (second from-objects-remaining)))
               (if nth+1-elt                                                    ;IF there is a next object in the list:
                   (let ((nth+1-dimension (gethash nth-elt nth-dimension)))         ;THEN set in current hash-table for key object to reference the next key object hash table
                     (if (null nth+1-dimension)                                         ;IF that next key object hash table doesn't exist:
                         (map-add-rec (setf (gethash nth-elt nth-dimension)                 ;THEN make a hash-table for it and recurse starting at the next object in the list.
                                            (make-hash-table :test *map-test*))
                                      (rest from-objects-remaining))
                         (map-add-rec nth+1-dimension                                       ;ELSE simply recurse with next key object and hash-table.
                                      (rest from-objects-remaining))))
                   (setf (gethash nth-elt nth-dimension)                            ;ELSE we are on the last key object and so finally set the output to-object.
                         to-object)))))
    (values (map-add-rec (stor map-inst) from-objects)
            from-objects
            map-inst)))

(defgeneric map-get (from-object/objects-list map-instance)
  (:documentation "Get the output object mapping for a respective object or objects list."))

(defmethod map-get :before (from-obj/s (map-inst map))
  (when (/= (typecase from-obj/s
              (cons (list-length from-obj/s))
              (atom 1))
            (dimension map-inst))
    (error "Incorrect number of from-objects.")))

(defmethod map-get (from-object (map-inst map))
  (map-get-2 (list from-object)
             map-inst))

(defmethod map-get ((from-objects cons) (map-inst map))
  (map-get-2 from-objects
             map-inst))

(defun map-get-2 (from-objects map-inst)
  (labels ((map-get-rec (nth-dimension from-objects-remaining)
             (let* ((nth-elt (first from-objects-remaining))
                    (nth+1-elt (second from-objects-remaining))
                    (nth+1-dimension (gethash nth-elt nth-dimension)))
               (if nth+1-elt
                   (map-get-rec nth+1-dimension (rest from-objects-remaining))
                   nth+1-dimension))))
    (values (map-get-rec (stor map-inst) from-objects)
            from-objects
            map-inst)))
