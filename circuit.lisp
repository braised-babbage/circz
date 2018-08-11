;;;
;;; circuit.lisp
;;;

(defpackage #:circuit
  (:use :cl :assoc-utils)
  (:export #:input))

(in-package #:circuit)

(defclass node () ())

(defclass variable-node (node)
  ((name
    :initarg :name
    :initform (error ":name must be specified.")
    :reader variable-name)))


(defclass constant-node (node)
  ((value
    :initarg :value
    :initform (error ":value must be specified.")
    :reader constant-value)))

(defclass binary-node (node)
  ((op
    :initarg :op
    :initform (error ":op must be specified.")
    :reader node-op)
   (left
    :initarg :left
    :initform (error ":left must be specified.")
    :accessor left-operand)
   (right
    :initarg :right
    :initform (error ":right must be specified.")
    :accessor right-operand)))

(defclass unary-node (node)
  ((op
    :initarg :op
    :initform (error ":op must be specified.")
    :reader node-op)
   (operand
    :initarg :operand
    :initform (error ":operand must be specified.")
    :accessor operand)))


;;; Some helpful syntax


(defun var (name)
  "Construct a variable node from a name."
  (make-instance 'variable-node :name name))

(defun const (value)
  "Constructs a constant node from a boolean or integer."
  (cond
    ((typep value 'boolean) (make-instance 'constant-node :value value))
    ((integerp value)       (make-instance 'constant-node
					   :value (integer->boolean (mod x 2))))
    (t (error "const expects either an integer or a boolean literal"))))

(defun integer->boolean (x) (= x 1)) 	; not sure of the idomatic way to do this


(defun qand (left right)
  (make-instance 'binary-node
		 :op #'(lambda (x y) (and x y))
		 :left left
		 :right right))

(defun qor (left right)
  (make-instance 'binary-node
		 :op #'(lambda (x y) (or x y))
		 :left left
		 :right right))

(defun qnot (node)
  (make-instance 'unary-node
		 :op #'(lambda (x) (not x))
		 :operand node))


;;; Evaluator

(defun circuit-value (node input-values)
  "Computes the output value of the specified circuit, given an assignment
of boolean values to the input variables (an alist)."
  (eval-circuit node input-values (make-hash-table)))


(defun lookup (variable environment)
  "Returns the value of the variable in the environment."
  (let ((binding (assoc variable environment)))
    (if binding
	(cdr binding)
	(error (format nil "variable ~a not present in environment" variable)))))


(defun eval-circuit (node env cache)
  "Evaluates a circuit node with respect to an environment. If the node value is present
in the cache, we use that value. Otherwise, we add the resulting value to the cache."
  (multiple-value-bind (value present) (gethash node cache)
    (if present
	value
	(setf (gethash node cache)
	      (case (type-of node)
		(constant-node (constant-value node))
		(variable-node (lookup (variable-name node) env))
		(unary-node    (funcall (node-op node)
					(eval-circuit (operand node) env cache)))
		(binary-node   (funcall (node-op node)
					(eval-circuit (left-operand node) env cache)
					(eval-circuit (right-operand node) env cache))))))))


;;; todo: what is the unit testing workflow like?
