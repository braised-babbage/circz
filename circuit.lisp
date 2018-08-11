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

(defun binary-node (op left right)
  (make-instance 'binary-node :op op :left left :right right))
(defun unary-node (op operand)
  (make-instance 'unary-node :op op :operand operand))

(defun qand (left right)
  (binary-node #'(lambda (x y) (and x y)) left right))

(defun qor (left right)
  (binary-node #'(lambda (x y) (or x y)) left right))

(defun qnot (operand)
  (unary-node #'(lambda (x) (not x)) operand))


;;; Node visitors

(defconstant currently-visiting 'visiting)

(defparameter *cache* (make-hash-table))

(defun visit (visitor node)
  (multiple-value-bind (value present) (gethash node *cache*)
    (cond
      ((eq value currently-visiting)
       (error "cyclic node graph detected"))
      (present value)
      (t (progn
	   (setf (gethash node *cache*) currently-visiting)
	   (let ((value (funcall visitor node)))
	     (setf (gethash node *cache*) value)
	     value))))))

;;; Copier

(defun copy (node)
  "Returns a copy of the circuit."
  (let ((*cache* (make-hash-table)))
    (visit #'copy-circuit node)))

(defgeneric copy-circuit (node))

(defmethod copy-circuit ((node constant-node))
  (const (constant-value node)))
(defmethod copy-circuit ((node variable-node))
  (var (variable-name node)))
(defmethod copy-circuit ((node unary-node))
  (unary-node (node-op node) (operand node)))
(defmethod copy-circuit ((node binary-node))
  (binary-node (node-op node) (left-operand node) (right-operand node)))



;;; Evaluator

(defun circuit-value (node input-values)
  "Computes the output value of the specified circuit, given an assignment
of boolean values to the input variables (an alist)."
  (let ((*cache* (make-hash-table)))
    (visit (make-eval-visitor input-values)
	   node)))

(defgeneric eval-circuit (node env))

(defmethod eval-circuit ((node constant-node) env)
  (constant-value node))

(defmethod eval-circuit ((node variable-node) env)
  (lookup (variable-name node) env))

(defmethod eval-circuit ((node unary-node) env)
  (let* ((v (make-eval-visitor env))
	 (value (visit v (operand node))))
    (funcall (node-op node) value)))

(defmethod eval-circuit ((node binary-node) env)
  (let* ((v (make-eval-visitor env))
	 (left (visit v (left-operand node)))
	 (right (visit v (right-operand node))))
    (funcall (node-op node) left right)))

(defun make-eval-visitor (env)
  #'(lambda (node) (eval-circuit node env)))

(defun lookup (variable environment)
  "Returns the value of the variable in the environment."
  (let ((binding (assoc variable environment)))
    (if binding
	(cdr binding)
	(error (format nil "variable ~a not present in environment" variable)))))




;;; todo: what is the unit testing workflow like?

;;;

(defvar *w*
  (let*
      ((x (var 'x))
       (y (var 'y))
       (z (qand x y))
       (w (qor z (qnot x))))
    w))
