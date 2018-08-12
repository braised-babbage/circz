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
  (unary-node (node-op node)
	      (visit #'copy-circuit (operand node))))
(defmethod copy-circuit ((node binary-node))
  (binary-node (node-op node)
	       (visit #'copy-circuit (left-operand node))
	       (visit #'copy-circuit (right-operand node))))



;;; Evaluator


(defun circuit-value (node input-values)
  "Computes the output value of the specified circuit, given an assignment
of boolean values to the input variables (an alist)."
  (let ((*cache* (make-hash-table)))
    (visit (make-eval-visitor input-values)
	   node)))

(defparameter *operators*
  `((and . ,#'(lambda (x y) (and x y)))
    (or  . ,#'(lambda (x y) (or x y)))
    (not . ,#'(lambda (x)   (not x)))
    (xor . ,#'(lambda (x y) (or (and x (not y))
				(and (not x) y))))))

(defun qand (left right)
  (binary-node 'and left right))

(defun qor (left right)
  (binary-node 'or left right))

(defun qnot (operand)
  (unary-node 'not operand))

(defun qxor (left right)
  (binary-node 'xor left right))

(defun apply-op (op &rest args)
  "Apply an operator from the *operators* table."
  (let ((binding (assoc op *operators*)))
    (if binding
	(apply (cdr (assoc op *operators*)) args)
	(error "op ~a not found" op))))

(defgeneric eval-circuit (node env))

(defmethod eval-circuit ((node constant-node) env)
  (constant-value node))

(defmethod eval-circuit ((node variable-node) env)
  (lookup (variable-name node) env))

(defmethod eval-circuit ((node unary-node) env)
  (let* ((v (make-eval-visitor env))
	 (value (visit v (operand node))))
    (apply-op (node-op node) value)))

(defmethod eval-circuit ((node binary-node) env)
  (let* ((v (make-eval-visitor env))
	 (left (visit v (left-operand node)))
	 (right (visit v (right-operand node))))
    (apply-op (node-op node) left right)))

(defun make-eval-visitor (env)
  #'(lambda (node) (eval-circuit node env)))

(defun lookup (variable environment)
  "Returns the value of the variable in the environment."
  (let ((binding (assoc variable environment)))
    (if binding
	(cdr binding)
	(error (format nil "variable ~a not present in environment" variable)))))


;;; Linearizer


(defparameter *count* 0)
(defparameter *inputs* nil)
(defparameter *instructions* nil)

(defun linearize (node)
  "Converts a circuit into a simple, linearized form. Returns a list of
instructions, the name of the 'output' variable, and the names of the input variables."
  (let ((*count* 0)
	(*inputs* nil)
	(*instructions* nil)
	(*cache* (make-hash-table)))
    (let ((output-name (visit #'linearize-circuit node)))
      (values (reverse *instructions*) output-name (reverse *inputs*)))))

(defun make-name (node)
  "Construct a name for a node."
  (let ((name
	 (format nil "~a_~a"
		 (if (typep node 'variable-node)
		     (string (variable-name node))
		     "t")
		 *count*)))
    (incf *count*)
    ; not sure if this is the right way to produce a symbol
    (intern (string-upcase name))))

(defgeneric linearize-circuit (node))

(defmethod linearize-circuit ((node constant-node))
  (let ((name (make-name node)))
    (progn 
      (push (list name 'const (constant-value node)) *instructions*)
      name)))

(defmethod linearize-circuit ((node variable-node))
  (if (member (variable-name node) *inputs*)
      (error "duplicate variable node: ~a" (variable-name node))
      (let ((name (make-name node)))
	(progn
	  (push (variable-name node) *inputs*)
	  (push (list name 'var (variable-name node)) *instructions*)
	  name))))

(defmethod linearize-circuit ((node unary-node))
  (let* ((operand-name (visit #'linearize-circuit (operand node)))
	 (name (make-name node)))
    (progn
      (push (list name 'unary-op (node-op node) operand-name) *instructions*)
      name)))

(defmethod linearize-circuit ((node binary-node))
  (let* ((left-name (visit #'linearize-circuit (left-operand node)))
	 (right-name (visit #'linearize-circuit (right-operand node)))
	 (name (make-name node)))
    (progn
      (push (list name 'binary-op (node-op node) left-name right-name) *instructions*)
      name)))

;;; Compiler

(defun compile-circuit (node)
  "Compiles the circuit to a lisp function, with input variables as keyword arguments."
  (eval (translate-circuit node)))

(defun translate-circuit (node)
  "Translates a circuit (node) to a lambda expression."
 (multiple-value-bind (instructions output inputs) (linearize node)
    `(lambda (&key ,@inputs)
       (let*
	   ,(mapcar #'translate-instruction instructions)
	 ,output))) )

(defun translate-instruction (instr)
  "Translate an instruction to a (var init-form) list."
  (destructuring-bind (name type . args) instr
    (list name
	  (case type
	    (var       (car args))
	    (const     (car args))
	    (unary-op  args)
	    (binary-op args)))))

;;; todo: what is the unit testing workflow like?


(defparameter *w*
  (let*
      ((x (var 'x))
       (y (var 'y))
       (z (qand x y))
       (w (qor z (qnot x))))
    w))

(defparameter *wfn* (compile-circuit *w*))

(assert (funcall *wfn* :x t :y t))
(assert (funcall *wfn* :x nil :y t))
(assert (funcall *wfn* :x nil :y nil))
(assert (not (funcall *wfn* :x t :y nil)))
