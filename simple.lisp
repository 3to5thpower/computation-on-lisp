;;practice of CLOS
;(defclass foo () ((a :accessor foo-a :initform 1 :initarg :a)))


;;small step semantics

(defclass num ()
  ((value :accessor value :initarg :value)))
(defmacro make-num (value)
  `(make-instance 'num :value ,value))

(defclass add ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro make-add (left right)
  `(make-instance 'add :left ,left :right ,right))

(defclass multiply ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro make-multiply (left right)
  `(make-instance 'multiply :left ,left :right ,right))

(defun to-s (obj)
  (typecase obj
    (num (format nil "<~a>" (slot-value obj 'value)))
    (add (format nil "~a + ~a"
                 (to-s (slot-value obj 'left))
                 (to-s (slot-value obj 'right))))
    (multiply (format nil "~a * ~a"
                      (to-s (slot-value obj 'left))
                      (to-s (slot-value obj 'right))))))
