;;practice of CLOS
;(defclass foo () ((a :accessor foo-a :initform 1 :initarg :a)))


;;small step semantics
(defun show (obj)
  (format nil "<<~A>>" (to-s obj)))
(defmacro get-slot (slot-name class)
  `(defmethod ,slot-name ((obj ,class))
     (slot-value obj ',slot-name)))


(defclass num ()
  ((value :accessor value :initarg :value)))
(defmacro make-num (value)
  `(make-instance 'num :value ,value))
(defmethod to-s ((obj num))
  (format nil "~a" (slot-value obj 'value)))
(defmethod reduciblep ((obj num))
  nil)
(get-slot value num)

(defclass add ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro make-add (left right)
  `(make-instance 'add :left ,left :right ,right))
(defmethod to-s ((obj add))
  (format nil "~a + ~a"
                 (to-s (slot-value obj 'left))
                 (to-s (slot-value obj 'right))))
(get-slot right add)
(get-slot left add)
(defmethod reduciblep ((obj add))
  t)
(defmethod reduction ((obj add))
  (cond
    ((reduciblep (left obj))
     (make-add (reduction (left obj)) (right obj)))
    ((reduciblep (right obj))
     (make-add (left obj) (reduction (right obj))))
    (t (make-num (+ (value (left obj)) (value (right obj)))))))


(defclass multiply ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro make-multiply (left right)
  `(make-instance 'multiply :left ,left :right ,right))
(defmethod to-s ((obj multiply))
  (format nil "~a * ~a"
                 (to-s (slot-value obj 'left))
                 (to-s (slot-value obj 'right))))
(defmethod reduciblep ((obj multiply))
  t)
(get-slot right multiply)
(get-slot left multiply)
(defmethod reduction ((obj multiply))
  (cond
    ((reduciblep (left obj))
     (make-multiply (reduction (left obj)) (right obj)))
    ((reduciblep (right obj))
     (make-multiply (left obj) (reduction (right obj))))
    (t (make-num (* (value (left obj)) (value (right obj)))))))

(defclass boolean ()
  ((value :accessor :initarg ::value)))
(defmacro make-boolean (value)
  `(make-instance 'boolean :value ,value))
(defmethod to-s ((obj boolean))
  (format nil "~a" (slot-value obj 'value)))
(defmethod reduciblep ((obj boolean))
  nil)




(defun machine-run (obj)
  (format t "~a~%" (show obj))
    (if (reduciblep obj)
        (machine-run (reduction obj))
        obj))
