;;practice of CLOS
;(defclass foo () ((a :accessor foo-a :initform 1 :initarg :a)))


;;small step semantics------------------------------------------
;;ユーティリティとして用いる関数・マクロ
(defun show (obj)
  (format t "<< ~A >>~%" (to-s obj)))
(defmacro get-slot (slot-name class)
  `(defmethod ,slot-name ((obj ,class))
     (slot-value obj ',slot-name)))

;;numクラス
(defclass num ()
  ((value :accessor value :initarg :value)))
(defmacro make-num (value)
  `(make-instance 'num :value ,value))
(defmethod to-s ((obj num))
  (format nil "~a" (slot-value obj 'value)))
(defmethod reduciblep ((obj num))
  nil)
(get-slot value num)

;;addクラス
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

;;multiplyクラス
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

;;boolクラス
(defclass bool ()
  ((value :accessor value :initarg :value)))
(defmacro make-bool (value)
  `(make-instance 'bool :value ,value))
(defmethod to-s ((obj bool))
  (format nil "~a" (slot-value obj 'value)))
(defmethod reduciblep ((obj bool))
  nil)
(get-slot value bool)

;;lessthanクラス
(defclass lessthan ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro make-lessthan (left right)
  `(make-instance 'lessthan :left ,left :right ,right))
(get-slot right lessthan)
(get-slot left lessthan)
(defmethod to-s ((obj lessthan))
  (format nil "(~a < ~a)"
          (to-s (slot-value obj 'left))
          (to-s (slot-value obj 'right))))
(defmethod reduciblep ((obj lessthan))
  t)
(defmethod reduction ((obj lessthan))
  (cond
    ((reduciblep (left obj))
     (make-lessthan (reduction (left obj)) (right obj)))
    ((reduciblep (right obj))
     (make-lessthan (left obj) (reduction (right obj))))
    (t (make-bool (< (value (left obj)) (value (right obj)))))))


;;抽象機械本体
(defun machine-run (obj)
  (show obj)
  (if (reduciblep obj)
      (machine-run (reduction obj))
      obj))
