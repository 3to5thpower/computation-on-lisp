;;practice of CLOS
;(defclass foo () ((a :accessor foo-a :initform 1 :initarg :a)))


;;small step semantics------------------------------------------
;;ユーティリティとして用いる関数・マクロ
(defun show (obj)
  (format t "<< ~A >>~%" (to-s obj)))

;;numクラス
(defclass num ()
  ((value :accessor value :initarg :value)))
(defmacro make-num (value)
  `(make-instance 'num :value ,value))
(defmethod to-s ((obj num))
  (format nil "~a" (slot-value obj 'value)))
(defmethod reduciblep ((obj num))
  nil)


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
(defmethod reduciblep ((obj add))
  t)
(defmethod reduction ((obj add) &optional environment)
  (cond
    ((reduciblep (left obj))
     (make-add (reduction (left obj) environment) (right obj)))
    ((reduciblep (right obj))
     (make-add (left obj) (reduction (right obj) environment)))
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
(defmethod reduction ((obj multiply) &optional environment)
  (cond
    ((reduciblep (left obj))
     (make-multiply (reduction (left obj) environment) (right obj)))
    ((reduciblep (right obj))
     (make-multiply (left obj) (reduction (right obj) environment)))
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

;;lessthanクラス
(defclass lessthan ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro make-lessthan (left right)
  `(make-instance 'lessthan :left ,left :right ,right))
(defmethod to-s ((obj lessthan))
  (format nil "(~a < ~a)"
          (to-s (slot-value obj 'left))
          (to-s (slot-value obj 'right))))
(defmethod reduciblep ((obj lessthan))
  t)
(defmethod reduction ((obj lessthan) &optional environment)
  (cond
    ((reduciblep (left obj))
     (make-lessthan (reduction (left obj) environment) (right obj)))
    ((reduciblep (right obj))
     (make-lessthan (left obj) (reduction (right obj) environment)))
    (t (make-bool (< (value (left obj)) (value (right obj)))))))

;;varablクラス(変数)
(defclass varabl ()
  ((name :accessor name :initarg :name)))
(defmacro make-varabl (name)
  `(make-instance 'varabl :name ',name))
(defmethod to-s ((obj varabl))
  (format nil "~a" (name obj)))
(defmethod reduciblep ((obj varabl))
  t)
(defmethod reduction ((obj varabl) &optional environment)
  (cdr (assoc (name obj) environment)))

;;環境を生成する関数(make-env 'x 1 'y 2 ...)のように利用
(defun make-one-env (var val)
  (cons var (make-num val)))
(defun make-env (var val &rest body)
  (do ((bodylst body (cddr bodylst))
       (varname var (first bodylst))
       (val val (second bodylst))
       (lst nil (cons (make-one-env varname val) lst)))
      ((null bodylst) (nreverse (cons (make-one-env varname val) lst)))))
(defmacro one-varabl-env (var val)
  `(make-env ',var ,val))
(defmacro two-varabl-env (var1 val1 var2 val2)
  `(nconc (make-env ',var1 ,val1)
          (make-env ',var2 ,val2)))

;;文(環境を変更する)
;;donothing(簡約しきった文)のクラス
(defclass donothing () ())
(defmacro make-donothing () '(make-instance 'donothing))
(defmethod to-s ((obj donothing))
  (format nil "do-nothing"))
(defmethod reduciblep ((obj donothing))
  nil)

;;assign(代入)クラス



;;抽象機械本体
(defun machine-run (obj &optional environment)
  (let ((obj obj))
    (show obj)
    (if (reduciblep obj)
        (machine-run (reduction obj environment) environment)
        obj)))
