;;small step semantics------------------------------------------
;;ユーティリティとして用いる関数・マクロ
(defun show (obj)
  (format t "<< ~A >>~%" (to-s obj)))
(defun push! (obj &optional place)
  (if place
      (push obj place)
      (list obj)))

;;numクラス
(defclass num ()
  ((value :accessor value :initarg :value)))
(defmacro num (value)
  `(make-instance 'num :value ,value))
(defmethod to-s ((obj num))
  (format nil "~a" (slot-value obj 'value)))
(defmethod reduciblep ((obj num)) nil)


;;addクラス
(defclass add ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro add (left right)
  `(make-instance 'add :left ,left :right ,right))
(defmethod to-s ((obj add))
  (format nil "~a + ~a"
                 (to-s (slot-value obj 'left))
                 (to-s (slot-value obj 'right))))
(defmethod reduciblep ((obj add)) t)
(defmethod reduction ((obj add) &optional environment)
  (cond
    ((reduciblep (left obj))
     (add (reduction (left obj) environment) (right obj)))
    ((reduciblep (right obj))
     (add (left obj) (reduction (right obj) environment)))
    (t (num (+ (value (left obj)) (value (right obj)))))))

;;multiplyクラス
(defclass multiply ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro multiply (left right)
  `(make-instance 'multiply :left ,left :right ,right))
(defmethod to-s ((obj multiply))
  (format nil "~a * ~a"
                 (to-s (slot-value obj 'left))
                 (to-s (slot-value obj 'right))))
(defmethod reduciblep ((obj multiply)) t)
(defmethod reduction ((obj multiply) &optional environment)
  (cond
    ((reduciblep (left obj))
     (multiply (reduction (left obj) environment) (right obj)))
    ((reduciblep (right obj))
     (multiply (left obj) (reduction (right obj) environment)))
    (t (num (* (value (left obj)) (value (right obj)))))))

;;boolクラス
(defclass bool ()
  ((value :accessor value :initarg :value)))
(defmacro bool (value)
  `(make-instance 'bool :value ,value))
(defmethod to-s ((obj bool))
  (if (value obj)
      (format nil "true")
      (format nil "false")))
(defmethod reduciblep ((obj bool)) nil)

;;lessthanクラス
(defclass lessthan ()
  ((left :accessor left :initarg :left)
   (right :accessor right :initarg :right)))
(defmacro lessthan (left right)
  `(make-instance 'lessthan :left ,left :right ,right))
(defmethod to-s ((obj lessthan))
  (format nil "(~a < ~a)"
          (to-s (slot-value obj 'left))
          (to-s (slot-value obj 'right))))
(defmethod reduciblep ((obj lessthan)) t)
(defmethod reduction ((obj lessthan) &optional environment)
  (cond
    ((reduciblep (left obj))
     (lessthan (reduction (left obj) environment) (right obj)))
    ((reduciblep (right obj))
     (lessthan (left obj) (reduction (right obj) environment)))
    (t (bool (< (value (left obj)) (value (right obj)))))))

;;varablクラス(変数)
(defclass varabl ()
  ((name :accessor name :initarg :name)))
(defmacro var (name)
  `(make-instance 'varabl :name ',name))
(defmethod to-s ((obj varabl))
  (format nil "~a" (name obj)))
(defmethod reduciblep ((obj varabl)) t)
(defmethod reduction ((obj varabl) &optional environment)
  (cdr (assoc (name obj) environment)))

;;環境を生成する関数(make-env 'x 1 'y 2 ...)のように利用
(defun make-one-env (var val)
  (cons var (num val)))
(defun make-env (var val &rest body)
  (do ((bodylst body (cddr bodylst))
       (varname var (first bodylst))
       (val val (second bodylst))
       (lst nil (cons (make-one-env varname val) lst)))
      ((null bodylst) (nreverse (cons (make-one-env varname val) lst)))))
(defmacro env1 (var val)
  `(make-env ',var ,val))
(defmacro env2 (var1 val1 var2 val2)
  `(nconc (make-env ',var1 ,val1)
          (make-env ',var2 ,val2)))


;;抽象機械本体
(defclass machine ()
  ((statement :accessor statement :initarg :statement)
   (environment :accessor environment :initarg :environment)))
(defmacro make-machine (statement environment)
  `(make-instance 'machine :statement ,statement
                  :environment ,environment))
(defmethod to-s ((obj machine))
  (format nil "~a, ~a" (to-s (statement obj))
          (remove-if #'null
                     (mapcar (lambda (x)
                               (if x (cons (car x) (to-s (cdr x)))))
                             (environment obj)))))
(defmethod machine-step ((obj machine))
  (let* ((reduced ;<-machineオブジェクト
          (reduction (statement obj) (environment obj)))
         (state (statement reduced))
         (env (environment reduced)))
    (make-machine state env)))

(defmethod machine-run ((obj machine))
  (do ((local-obj obj (machine-step local-obj)))
      ((not (reduciblep (statement local-obj)))
       (progn (show local-obj) local-obj))
    (show local-obj)))

;;文(環境を変更する)
;;donothing(簡約しきった文)のクラス
(defclass donothing () ())
(defmacro make-donothing () '(make-instance 'donothing))
(defmethod to-s ((obj donothing))
  (format nil "do-nothing"))
(defmethod reduciblep ((obj donothing)) nil)

;;assign(代入)クラス
(defclass assign ()
  ((name :accessor name :initarg :name)
   (expression :accessor expression :initarg :expression)))
(defmacro set! (name expression)
  `(make-instance 'assign :name ',name :expression ,expression))
(defmethod to-s ((obj assign))
  (format nil "~a = ~a" (name obj) (to-s (expression obj))))
(defmethod reduciblep ((obj assign)) t)
(defmethod reduction ((obj assign) &optional environment)
  (let ((name (name obj)) (exp (expression obj)))
    (if (reduciblep exp)
        (make-machine
         (make-instance 'assign
           :name name :expression (reduction exp environment))
         environment)
        (make-machine
         (make-donothing)
         (if (assoc name environment)
             (loop  for x in environment
                collect (if (eq name (car x))
                            (cons name exp)
                            x))
             (push! (cons `,name exp) environment))))))

;;IFクラス
(defclass if-class ()
  ((condp :accessor condp :initarg :condition)
   (consequence :accessor consequence :initarg :consequence)
   (alternative :accessor alternative :initarg :alternative)))
(defmethod to-s ((obj if-class))
  (format nil "if (~a) { ~a }~%  else { ~a } "
          (to-s (condp obj))
          (to-s (consequence obj))
          (to-s (alternative obj))))
(defmacro if! (condp conseq alter)
  `(make-instance 'if-class
                  :condition ,condp
                  :consequence ,conseq
                  :alternative ,alter))
(defmethod reduciblep ((obj if-class)) t)
(defmethod reduction ((obj if-class) &optional environment)
  (cond
    ((reduciblep (condp obj))
     (make-machine
      (make-if-class (reduction (condp obj) environment)
                     (consequence obj) (alternative obj))
      environment))
    ((eq t (value (condp obj)))
     (make-machine (consequence obj) environment))
    (t (make-machine (alternative obj) environment))))

;;sequenceクラス
(defclass squenc ()
  ((fst :accessor fst :initarg :fst)
   (scnd :accessor scnd :initarg :scnd)))
(defmethod to-s ((obj squenc))
  (format nil "~a;~%   ~a" (to-s (fst obj)) (to-s (scnd obj))))
(defmacro make-squenc (fst scnd)
  `(make-instance 'squenc :fst ,fst :scnd ,scnd))
(defmethod reduciblep ((obj squenc)) t)
(defmethod reduction ((obj squenc) &optional environment)
  (if (equal (class-of (make-donothing))
             (class-of (fst obj)))
      (make-machine
       (scnd obj) environment)
      (let ((machine (reduction (fst obj) environment)))
        (make-machine
         (make-squenc (statement machine) (scnd obj))
         (environment machine)))))

;;whileクラス
(defclass while ()
  ((condp :accessor condp :initarg :condp)
   (body :accessor body :initarg :body)))
(defmacro make-while (condp body)
  `(make-instance 'while :condp ,condp :body ,body))
(defmethod to-s ((obj while))
  (format nil "while (~a) { ~a }"
          (to-s (condp obj)) (to-s (body obj))))
(defmethod reduciblep ((obj while)) t)
(defmethod reduction ((obj while) &optional environment)
  (make-machine
   (make-if-class
    (condp obj)
    (make-squenc (body obj) (make-while (condp obj) (body obj)))
    (make-donothing))
   environment))

;;ビッグステップ意味論
(defmethod evaluater ((obj num) &optional environment)
  (declare (ignore environment)) obj)
(defmethod evaluater ((obj bool) &optional environment)
  (declare (ignore environment)) obj)
(defmethod evaluater ((obj varabl) &optional environment)
  (cdr (assoc (name obj) environment)))
(defmethod evaluater ((obj add) &optional environment)
  (num (+ (value (evaluater (left obj) environment))
               (value (evaluater (right obj) environment)))))
(defmethod evaluater ((obj multiply) &optional environment)
  (num (* (value (evaluater (left obj) environment))
               (value (evaluater (right obj) environment)))))
(defmethod evaluater ((obj lessthan) &optional environment)
  (bool (< (value (evaluater (left obj) environment))
                (value (evaluater (right obj) environment)))))
(defun evaluate (obj &optional environment)
  (show (evaluater obj environment)))

(defmethod evaluater ((obj assign) &optional environment)
  (if environment
      (progn
        (push (car (one-varabl-env (name obj) (evaluate (expression obj))))
              environment)
        environment)
      (one-varabl-env (name obj) (evaluate (expression obj)))))
