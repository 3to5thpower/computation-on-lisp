;;正規表現

;;継承元
(defclass pattern ()
  ((precedence :accessor precedence :initarg :precedence)))
(defmethod bracket ((obj pattern) outer-precedence)
  (if (< (precedence obj) outer-precedence)
      (format nil "(~a)" (to-s obj))
      (to-s obj)))

;;emptyクラス
(defclass empty (pattern) ())
(defmethod to-s ((obj empty)) (format nil ""))
(defmethod initialize-instance :after ((empty empty) &key)
  (with-slots (precedence) empty
    (setf precedence 3)
    (format nil "()")))
(defmacro make-empty ()
  `(make-instance 'empty))


;;literalクラス
(defclass literal (pattern)
  ((char :accessor literal-char :initarg :char)))
(defmethod to-s ((obj literal))
  (format nil "~a" (literal-char obj)))
(defmethod initialize-instance :after ((literal literal) &key)
  (with-slots (precedence char) literal
    (setf precedence 3)
    (format nil "~a" (to-s literal))))
(defmacro make-literal (char)
  `(make-instance 'literal :char ,char))

;;concat(連結)クラス
(defclass concat (pattern)
  ((first :accessor fst :initarg :fst)
   (second :accessor scnd :initarg :scnd)))
(defmethod initialize-instance :after ((concat concat) &key)
  (with-slots (precedence fst scnd) concat
    (setf precedence 1)
    (format nil "~a" (to-s concat))))
(defmethod to-s ((obj concat))
  (format nil "~a~a"
          (bracket (fst obj) (precedence (fst obj)))
          (bracket (scnd obj) (precedence (scnd obj)))))
(defmacro make-concat (fst scnd)
  `(make-instance 'concat :fst ,fst :scnd ,scnd))


;;chooseクラス
(defclass choose (pattern)
  ((first :accessor fst :initarg :fst)
   (second :accessor scnd :initarg :scnd)))
(defmethod initialize-instance :after ((choose choose) &key)
  (with-slots (precedence fst scnd) choose
    (setf precedence 0)
    (format nil "~a" (to-s choose))))
(defmethod to-s ((obj choose))
  (format nil "~a|~a"
          (bracket (fst obj) (precedence (fst obj)))
          (bracket (scnd obj) (precedence (scnd obj)))))
(defmacro make-choose (fst scnd)
  `(make-instance 'choose :fst ,fst :scnd ,scnd))

;;repeatクラス
(defclass repeat (pattern)
  ((pattern :accessor pattern :initarg :pattern)))
(defmethod initialize-instance :after ((repeat repeat) &key)
  (with-slots (precedence pattern) repeat
    (setf precedence 2)
    (format nil "~a" (to-s repeat))))
(defmethod to-s ((obj repeat))
  (format nil "~a*" (bracket (pattern obj) (precedence obj))))
(defmacro make-repeat (pattern)
  `(make-instance 'repeat :pattern ,pattern))
