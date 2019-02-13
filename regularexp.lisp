;;正規表現

;;継承元
(defclass pattern ()
  ((precedence :accessor precedence :initarg :precedence)))
(defmethod bracket ((obj pattern) outer-precedence)
  (if (< (precedence obj) outer-precedence)
      (format nil "( ~a )" (to-s obj))
      (to-s obj)))

;;emptyクラス
(defclass empty (pattern) ())
(defmethod to-s ((obj empty)) (format nil ""))
(defmethod initialize-instance :after ((empty empty) &key)
  (with-slots (precedence) empty
    (setf precedence 3)
    (format t "()")))
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
    (format t "~a" (to-s literal))))
(defmacro make-literal (char)
  `(make-instance 'literal :char ,char))

;;concat(連結)クラス
(defclass concat (pattern)
  ((first :accessor fst :initarg :fst)
   (second :accessor scnd :initarg :scnd)))
(defmethod to-s ((obj concat))
  (format nil "~a~a"
          (bracket (fst obj) (precedence (fst obj)))
          (bracket (scnd obj) (precedence (scnd obj)))))
(defmacro make-concat (fst scnd)
  `(make-instance 'concat :fst ,fst :scnd ,scnd))


;;
