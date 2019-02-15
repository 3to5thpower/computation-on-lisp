;;diterministic push down automaton

;;Stackの構造体とpush,pop,top
(defstruct stack content)
(defmacro newstack (cont)
  `(make-stack :content ,cont))

(defun sta-push (obj stack)
  (newstack (cons obj (stack-content stack))))
(defun sta-pop (stack)
  (newstack (cdr (stack-content stack))))
(defun top (stack)
  (first (stack-content stack)))

(defmethod to-s ((stack stack))
  (format nil "~a" (stack-content stack)))

;;PDAの構成(状態とStack)を保持する構造
(defclass pda-config ()
  ((state :accessor state :initarg :state)
   (pda-stack :accessor pda-stack :initarg :pda-stack)))
(defmacro config (state stack)
  `(make-instance 'pda-config :state ,state :pda-stack ,stack))
(defmethod to-s ((config pda-config))
  (format nil "~a:~a" (state config) (stack-content (pda-stack config))))

;;PDAの遷移規則
(defclass  pda-rule ()
  ((state :accessor state :initarg :state)
   ;;受け取る文字
   (pda-char :accessor pda-char :initarg :pda-char)
   ;;遷移先の状態
   (next-state :accessor next-state :initarg :next-state)
   ;;popする文字
   (pop-char :accessor pop-char :initarg :pop-char)
   ;;pushする文字のリスト
   (push-chars :accessor push-chars :initarg :push-chars)))

(defmacro rule (state char next pop pushes)
  `(make-instance 'pda-rule :state ,state :pda-char ,char
                  :next-state ,next :pop-char ,pop :push-chars ,pushes))
(defmethod appliablep ((rule pda-rule) config &optional char)
  (and
   (equal (state rule) (state config))
   (equal (pop-char rule) (first (stack-content (pda-stack config))))
   (equal (pda-char rule) char)))
(defmethod to-s ((rule pda-rule))
  (format nil "~a->~a:get ~a pop ~a push ~a~%"
          (state rule) (next-state rule)
          (pda-char rule) (pop-char rule) (push-chars rule)))

;;遷移規則と現在の状態を受け取って新しい状態を返す
(defmethod follow ((rule pda-rule) config)
  (config (next-state rule) (next-stack rule config)))
(defmethod next-stack ((rule pda-rule) config)
  (let ((poped-stack (sta-pop (pda-stack config))))
    (reduce (lambda (char stack) (sta-push char stack))
            (push-chars rule) :from-end t :initial-value poped-stack)))

(defun dpdarule-book (&rest rules)
  rules)

(defun next-config (book config &optional char)
  (follow (rule-for book config char) config))
(defun rule-for (book config &optional char)
  (find-if (lambda (rule) (appliablep rule config char))
           book))

;;dpdaクラス
(defclass dpda ()
  ((curr-config :initarg :config)
   (accept-states :accessor acceptors :initarg :accepts)
   (book :accessor book :initarg :book)))
(defun curr (dpda)
  (follow-free-moves (book dpda) (slot-value dpda 'curr-config)))

(defmacro make-dpda (config accept-states book)
  `(make-instance 'dpda :config ,config
                  :accepts ,accept-states
                  :book ,book))
(defmethod to-s ((dpda dpda))
  (format nil "config:~a~%accept-states:~a~%rule:~a"
          (to-s (curr dpda)) (acceptors dpda) (mapcar #'to-s (book dpda))))
(defmethod acceptingp ((dpda dpda))
  (if (find (state (curr dpda)) (acceptors dpda)) t))

;;文字列を読む
(defun read-character (dpda &optional char)
  (setf (slot-value dpda 'curr-config)
        (next-config (book dpda) (curr dpda) char))
  dpda)
(defun read-string (dpda &optional str)
  (if str
      (dolist (c (coerce str 'list) dpda) (read-character dpda c))
      (read-character dpda nil)))

;;自由移動をサポートする
(defun book-appliablep (book config &optional char)
  (if (rule-for book config char) t))
(defun follow-free-moves (book config)
  (if (book-appliablep book config)
      (follow-free-moves book (next-config book config nil))
      config))
