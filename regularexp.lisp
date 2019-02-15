;;ユーティリティ関数
(defun flat (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (if (listp (car lst))
                     (append (car lst) (rec (cdr lst) acc))
                     (cons (car lst) (rec (cdr lst) acc))))))
    (rec lst nil)))
;;Finite Automatonの遷移規則
(defstruct (farule
             (:constructor rule (state char next-state)))
  state char next-state)

(defun appliablep (rule state char)
  (and
   (equal state (farule-state rule))
   (if (characterp char)
       (if (farule-char rule)
           (equal char (farule-char rule)))
       (if (farule-char rule)
           nil
           t))))
(defun follow (rule) (farule-next-state rule))

(defun follow-free-moves (book states)
  (if (subsetp (next-states book states nil) states)
      states
      (follow-free-moves book (union states (next-states book states nil)))))

;;nfaの遷移規則を生成
(defun next-states (book states char)
  (apply #'append (mapcar (lambda (state) (follow-rules-for book state char))
                          states)))
(defun follow-rules-for (rules state char)
  (mapcar #'follow (rules-for rules state char)))
(defun rules-for (rules state char)
  (remove-if-not (lambda (rule) (appliablep rule state char))
                 rules))

;;nfa
(defstruct (nfa
             (:constructor make-nfa (curr-state accept-state book)))
  curr-state accept-state book)
(defun acceptp (nfa)
  (if (intersection (curr-states nfa) (nfa-accept-state nfa))
      t))

(defun curr-states (nfa)
  (flat (follow-free-moves (nfa-book nfa) (nfa-curr-state nfa))))

(defun read-character (nfa char)
  (if char
      (make-nfa
       (curr-states
        (make-nfa (next-states (nfa-book nfa) (curr-states nfa) char)
                  (nfa-accept-state nfa)
                  (nfa-book nfa)))
       (nfa-accept-state nfa)
       (nfa-book nfa))
      (copy-nfa nfa)))

(defun read-string (nfa string)
  (let ((temp (copy-nfa nfa)))
    (acceptp (reduce (lambda (nfa c) (read-character nfa c))
                     string :initial-value temp))))


;;----------------------------------------------------------------------nfa.lisp
;;正規表現

(defun show (pattern)
  (format t "/~a/" (to-s pattern)))
;;継承元
(defclass pattern ()
  ((precedence :accessor precedence :initarg :precedence)))
(defmethod bracket ((obj pattern) outer-precedence)
  (if (< (precedence obj) outer-precedence)
      (format nil "(~a)" (to-s obj))
      (to-s obj)))
(defmethod matchesp ((obj pattern) str)
  (read-string (to-nfa obj) str))

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
          (bracket (fst obj) (precedence obj))
          (bracket (scnd obj) (precedence obj))))
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
  ((pat :accessor pat :initarg :pat)))
(defmethod initialize-instance :after ((repeat repeat) &key)
  (with-slots (precedence pat) repeat
    (setf precedence 2)
    (format nil "~a" (to-s repeat))))
(defmethod to-s ((obj repeat))
  (format nil "~a*" (bracket (pat obj) (precedence obj))))
(defmacro make-repeat (pat)
  `(make-instance 'repeat :pat ,pat))

;;意味論
(defclass state () ())
(define-symbol-macro object (make-instance 'state))


;;emptyとliteralに対してNFAを生成するメソッド

(defmethod to-nfa ((obj empty))
  (let* ((start-state (list object))
         (accept-state start-state)
         (book nil))
    (make-nfa start-state accept-state book)))
(defmethod to-nfa ((obj literal))
  (let* ((start object) (accept object)
         (rule (rule start (literal-char obj) accept))
         (book (list rule)))
    (make-nfa (list start) (list accept) book)))

;;concatに対してNFAを生成するメソッド
(defmethod to-nfa ((obj concat))
  (let* ((first (to-nfa (fst obj)))
         (second (to-nfa (scnd obj)))
         (start (nfa-curr-state first))
         (accept (nfa-accept-state second))
         (rules (union (nfa-book first) (nfa-book second)))
         (exrules
          (mapcar (lambda (state) (rule state nil (curr-states second)))
                  (nfa-accept-state first)))
         (book (union rules exrules)))
    (make-nfa start accept book)))

;;chooseに対してNFAを生成するメソッド
(defmethod to-nfa ((obj choose))
  (let* ((first (to-nfa (fst obj)))
         (second (to-nfa (scnd obj)))
         (start object)
         (accept (union (nfa-accept-state first)
                        (nfa-accept-state second)))
         (rules (union (nfa-book first) (nfa-book second)))
         (exrules
          (mapcar (lambda (nfa) (rule start nil (nfa-curr-state nfa)))
                  (list first second)))
         (book (union rules exrules)))
    (make-nfa (list start) accept book)))

;;repeatに対してNFAを生成するメソッド
(defmethod to-nfa ((obj repeat))
  (let* ((pattern (to-nfa (pat obj)))
         (start object)
         (accept (union (list start) (nfa-accept-state pattern)))
         (rules (nfa-book pattern))
         (exrules
          (union
           (mapcar (lambda (acc) (rule acc nil (nfa-curr-state pattern)))
                   (nfa-accept-state pattern))
           (list (rule start nil (nfa-curr-state pattern)))))
         (book (union rules exrules)))
    (make-nfa (list start) accept book)))
