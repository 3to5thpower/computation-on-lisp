;;Finite Automatonの遷移規則
(defstruct (farule
             (:constructor rule (state char next-state)))
  state char next-state)

(defun appliablep (rule state char)
  (and
   (= state (farule-state rule))
   (if (characterp char)
       (if (farule-char rule)
           (char= char (farule-char rule)))
       (if (farule-char rule)
           nil
           t))))
(defun follow (rule) (farule-next-state rule))

(defun follow-free-moves (book states)
  (let ((more-states (next-states book states nil))
        (temp states))
    (if (subsetp more-states temp)
        temp
        (follow-free-moves book (union temp more-states)))))

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
  (let ((curr (nfa-curr-state nfa))
        (acc (nfa-accept-state nfa)))
    (if (intersection curr acc)
      t)))

(defun curr-states (nfa)
  (follow-free-moves (nfa-book nfa) (nfa-curr-state nfa)))

(defun read-character (nfa char)
  (make-nfa (next-states (nfa-book nfa) (curr-states nfa) char)
                      (nfa-accept-state nfa)
                      (nfa-book nfa)))

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
(defclass repeat (pat)
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
