;;Finite Automatonの遷移規則
(defstruct (farule
             (:constructor make-rule (state char next-state)))
  state char next-state)

(defun appliablep (rule state char)
  (and
   (= state (farule-state rule))
   (char= char (farule-char rule))))
(defun follow (rule) (farule-next-state rule))
;;DFAの遷移規則の集合は遷移規則のリストを持つ
;;(setf book (list (make-rule state char next-state) &rest rest))
(defun rule-for (book state char)
  (find-if (lambda (rule) (appliablep rule state char)) book))
(defun nextstate(book state char)
  (follow (rule-for book state char)))

;;遷移規則のリストを生成する関数
(defun makebook-fromlst (lst &rest rest)
  (labels ((rec (acc lst rest)
             (let ((rulelst (cons (make-rule (first lst)
                                             (second lst)
                                             (third lst))
                                  acc)))
               (if (null rest)
                   rulelst
                   (rec rulelst (car rest) (cdr rest))))))
    (rec nil lst rest)))
(defun make-arglst (n charlst)
  (loop for i from 1 to n
     append (loop for char in charlst
                 do (format t "~a:'~a'->?" i char)
               collect (list i char (read)))))
(defun make-charlst (maxchar)
  (let ((maxcode (- (char-code maxchar) 97)))
    (loop for i to maxcode
         collect (code-char (+ i 97)))))
(defun make-book-fun (n maxchar)
  (apply #'makebook-fromlst (make-arglst n (make-charlst maxchar))))
(defmacro make-book (n symbolchar)
  `(let ((maxchar (char-downcase (char (symbol-name ',symbolchar) 0))))
     (make-book-fun ,n maxchar)))


;;dfa
(defstruct (dfa
             (:constructor make-dfa (current-state accept-states book)))
  current-state accept-states book)

(defun acceptingp (dfa)
  (if(find (dfa-current-state dfa) (dfa-accept-states dfa))
     t))
(defun reset-dfa (dfa)
  (setf (dfa-current-state dfa) 1)
  dfa)
(defun read-character (dfa char)
  (setf (dfa-current-state dfa) (nextstate (dfa-book dfa)
                                           (dfa-current-state dfa)
                                           char)))
(defun read-str (dfa str)
  (mapc (lambda (c) (read-character dfa c))
        (map 'list (lambda (x) x) str))
  dfa)
(defun dfa-run (dfa str)
  (let ((temp dfa))
    (reset-dfa temp)
    (acceptingp (read-str temp str))))



