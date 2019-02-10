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
(defun make-book (n charlst)
  (apply #'makebook-fromlst (make-arglst n charlst)))


(defun show (obj)
  (cond
    ((farule-p obj)
     (format t "rule: ~a ~a -> ~a"
             (farule-state obj)
             (farule-char obj)
             (farule-next-state obj)))
    (t (format t "~a" obj))))

