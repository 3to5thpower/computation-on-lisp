;;Finite Automatonの遷移規則
(defstruct (farule
             :constructor make-rules (state char next-state))
  state char next-state)

(defun appliablep (rules state char)
  (and
   (= state (farule-state rules))
   (char= char (farule-char))))
(defun follow (rules) (farule-next-state rules))


(defun show (obj)
  (cond
    ((farule-p obj)
     (format t "rule: ~a ~a -> ~a"
             (farule-state obj)
             (farule-char obj)
             (farule-next-state obj)))))

