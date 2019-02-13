;;Finite Automatonの遷移規則
(defstruct (farule
             (:constructor rule (state char next-state)))
  state char next-state)

(defun appliablep (rule state char)
  (and
   (= state (farule-state rule))
   (char= char (farule-char rule))))
(defun follow (rule) (farule-next-state rule))

;;nfaの遷移規則を生成
(defun next-states (book states char)
  (mapcan (lambda (state) (follow-rules-for book state char))
           states))
(defun follow-rules-for (rules state char)
  (mapcar #'follow (rules-for rules state char)))
(defun rules-for (rules state char)
  (remove-if-not (lambda (rule) (appliablep rule state char))
                 rules))
