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
  (let ((more-states (next-states book states nil)))
    (if (subsetp more-states states)
        states
        (follow-free-moves book (union states more-states)))))

;;nfaの遷移規則を生成
(defun next-states (book states char)
  (mapcan (lambda (state) (follow-rules-for book state char))
          states))
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
  (if (intersection (nfa-curr-state nfa) (nfa-accept-state nfa))
      t))

(defun curr-states (nfa)
  (sort (follow-free-moves (nfa-book nfa) (nfa-curr-state nfa))
        #'<))

(defun read-character (nfa char)
  (setf (nfa-curr-state nfa)
        (next-states (nfa-book nfa) (curr-states nfa) char)))
(defun read-string (nfa1 string)
  (let ((temp nfa1))
    (map 'list (lambda (c) (read-character temp c)) string)
    (acceptp temp)))
