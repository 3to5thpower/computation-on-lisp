(ql:quickload 'let-over-lambda)
(use-package 'let-over-lambda)
(defstruct rule curr char next)
(defun rule (curr char next)
  (make-rule :curr curr :char char :next next))

(defun foo ()
  (let ((state 1) (rules nil) (accept-state 3))
    (dlambda
     (:move-with-input (input rule)
            (if (and (char= input (rule-char rule))
                     (= state (rule-curr rule)))
                (setf state (rule-next rule))))
     (:show ()
       (format t "curr-state:~a~%" state)
       (format t "current-rules:~%")
       (loop for r in (reverse rules)
           do (format t "~a -> ~a when '~a'~%"
                    (rule-curr r) (rule-next r) (rule-char r))))
     (:set-state (new) (setf state new))
     (:add-rules (curr char next)
       (if rules
           (push (rule curr char next) rules)
           (setf rules (list (rule curr char next)))))
     (:move (input)
       (loop for r in rules
          when(and (char= input (rule-char r))
                   (= state (rule-curr r)))
          do(setf state (rule-next r)) (return state)))
     (:set-acceptate (acc) (setf accept-state acc)))))

