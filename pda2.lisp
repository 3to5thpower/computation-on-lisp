(defpackage my-pda
  (:use common-lisp)
  (:nicknames pda)
  (:export push pop))

(defparameter stack nil)

;;stack操作用関数
(defun push (stack obj)
  (if stack
      (cons (car stack) (push (cdr stack) obj))
      (cons obj nil)))
(defun pop (stack)
  (let ((temp (reverse stack)))
    (nreverse (apply #'make-stack (cdr temp)))))
(defun top (stack)
  (if (= 1 (length stack))
      (car stack) (top (cdr stack))))
(defun make-stack (obj &rest rest)
  (if rest
      (cons obj (apply #'make-tack rest))
      (list obj)))

;;状態とスタックの組を構成(config)とする
(defstruct config
  state
  (stack nil :type list))
;;PDAの遷移規則
(defstruct rule
  state
  char
  next-state
  pop-char
  push-chars)
(defun rule-appliablep(rule config char)
  (and (= (rule-state rule) (config-state config))
       (char= (rule-pop-char rule) (top (config-stack config)))
       (char= (rule-char rule) char)))

(let ())
