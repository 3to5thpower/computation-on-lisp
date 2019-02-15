;;diterministic push down automaton

;;PDAの構成(状態とStack)を保持する構造
(defclass pda-config ()
  ((state :accessor state :initarg :state)
   (pda-stack :accessor pda-stack :initarg :pda-stack)))
(defmacro config (state stack)
  `(make-instance 'pda-config :state ,state :pda-stack ,stack))

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
(defmethod appliablep ((rule pda-rule) (config pda-config) char)
  (and
   (equal (state rule) (state config))
   (equal (pop-char rule) (first (pda-stack config)))
   (equal (pda-char rule) char)))
