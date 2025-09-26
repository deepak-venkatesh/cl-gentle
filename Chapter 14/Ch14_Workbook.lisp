;; Chapter 14 - Macros and Compilation

(macroexpand '(incf a))

(defstruct starship
  (name nil)
  (condition 'green))

(macroexpand 'starship)

(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))

(defmacro set-nil (var)
  (list 'setq var nil))

(defmacro simple-rotatef (a b)
  `(let ((tempa ,b)
	 (tempb ,a))
    (setf ,a tempa)
     (setf ,b tempb)))

(let ((a 2)
      (b 7))
  (simple-rotatef a b)
  (list a b))
