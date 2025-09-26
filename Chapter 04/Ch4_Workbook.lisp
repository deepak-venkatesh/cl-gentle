;;Chapter 4 - Conditionals


(defun make-even (x)
  (if (evenp x) x (+ 1 x)))

(defun further (x)
  (if (< x 0) (- x 1) (+ x 1)))

(defun my-not (x)
  (if x nil t))

(defun ordered (x y)
  (if (> x y) (list y x) (list x y)))

(defun my-abs-if (x)
  (if (< x 0) (- x) x))

;;C-c M-o for clearing repl in slime

(defun my-abs-cond (x)
  (cond ((< x 0) (- x))
	((> x 0) x)))

(defun emph3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t (cons 'very x))))

(defun constrain1 (x min max)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(defun constrain2 (x min max)
  (if (< x min) min
      (if (> x max) max x)))

(defun firstzero (x)
  (cond ((equal (first x) 0) 'first)
	((equal (second x) 0) 'second)
	((equal (third x) 0) 'third)
	(t 'none)))

(defun cycle (x)
  (if (not (equalp x 99)) (+ x 1) 1))

(defun howcompute (x y z)
  (cond ((equalp z (+ x y)) 'sum-of)
	((equalp z (- x y)) 'difference-of)
        ((equalp z (* x y)) 'product-of)
        (t '(beats me))))

(defun geq (x y)
  (or (> x y) (equalp x y)))

(defun squareeo (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
	((and (oddp x) (< x 0)) (* x 2))
	(t (/ x 2))))

;; M-p is the previous input in the slime repl

(defun check-name (x y)
  (or (and (or (equal x 'boy) (equal x 'girl)) (equal y 'child))
      (and (or (equal x 'man) (equal x 'woman)) (equal y 'adult))))

(defun play (x y)
  (cond ((equal x 'rock) (cond ((equal y 'scissors) 'first-wins)
			       ((equal y 'paper) 'second-wins)
			       ((equal y 'rock) 'tie)))
	((equal x 'paper) (cond ((equal y 'scissors) 'second-wins)
				((equal y 'paper) 'tie)
				((equal y 'rock) 'first-wins)))
	((equal x 'scissors) (cond ((equal y 'scissors) 'tie)
				   ((equal y 'paper) 'first-wins)
				   ((equal y 'rock) 'second-wins)))))

(defun compareif (x y)
  (if (equal x y) 'numbers-are-same
      (if (< x y) 'first-is-smaller 'first-is-bigger)))

(defun compareandor (x y)
  (or (and (equal x y) 'numbers-are-same)
      (and (< x y) 'first-is-smaller)
      'first-is-bigger))

(defun gtestif (x y)
  (if (> x y) t
      (if (zerop x) t (zerop y))))

(defun gtestcond (x y)
  (cond((> x y) t)
       ((zerop x) t)
       ((zerop y) t)
       (t nil)))

(defun boilingp (temp scale)
  (cond ((equal scale 'f) (> temp 212))
	((equal scale 'c) (> temp 100))))

(defun boilingifp (temp scale)
  (if (equal scale 'f) (> temp 212)
      (if (equal scale 'c) (> temp 100) nil)))

(defun boilingandor (temp scale)
  (or (and (equal scale 'f) (> temp 212))
      (and (equal scale 'c) (> temp 100))))

(defun if-andor (x y)
  (if (oddp x) (evenp y) 'foo))

(defun logical-and (x y)
  (and x y t))

(defun logical-and-if (x y)
  (if x (if y t)))

(defun logical-and-cond (x y)
  (cond (x (cond (y t)))))

(defun logical-or (x y)
  (cond (x t)
	(y t)
	(t nil)))
