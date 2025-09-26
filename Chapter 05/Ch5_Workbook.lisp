;; Chapter 5 Variables and Side Effects

(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(defun good-style (q)
  (let ((var-1 (+ q 5)))
    (list 'result 'is var-1)))

(defun throw-die ()
  (+ 1 (random 6)))

(defun throw-dice ()
  (list (throw-die) (throw-die)))

;;emacs move to next window with C-x o

(defun snake-eyes-p (throw)
  (equal throw '(1 1)))

(defun boxcars-p (throw)
  (equal throw '(6 6)))

(defun instant-win2-p ()
  (or (equal 7 (+ (car (throw-dice)) (car (cdr (throw-dice)))))
      (equal 11 (+ (car (throw-dice)) (car (cdr (throw-dice)))))))

(defun throw-value (throw)
  (+ (first throw) (second throw)))

;; M-w for copy and C-y for paste

(defun instant-win2 ()
  (list (+ (car (throw-dice)) (car (cdr (throw-dice))))
	(+ (car (throw-dice)) (car (cdr (throw-dice))))))

(defun instant-win-p (throw)
  (member (throw-value throw) '(7 11)))

(defun instant-loss-p (throw)
  (member (throw-value throw) '(2 3 12)))

(defun say-throw (throw)
  (cond ((equal (throw-value throw) 2) 'SNAKE-EYES)
	((equal (throw-value throw) 12) 'BOXCARS)
	(t (throw-value throw))))

(defun crap3 (throw)
  (cond ((snake-eyes-p throw) (list 'THROW (first throw) 'AND (second throw) '-- 'SNAKEYES '-- 'YOU 'LOSE))
	((boxcars-p throw) (list 'THROW (first throw) 'AND (second throw) '-- 'BOXCARS '-- 'YOU 'LOSE))
	((instant-loss-p throw) (list 'THROW (first throw) 'AND (second throw) '-- 'YOU 'LOSE))
	((instant-win-p throw) (list 'THROW (first throw) 'AND (second throw) '-- 'YOU 'WIN))
	(t (list 'THROW (first throw) 'AND (second throw) '-- 'YOUR 'POINT 'IS (throw-value throw)))))

(defun craps2 (throw)
  (cond ((snake-eyes-p throw) (list 'THROW (first throw) 'AND (second throw) '-- (say-throw throw) '-- 'YOU 'LOSE))
	((boxcars-p throw) (list 'THROW (first throw) 'AND (second throw) '--  (say-throw throw) '-- 'YOU 'LOSE))
	((instant-loss-p throw) (list 'THROW (first throw) 'AND (second throw) '-- 'YOU 'LOSE))
	((instant-win-p throw) (list 'THROW (first throw) 'AND (second throw) '-- 'YOU 'WIN))
	(t (list 'THROW (first throw) 'AND (second throw) '-- 'YOUR 'POINT 'IS (throw-value throw)))))

(defun craps ()
  (let ((throw (throw-dice)))  ;;important use of let over let*
    (append
     (list 'throw (first throw)
	   'and (second throw)
	   '--
	   (say-throw throw)
	   '--)
     (cond ((instant-win-p throw) '(you win))
	   ((instant-loss-p throw) '(you lose))
	   (t (list 'your 'point 'is
		    (throw-value throw)))))))

(defun try-for-point (point)
  (let* ((throw (throw-dice))    ;;important use of let* over let
	 (val (throw-value throw)))
    (append
     (list 'throw (first throw)
	   'and (second throw)
	   '--
	   (say-throw throw)
	   '--)
     (cond ((equal val point) '(you win))
	   ((equal val 7) '(you lose))
	   (t '(throw again))))))
