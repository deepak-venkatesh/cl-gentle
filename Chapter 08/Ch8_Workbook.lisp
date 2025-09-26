;; Chapter 8 - Recursion

(defun anyoddp (x)
  (cond ((null x) nil)
	((oddp (first x)) t)
	(t (anyoddp (rest x)))))

(defun anyoddp-if (x)
  (if x
      (if (oddp (first x))
	  t
	  (anyoddp-if (rest x)))))

;; Using If:
;; The condition part of an if evaluates to true if the expression is not nil (anything else is considered true, including t, numbers, strings, and non-empty lists).When only the test condition is given and no else
;; When x is a non-empty list, (if x ...) proceeds to evaluate the "then" branch of the if
;; When x is an empty list, (if x ...) evaluates the "else" branch, which, in this case, is missing. If the "else" branch is missing, the if expression implicitly returns nil

(defun fact (n)
  (cond ((zerop n) 1)
	(t (* n (fact (- n 1))))))

(defun laugh (n)
  (cond ((zerop n) nil)
	(t (cons 'ha (laugh (- n 1))))))

(defun add-up (nums)
  (cond ((null nums) 0)
	(t (+ (first nums) (add-up (rest nums))))))

(defun allodd (x)
  (cond ((null x) t)                     ;; for an empty list it will give t
	((not (oddp (first x))) nil)     ;; can use evenp here
	(t (allodd (rest x)))))

(defun member-recursion (mem x)
  (cond ((null x) nil)
	((equal mem (first x)) x)
	(t (member-recursion mem (rest x)))))

(setf note-table
      '((C 1)
       (C# 2)
       (D 3)
       (D# 4)
       (E 5)
       (F 6)
       (F# 7)
       (G 8 )
       (G# 9)
       (A 10)
       (A# 11)
	(B 12)))

(defun rec-assoc (mem x)
  (cond ((null x) nil)
	((equal mem (first (first x))) (first x))
	(t (rec-assoc mem (rest x)))))

(defun rec-nth (n x)
  (cond ((zerop n) (first x))
	(t (rec-nth (- n 1) (rest x)))))

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
	(t (rec-plus (add1 x) (sub1 y)))))

;; C-c C-c (Interrupt the Evaluation)
;; C-c C-b (Abort All Background Operations)

;; I should write fizzbuzz using recursion after chapter 9 which will teach me how to print in Common Lisp

(defun fib (n)
  (cond ((equal n 0) 1)
	((equal n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))

;; no base case of NIL such as (null (first x) nil)
(defun any-7-p (x)
  (cond ((equal (first x) 7) t)
	(t (any-7-p (rest x)))))

(defun find-first-odd (x)
  (cond ((null x) nil)
	((oddp (first x)) (first x))
	(t (find-first-odd (rest x)))))

(defun last-element (x)
  (cond ((atom (cdr x)) (car x))
	(t (last-element (cdr x)))))

(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n (add-nums (- n 1))))))

(defun all-equal (x)
  (cond ((null (rest x)) t)
	 ((not (equal (first x) (second x))) nil)
	 (t (all-equal (rest x)))))

(defun count-down (n)
  (cond ((zerop n) nil)
	(t (cons n (count-down (- n 1))))))

(defun fact-countdown (n)
  (reduce #'* (count-down n)))

(defun laugh (n)
  (cond ((zerop n) nil)
	(t (cons 'ha (laugh (- n 1))))))

(defun count-down-zero (n)
  (cond ((zerop (+ n 1)) nil)
	(t (cons n (count-down-zero (- n 1))))))

(defun square-list (x)
  (cond((null x) nil)
	(t (cons (* (first x) (first x)) (square-list (rest x))))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	(t (my-nth (- n 1) (rest x)))))

(defun my-nth-stop (n x)
  (cond ((null x) nil)
	((zerop n) (first x))
	(t (my-nth (- n 1) (rest x)))))

(defun my-member (n x)
  (cond ((null x) nil)
	((equal n (first x)) x)
	(t (my-member n (rest x)))))

;; flatten a list
;; example (a (b (c d) e)))

(defun flatten (x)
  (cond ((null x) nil)
        ((atom x) (list x))
        (t (append (flatten (car x)) (flatten (cdr x))))))

(defun my-assoc (key table)
  (cond ((null table) nil)
	((equal key (first (first table))) (first table))
	(t (my-assoc key (rest table)))))

(defun compare-lengths (x y)
  (cond ((and (null x) (null y) 'same-length))
	((null x) 'second-is-longer)
	((null y) 'first-is-longer)
	(t (compare-lengths (rest x) (rest y)))))

(defun sum-numeric-elements (x)
  (cond ((null x) 0)
	((numberp (first x))
	 (+ (first x) (sum-numeric-elements (rest x))))
	(t (sum-numeric-elements (rest x)))))

(defun my-remove (n x)
  (cond ((null x) nil)
	((equal n (first x))
	 (my-remove n (rest x)))
	(t (cons (first x) (my-remove n (rest x))))))

(defun my-intersection (a b)
  (cond ((null a) nil)
	((member (first a) b)
	 (cons (first a) (my-intersection (rest a) b)))
	(t (my-intersection (rest a) b))))

(defun my-set-difference (a b)
  (cond ((null a) nil)
	((not (member (first a) b))
	 (cons (first a) (my-set-difference (rest a) b)))
	(t (my-set-difference (rest a) b))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	(t (my-nth (- n 1) (rest x)))))

(defun count-odd (x)
  (cond ((null x) 0)
	((oddp (first x))
	      (+ 1 (count-odd (rest x))))
	(t (count-odd (rest x)))))



