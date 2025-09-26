;; Chapter 7 - Applicative Programming

(defun add1 (x)
  (+ 1 x))

 (mapcar #'add1 '(1 3 5 7 9))

(setf daily-planet
      '((olsen jimmy 123-76-4535 cub-reporter)
	(kent clark 089-52-6787)))

(mapcar #'third daily-planet)

(mapcar #'zerop '(2 0 3 4 0 -5 -6))

(defun greater-than-five-p (x)
  (if (> x 5) t nil))

(mapcar #'greater-than-five-p '(2 0 3 4 0 -5 -6 7 8 9 2))

(mapcar #'(lambda (n) (* n n)) '(1 2 3 4 5))

(mapcar #'(lambda (n) (- n 7)) '(9 8 7))

(mapcar #'(lambda (x)
	    (cond ((or (equal x t) (equal x nil)) t)
		  (t nil))) '(t nil 67389 aksjdhd 0hjahj!! &*&% nil t abc))

(mapcar #'(lambda (x)
	    (cond ((equal x 'up) 'down)
		  ((equal x 'down) 'up))) '(up down up up))

(find-if #'oddp '(1 3 4 5 6 7 8 9 0))

(find-if #'(lambda (x) (> x 3)) '(0 1 2 3 4 5 6 7 8 9))

(setf words
      '((one un)
	(two deux)
	(three trois)
	(four quatre)
	(five cinq)))

(defun my-assoc (key table)
  (find-if #'(lambda (entry)
	       (equal key (first entry)))
	   table))

(defun nearby-no (k x)
  (find-if #'(lambda (entry)
	       (and (< k (+ entry 10))
		    (> k (- entry 10))))
	   x))

(defun roughly-equal (e k)
  (and (not (< e  (- k 10)))
       (not (> e (+ k 10)))))

(defun find-first-roughly-equal (x k)
  (find-if #'(lambda (e) (roughly-equal e k))
	   x))

(defun find-nested (x)
  (find-if #'consp x))

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

(defun numbers (x)
  (mapcar #'(lambda (y) (second (assoc y note-table)))
	  x))

(defun last-element-reverse (x)
  (car (reverse x)))

(defun last-element-last (x)
  (car (last x)))

(defun last-element-recurse (x)
  (if (null (cdr x))
      (car x)
      (last-element-recurse (cdr x))))

;;This is given as the solution in the book. But is probably incorrect

;;(defun notes (x)
;;  (mapcar #'(lambda (y) (first (rassoc y note-table)))
;;	  x))

(defun rassoc-nondot (key table)
  (find-if #'(lambda (entry)
	       (equal key (second entry)))
	   table))

(defun notes (x)
  (mapcar #'(lambda (y) (car (rassoc-nondot y note-table)))
	  x))

(defun raise (n x)
  (mapcar #'(lambda (y) (+ y n))
	  x))

(defun normalize (x)
  (mapcar #'(lambda (y)
	      (cond ((> y 12) (- y 12))
		    ((< y 1) (+ y 12))
		    (t y)))
	  x))

(defun transpose (n x)
  (notes (normalize (raise n (numbers x)))))

(defun my-but-last (x)
  (let ((y (reverse x)))
    (list (car (cdr y)) (car y))))


(defun element-at (list n)
  (if (= n 1)
      (car list)
      (element-at (cdr list) (- n 1))))

(defun length-list (x)
  (length x))

(defun reverse-list (x)
  (reverse x))

(defun pick-nos-gone-lfive (x)
  (remove-if-not #'(lambda (y) (and (> y 1) (< y 5))) x))
       
(defun count-the (x)
  (length
   (remove-if-not #'(lambda (y)
		      (equal y 'the))
		  x)))

(defun length-two (x)
  "this function does not consider the cases of non lists"
  (remove-if-not #'(lambda (y)
		     (equal (length y) 2))
		 x))

(defun my-intersection (x y)
  (remove-if-not #'(lambda (e) (member e y)) x))

(defun my-union (x y)
  (append x (remove-if #'(lambda (e) (member e x)) y)))

(defun sum-list-length (x)
  (reduce #'+ (mapcar #'length x)))

(defun all-odd (x)
  (every #'oddp x))

(defun none-odd (x)
  (every #'evenp x))

(defun not-all-odd (x)
  (cond ((and (equal (all-odd x) nil) (equal (none-odd x) nil)) t)
	 (t nil)))

(defun not-none-odd (x)
  (cond ((find-if #'oddp x) t)
	(t nil)))

(defun my-findif (x predicate)
  (first (remove-if-not predicate x)))
;; In the REPL we need to pass the x as '(1 2 3 4 5) and predicate as #'oddp

(defun my-every (x predicate)
  (if (equal (remove-if predicate x) nil) t nil))


(defun rank (x)
  (first x))

(defun suit (x)
  (second x))

(setf my-hand
      '((3 hearts)
	(5 clubs)
	(2 diamonds)
	(4 diamonds)
	(ace spades)))

(defun count-suit (suit hand)
  (length (remove-if-not #'(lambda (y) (equal (second y) suit)) hand)))

(setf colors
      '((clubs black)
	(diamonds red)
	(hearts red)
	(spades black)))

(defun color-of (cards)
  (second (assoc (suit cards) colors)))

(defun my-gcd (a b)
  (cond ((= b 0) a)
	(t (my-gcd b (mod a b)))))

(defun first-red (hand)
  (find-if #'(lambda (y) (equal (color-of y) 'red)) hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (y) (equal 'black (color-of y))) hand))

(defun what-ranks (suit hand)
  (mapcar #'rank
	  (remove-if-not
	   #'(lambda (y) (equal suit (suit y))) hand)))

(setf all-ranks
      '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (x y l)
  (member y (member x l)))

(defun higher-rank-p (card1 card2)
  (beforep (rank card1) (rank card2) all-ranks))

(defun high-card (hand)
  (assoc (find-if #'(lambda (y) (assoc y hand)) (reverse all-ranks)) hand))

(setf database
      '((b1 shape brick)
	(b1 color green)
	(b1 size small)
	(b1 supported-by b2)
	(b1 supported-by b3)
	(b2 shape brick)
	(b2 color red)
	(b2 size small)
	(b2 supports b1)
	(b2 left-of b3)
	(b3 shape brick)
	(b3 color red)
	(b3 size small)
	(b3 supports b1)
	(b3 right-of b2)
	(b4 shape pyramid)
	(b4 color blue)
	(b4 size large)
	(b4 supported-by b5)
	(b5 shape cube)
	(b5 color green)
	(b5 size large)
	(b5 supports b4)
	(b6 shape brick)
	(b6 color purple)
	(b6 size large)))

(defun match-element (sym1 sym2)
  (cond ((equal sym1 sym2) t)
	((equal sym2 '?) t)
	(t nil)))

(defun match-triple (list1 list2)
  (every #'match-element list1 list2))

(defun fetch (pattern)
  (remove-if-not #'(lambda (y) (match-triple y pattern)) databasepat))

(defun what-color (blockname)
  (list blockname 'color '?))

(defun supporters (blockname)
  (mapcar #'third (fetch (list blockname 'supported-by '?))))

(defun supp-cube (blockname)
  (member 'cube
	  (mapcar #'(lambda(y) (third (first (fetch (list y 'shape '?)))))
		  (supporters blockname))))

(defun desc1 (blockname)
  (fetch (list blockname '? '?)))

(defun desc2 (blockname)
  (mapcar #'cdr (desc1 blockname)))

(defun description (blockname)
 (reduce #'append (desc2 blockname)))

(setf words
      '((one un)
	(two deux)
	(three trois)
	(four quatre)
	(five cinq)))

(mapcar #'(lambda (x y) (append x (list y))) words '(uno dos tres quatro cinco))
