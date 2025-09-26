;; Chapter 6 - List Data Structures

(defun last-element (x)
  (car (last x)))

(defun last-element-reverse (x)
  (car (reverse x)))

(defun last-element-nth (x)
  (nth 0 (reverse x)))

(defun last-element-nthlength (x)
  (nth (- (length x) 1) x))

(defun next-to-last (x)
  (car (cdr (reverse x))))

(defun next-to-lastnth (x)
  (nth 1 (reverse x)))

(defun my-butlast (x)
  (reverse (cdr (reverse x))))

(defun palindromep (x)
  (equal (reverse x) x))

(defun make-palindrome (x)
  (append x (cdr (reverse x))))

(defun contains-the-p (sent)
  (member 'the sent))

(defun contains-article-p (x)
  (or (member 'a x)
      (member 'the x)
      (member 'an x)))

(defun contains-article-intersection (x)
  (intersection '(a an the) x))

(defun contains-article-and (x)
  (not (and (not (member 'a x))
	    (not (member 'an x))
	    (not (member 'the x)))))

(defun add-vowels (x)
  (union '(a e i o u) x))

(defun my-subsetp (x y)
  (null (set-difference x y)))

(defun set-equal-subsetp (x y)
  (and (subsetp x y)
       (subsetp y x)))

(defun proper-subsetp (x y)
  (and (subsetp x y)
       (null (set-equal-subsetp x y))))

(defun royal-we (x)
  (subst 'we 'i x))

(defun square (x)
  (* x x))

(defun titledp (name)
  (member (first name) '(mr ms miss mrs)))

(defun right-side (x)
  (cdr (member '-vs- x)))

(defun left-side (x)
  (reverse (right-side (reverse x))))

(defun count-common (x)
  (length (intersection (right-side x) (left-side x))))

(defun compare (x)
  (list (count-common x) 'common 'features))

(setf produce
      '((apple . fruit)
	(celery . veggie)
	(banana . fruit)
	(lettuce . veggie)))

(setf books
      '((war-and-peace leo-tolstoy)
	(crime-and-punishment fyodor-dostyovesky)
	(wuthering-height emily-bronte)
	(the-pickwick-papers charles-dickens)
	(coun-of-monte-cristo alexander-dumas)))

(defun who-wrote (x)
  (rest (assoc x books)))

(setf books
      (reverse books))

(setf atlas
     '((pensylvania (pitts john))
      (new-jersey (newark princeton trenton))
      (ohio (columbus))))

(setf nerd-states
     '((sleeping . eating)
       (eating . waiting-for-a-computer)
       (waiting-for-a-computer . programming)
       (programming . debugging)
       (debugging . sleeping)))

(defun nerdus (x)
  (cdr (assoc x nerd-states)))

(defun sleepless-nerd (x)
  (if (equal x 'debugging) 'eating (nerdus x)))

(defun nerd-on-caffeine (x)
  (nerdus (nerdus x)))

(defun swap-first-last (x)
  (let* ((a (reverse (rest x)))
	 (b (reverse (rest a))))
  (cons (first a)
	(append b (list (first x))))))

(defun rotate-right (x)
  (append (cdr x) (list (car x))))

(defun rotate-left (x)
  (let ((r (reverse x)))
    (cons (first r) (reverse (rest r)))))

(setf rooms
      '((living-room
	 (north front-stairs)
	 (south dining-room)
	 (east kitchen))
	(upstairs-bedroom
	 (west library)
	 (south front-stairs))
	(dining-room
	 (north living-room)
	 (east pantry)
	 (west downstairs-bedroom))
	(kitchen
	 (west living-room)
	 (south pantry))
	(pantry
	 (north kitchen)
	 (west dining-room))
	(downstairs-bedroom
	 (north back-stairs)
	 (east dining-room))
	(back-stairs
	 (south downstairs-bedroom)
	 (north library))
	(front-stairs
	 (north upstairs-bedroom)
	 (south living-room))
	(library
	 (east upstairs-bedroom)
	 (south back-stairs))))

(defun choices (x)
  (rest (assoc x rooms)))

(defun look (direction x)
  (second (assoc direction (choices x))))

(setf loc 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC"
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairsp (x)
  (or (equal x 'library) (equal x 'upstairs-bedroom)))

(defun onstairsp (x)
 (or (equal x 'front-stairs) (equal x 'back-stairs)))


(defun where ()
  (if (onstairsp loc)
      (list 'robbie 'is 'on 'the loc)
      (list 'robbie 'is
	    (if (upstairsp loc) 'upstairs 'downstairs)
	    'in 'the loc)))

(defun move (dir)
  (let ((new-loc (look dir loc)))
    (cond ((null new-loc)
	   '(ouch! robbie hit a wall))
	  (t (set-robbie-location new-loc) (where)))))
