;; Chapter 3 - EVAL Notation

; All exercises I have solved by hand in the book physically itself. Will only upload a few coding exercises which I think are good to keep.

(defun myfun (x y)
  (list (list x) y))

(defun firstp (x y)
  (equal x (first y)))

(defun mid-add1 (x)
  (list (car x) (+ (car (cdr x)) 1) (car (cdr (cdr x)))))

(defun f-to-c (x)
  (* (/ 5.0 9.0) (- x 32)))

(defun c-to-f (x)
  (+ 32 (* (/ 9.0 5.0) x)))

