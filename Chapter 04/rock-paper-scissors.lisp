;; rudimentary game of rock paper scissiors 

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
