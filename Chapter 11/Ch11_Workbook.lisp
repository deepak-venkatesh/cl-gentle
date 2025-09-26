;; Chapter 11 - Iteration and Block Structure

(defun it-member (item x)
    (dolist (i x)
      (when (equal item i) (return t))))
