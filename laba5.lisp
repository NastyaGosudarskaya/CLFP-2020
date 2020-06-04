(defun differentiate (var expr)
  (cond
    ((listp (car expr)) (differentiate var (car expr)))
    ((numberp (car expr)) 0)
    ((symbolp (car expr)) (cond
			    ((eq (car expr) var) 1)
			    ((or (eq (car expr) '+) (eq (car expr) '-)) (diff-add-sub (differentiate var (cdr expr)) (differentiate var (cddr expr)) (car expr)))
			    ((eq (car expr) '*) (diff-add-sub (diff-mult (cadr expr) (differentiate var (cddr expr))) (diff-mult (differentiate var (cdr expr)) (caddr expr)) '+))
			    ((or (eq (car expr) 'sin) (eq (car expr) 'cos)) (diff-mult (differentiate var (cdr expr)) (diff-sin-cos (cadr expr) (car expr))))
			    ((eq (car expr) 'expt) (diff-mult (differentiate var (cdr expr)) (diff-expt (cadr expr) (caddr expr))))
			    ((eq (car expr) '/) (list '/ (diff-add-sub (diff-mult (differentiate var (cdr expr)) (caddr expr)) (diff-mult (cadr expr) (differentiate var (cddr expr))) '-) (list 'expt (caddr expr) 2)))			    
))))

(defun diff-add-sub (l r sign)
    (cond
      ((and (numberp l) (eq l 0)) (if (eq sign '-) (if (numberp r) (- 0 r) (list '- 0 r)) r))
      ((and (numberp r) (eq r 0)) l)
      ((and (numberp l) (numberp r)) (funcall sign l r))
      (t (list sign l r))))

(defun diff-mult (l r)
  (cond
      ((or (and (numberp l) (eq l 0)) (and (numberp r) (eq r 0))) 0)
      ((and (numberp l) (eq l 1)) r)
      ((and (numberp r) (eq r 1)) l)
      ((and (numberp l) (numberp r)) (* l r))
      (t (list '* l r))))

(defun diff-sin-cos (l oper)
  (if (eq oper 'cos)
      (list '- 0 (list 'sin l))
      (list 'cos l)))

(defun diff-expt (l r)
  (cond
    ((eq (- r 1) 1)  (list '* r l))
    (t  (list '* r `(expt ,l ,(- r 1))))))

