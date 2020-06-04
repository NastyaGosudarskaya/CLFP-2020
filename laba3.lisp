;;map
(defun mymap (func lst)
  (when lst
    (cons (funcall func (car lst)) (mymap func (cdr lst)))))

;;filter
(defun myfilter (func lst)
  (when lst
    (if (funcall func (car lst))
      (cons (car lst) (myfilter func (cdr lst)))
      (myfilter func (cdr lst)))))

;;fold
(defun myfold (func lst &key (initial-value nil))
  (labels ((loc-func ( lst)
    (if (cadr lst)
      (loc-func (cons (funcall func (car lst) (cadr lst)) (cddr lst)))
      (car lst))))
   (loc-func (if initial-value (cons initial-value lst)  lst))))  

