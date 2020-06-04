(defclass lst ()
  ((lst :accessor lst)))
 
(defclass associative-list (lst) ())

(defclass binary-tree (lst) ())

(defgeneric dict-get (dict key))
(defgeneric dict-add (dict key value))
(defgeneric dict-delete (dict key))

(defmethod dict-get ((dict associative-list) key)
  (labels ((loc-func (lst)
	    (if (or (not lst) (eql (caar lst) key))
		(if (not lst) (values NIL NIL) (values (cdar lst) T))
		(loc-func (cdr lst))))) 
    (loc-func (lst dict))))

(defmethod dict-get ((dict binary-tree) key)
  (labels ((loc-func (tree)
	     (cond
	       ((null tree) (values nil nil))
	       ((string= (caar tree) key) (values (cdar tree)  T))
	       ((string> (caar tree) key) (loc-func (cadr tree)))
	       (t (loc-func (caddr tree))))))
    (loc-func (lst dict))))


(defmethod dict-add ((dict associative-list) key value)
    (setf (lst dict) (cons (cons key value) (lst dict))))

(defmethod dict-add ((dict binary-tree) key value)
  (prog1
  (let ((old-value (dict-get dict key)))(if old-value (values old-value T) (values nil nil)))
  (setf (lst dict) (labels ((loc-func (tree)
	     (cond
	       ((null tree) (list (cons key value) nil nil))
	       ((string> (caar tree) key) (list (car tree) (loc-func (cadr tree)) (caddr tree)))
	       ((string= (caar tree) key) (list (cons key value) (cadr tree) (caddr tree)))
	       (t (list (car tree) (cadr tree) (loc-func (caddr tree)))))
	     ))
    (loc-func (lst dict))))))

(defmethod dict-delete ((dict associative-list) key)
  (setf (lst dict) (labels ((loc-func (lst)
	     (if (or (not lst) (eql (caar lst) key))
		 (if (not lst) nil (cdr lst))
		 (cons (car lst)(loc-func (cdr lst)))))) 
    (loc-func (lst dict)))))

(defmethod dict-delete ((dict associative-list) key)
  (prog1
  (let ((val (dict-get dict key))) (if (not val) (values nil nil) (prog1 (values val T)
  (setf (lst dict) (labels ((loc-func (lst)
	     (if (or (not lst) (eql (caar lst) key))
		 (if (not lst) nil (cdr lst))
		 (cons (car lst)(loc-func (cdr lst)))))) 
    (loc-func (lst dict)))))))))

(defmethod dict-delete ((dict binary-tree) key)
  (prog1
  (let ((val (dict-get dict key))) (if (not val) (values nil nil) (prog1 (values val T)
  (setf (lst dict) (labels ((loc-func (tree)
	     (cond
	       ((null tree) nil)
	       ((string= (caar tree) key) (progn (values (cdar tree)  T) (cond 
										   ((and (eql nil (cadr tree)) (eql nil (caddr tree))) nil)
										   ((eql (caddr tree) nil) (cadr tree))
										   ((eql (cadr tree) nil) (caddr tree))
										   (t (list (find-left (caddr tree)) (cadr tree) (remove-left (caddr tree)))))
						 ))
	       ((string> (caar tree) key) (list (car tree) (loc-func (cadr tree)) (caddr tree)))
	       (t (list (car tree) (cadr tree) (loc-func (caddr tree)))))))
    (loc-func (lst dict)))))))))

(defun find-left (tree)
  (cond
    ((eql (cadr tree) nil) (car tree))
    (t (find-left (cadr tree)))))

(defun remove-left (tree)
  (cond
    ((eql (cadr tree) nil) (caddr tree))
    (t (list (car tree) (remove-left (cadr tree)) (caddr tree)))))
  
