;;associative list
(defun associative-list-add (list key value)
  (cons (cons key value) list))

(defun associative-list-get (list key)
  (if (or (not list) (eql (caar list) key))
      (if (not list) (values NIL NIL) (values (cdar list) T))
      (associative-list-get (cdr list) key)))
;;property list
(defun property-list-add (list key value)
  (cons key (cons value list)))

(defun property-list-get (list key)
  (if (or (not list) (eql (car list) key))
      (if (not list) (values nil nil) (values (cadr list) t))
      (property-list-get (cddr list) key)))
;;binary tree
(defun binary-tree-add (tree key value)
  (cond
    ((null tree) (list (cons key value) nil nil))
    ((string>= (caar tree) key) (list (car tree) (binary-tree-add (cadr tree) key value) (caddr tree)))
    (t (list (car tree) (cadr tree) (binary-tree-add (caddr tree) key value)))))

(defun binary-tree-get (tree key)
  (cond
    ((null tree) (values nil nil))
    ((string= (caar tree) key) (values (cdar tree)  T))
    ((string> (caar tree) key) (binary-tree-get (cadr tree) key))
    (t (binary-tree-get (caddr tree) key))))
