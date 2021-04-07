(defun APL-APPLY (f x)
  (cond
    ((null x) NIL)
    (t (cons (FUNCALL (car f) (car x)) (APL-APPLY (cdr f) (cdr x)))))
  )



(APL-APPLY '(car car) '((1 2) (3 4)))
