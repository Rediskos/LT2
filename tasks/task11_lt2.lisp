(DEFUN МНОГОФУН (l-func x)
  (cond
    ((null l-func) nil)
    (t (cons (FUNCALL (car l-func) x) (МНОГОФУН (cdr l-func) x))))
  )

(function-lambda-expression #'МНОГОФУН)
