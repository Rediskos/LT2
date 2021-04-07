(DEFUN -FUNCALL (fn &rest array) (APPLY fn array))

(print "task 1")
(-FUNCALL '+ 1 1 1 5)
          
                               
                               
(defun APL-APPLY (f x)
  (cond
    ((null x) NIL)
    (t (cons (FUNCALL (car f) (car x)) (APL-APPLY (cdr f) (cdr x)))))
  )



(print "task 3")
(APL-APPLY '(car car) '((1 2) (3 4)))

(defun НЕКОТОРЫЙ (пред список)
  (cond
    ((null список) NIL)
    ((FUNCALL пред (car список)) T)
    (t (НЕКОТОРЫЙ пред (cdr список))))
  )


(print "task 5")
(НЕКОТОРЫЙ 'atom '((1 2) (3 4) (5 4)))

(defun УДАЛИТЬ-ЕСЛИ-НЕ (пред список)
  ((lambda (car-список cdr-список)
     (cond
    ((null список) NIL)
    ((FUNCALL пред car-список) (cons car-список (НЕКОТОРЫЙ пред cdr-список)))
    (t (НЕКОТОРЫЙ пред cdr-список)))
     )
   (car список) (cdr список)
   )
  )



(print "task 7")
(УДАЛИТЬ-ЕСЛИ-НЕ 'atom '(1 (3 4) (5 4)))

(DEFUN ГЕН-ФИБОНАЧИ ()
  (let ((a -1) (b 1) (c -1))
    (lambda ()
      (setq c (cond
                ((< c 0) 0)
                ((+ (setq a b) (setq b c))))
            )
      )
    )
  )


(print "task 9")
(setq tmp (ГЕН-ФИБОНАЧИ))

(print (funcall tmp))
(print (funcall tmp))
(print (funcall tmp))
(print (funcall tmp))

(DEFUN МНОГОФУН (l-func x)
  (cond
    ((null l-func) nil)
    (t (cons (FUNCALL (car l-func) x) (МНОГОФУН (cdr l-func) x))))
  )

(print "task 11")
(function-lambda-expression #'МНОГОФУН)

(DEFUN ВЕРНУ-ЛЯМБДА () (function-lambda-expression #'ВЕРНУ-ЛЯМБДА))


(print "task 13")
(ВЕРНУ-ЛЯМБДА)


