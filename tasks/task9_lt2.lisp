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


(setq tmp (ГЕН-ФИБОНАЧИ))

(print (funcall tmp))
