(defun simplify (lis)
  (if (atom lis)
    lis
    (let ((args (mapcar #'simplify (cdr lis))))
      (cond
        ((eql (car lis) '+)
          (setq args (remove 0 args))
          (case (length args)
            (0 0)
            (1 (car args))
            (otherwise (cons '+ args)) ))
        ((eql (car lis) '*)
          (if (member 0 args)
              0
              (progn
                (setq args (remove 1 args))
                (case (length args)
                  (0 1)
                  (1 (car args))
                  (otherwise (cons '* args)) ))))
        (T (cons (car lis) args)) ))))
(print (simplify '(+ a b (* b (+ c 0) b) (*(+ b f ) 0) )  ))