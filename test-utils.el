(defun spy-on-fn (sym) 
  (setf (symbol-function sym) 
        (lambda () 
          (print "calling spy"))) 
  (spy-on sym))

(defun reset-maps () 
  (setf (symbol-value 'serenade-mode-maps ) 
        (ht("global" (ht)))))

(defmacro async-with-timeout (timeout &rest body) 
  `(progn 
     (setq jr-async-returned nil)
     ,@body (with-timeout (,timeout) 
              (while (not jr-async-returned) 
                (sleep-for 0.1)))))

(provide 'test-utils)
