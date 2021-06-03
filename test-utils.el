
(defun reset-maps () 
  (setf (symbol-value 'serenade-mode-maps ) 
        (ht("global" (ht)))))

(provide 'test-utils)
