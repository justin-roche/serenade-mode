(require 'log4e)
(log4e:deflogger "serenade" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal") 
                                                      (error 
                                                       .
                                                       "error") 
                                                      (warn 
                                                       .
                                                       "warn") 
                                                      (info  . "info") 
                                                      (debug . "debug") 
                                                      (trace . "trace")))
(serenade--log-enable-logging)

(defun serenade--log-and-message (data) 
  (progn (message (prin1-to-string data )) 
         (serenade--fatal (prin1-to-string data ))))
(provide 'serenade-log)
