(require 'log4e)

(log4e:deflogger "serenade" "%t [%l] %m" "%H:%M:%S:%3N" '((fatal . "fatal") 
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
         (serenade--info (prin1-to-string data ))))

(defun extract-json (data) 
  (s-replace "\\" "" (s-replace "\\n" "" (json-serialize data))))

(provide 'serenade-log)
