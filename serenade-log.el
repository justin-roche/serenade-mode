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

(defun serenade--truncate-log (orig-fn &rest args) 
  (with-current-buffer " *log4e-serenade*" (let* ((log-size (count-lines (point-min) 
                                                                         (point-max)))) 
                                             (if (> log-size serenade-truncate-log-length) 
                                                 (serenade--log-clear-log)))) 
  (apply orig-fn args))

(advice-add 'serenade--info 
            :around 'serenade--truncate-log)

(defun extract-json (data) 
  (s-replace "\\" "" (s-replace "\\n" "" (json-serialize data))))

(provide 'serenade-log)
