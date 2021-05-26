(defun serenade--log-to-scratch (message) 
  (print message (get-buffer "*scratch*")))

(provide 'serenade-log)
