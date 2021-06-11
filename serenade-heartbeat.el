(require 'serenade-log)
(require 'serenade-globals)

(setq serenade--heartbeat-timer nil)

(defun serenade--heartbeat () 
  (if serenade--heartbeat-timer (let* ((message (ht ("message" "heartbeat") 
                                                    ("data" (ht ("id" serenade-id))))) 
                                       ( message-json (json-serialize message))) 
                                  (serenade--info "sending heartbeat") 
                                  (websocket-send-text serenade--websocket message-json))))

(defun serenade--heartbeat-stop () 
  (if (not (equal nil serenade--heartbeat-timer)) 
      (progn (serenade--info "stopping heartbeat") 
             (cancel-timer serenade--heartbeat-timer))))

(defun serenade--heartbeat-reset () 
  (if (not (equal nil serenade--heartbeat-timer)) 
      (cancel-timer serenade--heartbeat-timer)))

(defun serenade--heartbeat-start () 
  (setq serenade--heartbeat-timer (run-with-timer 0 60 'serenade--heartbeat)))

(provide 'serenade-heartbeat)
