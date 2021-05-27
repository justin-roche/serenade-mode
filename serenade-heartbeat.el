(setq serenade--heartbeat-timer nil)
(defun serenade--heartbeat () 
  (let* ((message (ht ("message" "heartbeat") 
                      ("data" (ht ("id" serenade-id))))) 
         ( message-json (json-serialize message))) 
    (websocket-send-text serenade--websocket message-json) 
    (websocket-send-text serenade--websocket message-json)))

(defun serenade--heartbeat-stop () 
  (if (not (equal nil serenade--heartbeat-timer)) 
      (cancel-timer serenade--heartbeat-timer)))

(defun serenade--reset-heartbeat () 
  (if (not (equal nil serenade--heartbeat-timer)) 
      (cancel-timer serenade--heartbeat-timer)))

(provide 'serenade-heartbeat)
