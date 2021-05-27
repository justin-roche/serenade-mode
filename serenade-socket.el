
(setq serenade--websocket nil)

(defun serenade--open-socket () 
  (setq serenade--websocket (websocket-open "ws://localhost:17373" 
                                            :on-open (lambda (_websocket ) 
                                                       (print "connected to serenade")) 
                                            :on-message (lambda (_websocket frame) 
                                                          (serenade-handle-message
                                                           (json-parse-string (websocket-frame-text
                                                                               frame))))
                                            :on-close (lambda (_websocket) 
                                                        (message "websocket closed")))))

(defun serenade-register() 
  (setq serenade-id (random 10000)) 
  (let* ((message (ht ("message" "active") 
                      ("data" (ht ("id" serenade-id) 
                                  ("app" "Emacs") 
                                  ("match" "Emacs"))))) 
         (message-json (json-serialize message))) 
    (websocket-send-text serenade--websocket message-json)))

(defun serenade--close-socket () 
  (websocket-close serenade--websocket))

(defun serenade-connect () 
  (interactive) 
  (serenade--open-socket) 
  (serenade-register)
  ;; (setq serenade-heartbeat-timer (run-with-timer 0 10 'serenade-heartbeat))
  )

(provide 'serenade-socket)
