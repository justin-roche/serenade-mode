
(setq s-websocket nil)

(defun serenade-open-socket () 
  (setq s-websocket (websocket-open "ws://localhost:17373" 
                                    :on-open (lambda (_websocket ) 
                                               (print "connected to serenade")) 
                                    :on-message (lambda (_websocket frame) 
                                                  (serenade-handle-message (json-parse-string
                                                                            (websocket-frame-text
                                                                             frame))))
                                    :on-close (lambda (_websocket) 
                                                (message "websocket closed")))))

(defun serenade-close-socket () 
  (websocket-close s-websocket))

(defun serenade-connect() 
  (interactive) 
  (setq serenade-id (random 10000)) 
  (let* ((message (ht ("message" "active") 
                      ("data" (ht ("id" serenade-id) 
                                  ("app" "Emacs") 
                                  ("match" "Emacs"))))) 
         (message-json (json-serialize message))) 
    (websocket-send-text s-websocket message-json)))
