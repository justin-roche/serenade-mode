(require 'websocket)
(require 'serenade-desktop)
(setq serenade--websocket nil)

(defun serenade-start-prompt (err) 
  (if (y-or-n-p "There was a problem connecting to Serenade. Start Serenade now?") 
      (serenade--start-application) 
    (message (prin1-to-string err ))))

(defun serenade--open-socket () 
  (setq serenade--websocket (condition-case err (websocket-open "ws://localhost:17373" 
                                                                :on-open (lambda (_websocket ) 
                                                                           (print "connected to
Serenade")) 
                                                                :on-message (lambda (_websocket
                                                                                     frame)
                                                                              (serenade-handle-message
                                                                               (json-parse-string
                                                                                (websocket-frame-text
                                                                                 frame))))
                                                                :on-close (lambda (_websocket) 
                                                                            (message
                                                                             "Serenade websocket closed")))
                              (file-error (serenade-start-prompt err)))))

(defun serenade--register() 
  (setq serenade-id (random 10000)) 
  (let* ((message (ht ("message" "active") 
                      ("data" (ht ("id" serenade-id) 
                                  ("app" "Emacs") 
                                  ("match" "Emacs"))))) 
         (message-json (json-serialize message))) 
    (websocket-send-text serenade--websocket message-json)))

(defun serenade-disconnect () 
  (interactive) 
  (websocket-close serenade--websocket))

(defun serenade-connect () 
  (interactive) 
  (serenade--open-socket) 
  (serenade--register)
  ;; (setq serenade-heartbeat-timer (run-with-timer 0 10 'serenade-heartbeat))
  )
;; (serenade--open-socket)
(provide 'serenade-socket)
