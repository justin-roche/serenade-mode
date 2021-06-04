(require 'websocket)
(require 'serenade-desktop)
(require 'serenade-log)

(setq serenade--websocket nil)
(defvar serenade-prompt-for-application-start nil)

(defun serenade-start-prompt () 
  (if (y-or-n-p "There was a problem connecting to Serenade. Start Serenade now?") 
      (serenade--start-application)))

(defun serenade--open-socket () 
  (setq serenade--websocket (condition-case err (websocket-open "ws://localhost:17373" 
                                                                :on-open (lambda (_websocket ) 
                                                                           (print "connected to
Serenade") 
                                                                           (serenade--register) 
                                                                           (serenade--heartbeat-start)) 
                                                                :on-message (lambda (_websocket
                                                                                     frame)
                                                                              (serenade--handle-message
                                                                               (json-parse-string
                                                                                (websocket-frame-text
                                                                                 frame))))
                                                                :on-close (lambda (_websocket) 
                                                                            (serenade--heartbeat-stop) 
                                                                            (message
                                                                             "Serenade websocket closed")))
                              (file-error (progn (if serenade-prompt-for-application-start
                                                     (serenade-start-prompt))
                                                 (serenade--log-and-message err))))))

(defun serenade--register() 
  (setq serenade-id (random 10000)) 
  (let* ((message (ht ("message" "active") 
                      ("data" (ht ("id" serenade-id) 
                                  ("app" "Emacs") 
                                  ("match" "Emacs"))))) 
         (message-json (json-serialize message))) 
    (websocket-send-text serenade--websocket message-json)))

(defun serenade--disconnect () 
  (websocket-close serenade--websocket))

(defun serenade-disconnect () 
  (interactive) 
  (serenade--disconnect))

(defun serenade--connect () 
  (serenade--open-socket))

(defun serenade-connect () 
  (interactive) 
  (serenade--connect))

(provide 'serenade-socket)
