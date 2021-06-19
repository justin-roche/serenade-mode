(require 'websocket)
(require 'serenade-desktop)
(require 'serenade-log)
(require 'serenade-globals)
(require 'serenade-heartbeat)
(require 'serenade-handler)

(setq serenade--websocket nil)
(defvar serenade-prompt-for-application-start nil)
(defvar serenade-port 17373)
(defvar serenade-reuse-id-on-connect nil)

(defun serenade-start-prompt () 
  (if (y-or-n-p "There was a problem connecting to Serenade. Start Serenade now?") 
      (serenade--start-application) 
    (serenade-mode -1)))

(defun serenade--open-socket () 
  (condition-case err (websocket-open (concat "ws://localhost:" (number-to-string serenade-port)) 
                                      :on-open (lambda (_websocket ) 
                                                 (message "connected to Serenade") 
                                                 (serenade--info "connected to Serenade") 
                                                 (setq serenade--websocket _websocket) 
                                                 (serenade--register) 
                                                 (serenade--heartbeat-start)) 
                                      :on-message (lambda (_websocket frame) 
                                                    (if serenade--websocket
                                                        (serenade--handle-message (json-parse-string
                                                                                   (websocket-frame-text
                                                                                    frame)))))
                                      :on-close (lambda (_websocket) 
                                                  (setq serenade--websocket nil) 
                                                  (serenade--heartbeat-stop) 
                                                  (message "Serenade websocket closed") 
                                                  (serenade--info "Serenade websocket closed") 
                                                  (serenade-mode -1))) 
    (file-error (progn (if serenade-prompt-for-application-start (serenade-start-prompt)) 
                       (progn (serenade--log-and-message err) 
                              (serenade-mode -1))))))

(defun serenade--register() 
  (setq serenade-id (or (and serenade-reuse-id-on-connect 
                             serenade-id) 
                        (random 10000))) 
  (let* ((message (ht ("data" (ht ("match" "Emacs") 
                                  ("app" "Emacs") 
                                  ("id" serenade-id))) 
                      ("message" "active"))) 
         (message-json (json-serialize message))) 
    (serenade--info (concat "registering with id: " (number-to-string serenade-id))) 
    (websocket-send-text serenade--websocket message-json)))

(defun serenade--disconnect () 
  (serenade--heartbeat-stop) 
  (if serenade--websocket (websocket-close serenade--websocket)))

(defun serenade-disconnect () 
  (interactive) 
  (serenade--disconnect))

(defun serenade--connect () 
  (serenade--open-socket))

(defun serenade-connect () 
  (interactive) 
  (serenade--connect))

(provide 'serenade-socket)
