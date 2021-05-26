
(setq serenade-commands (ht ("comment-dwim" "comment") 
                            ("describe-face" "d")))

(defun serenade-auto-set-command (command ) 
  (let* ((split-command  (s-replace  "-" " " (symbol-name command)))) 
    (ht-set serenade-commands (symbol-name command) split-command)))

(defun serenade-set-commands 
    (&commands) 
  (dolist (command &commands ) 
    (serenade-set-command (car command) 
                          (cdr command))))

(defun serenade--send-completed () 
  (let* ((response (ht("message" "complete") 
                      ("data" nil))) 
         (response-json (json-serialize response))) 
    (websocket-send-text s-websocket response-json)))

(defun serenade--handle-custom-command (message-command) 
  (let* ((command-text (ht-get* message-command "text")) 
         (command-type (ht-get* message-command "type"))) 
    (eval (car (read-from-string command-text)))))
(defun serenade--handle-custom-command (message-command) 
  (let* ((command-text (ht-get* message-command "text")) 
         (command-type (ht-get* message-command "type"))) 
    (eval (car (read-from-string command-text)))))

(provide 'serenade-commands)
