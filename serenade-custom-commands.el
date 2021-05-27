
(setq serenade-commands (ht ("comment-dwim" "comment") 
                            ("describe-face" "d")))
(cl-defun 
    serenade-set-command
    (command fn &key repeatable)
  ;; (ht-set serenade-commands ( (symbol-name fn) command) command))
  (ht-set serenade-commands command (symbol-name fn)  ) 
  (serenade-update-helm-map  command fn))

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
    (websocket-send-text serenade--websocket response-json)))

(defun serenade--handle-custom-command (message-command) 
  (let* ((command-text (ht-get* message-command "text")) 
         (command-type (ht-get* message-command "type"))) 
    (eval (car (read-from-string command-text)))))

(defun serenade--handle-custom-command (message-command) 
  (let* ((command-text (ht-get* message-command "text")) 
         (command-type (ht-get* message-command "type"))) 
    (eval (car (read-from-string command-text)))))

(provide 'serenade-commands)

(serenade-set-command "describe face" 'describe-face)

(serenade-set-command "select window %s %s %s" 'select-window)
(serenade-set-command "org promote <level>" 'org-do-promote)
(serenade-set-command "promote <level>" 'org-do-promote)
;; (serenade-set-command "org promote <level>" 'org-do-promote)
(serenade-set-command "uncomment-region" 'uncomment-region)
