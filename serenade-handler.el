(require 'serenade-editor-state)
(require 'serenade-buffer)
(require 'serenade-log)
(require 'test-utils)
(require 'serenade-custom-commands)

(defun serenade--handle-message (message) 
  (serenade--info (extract-json message )) 
  (let* ((callback (ht-get* message "data" "callback")) 
         (command-vector (ht-get* message "data" "response" "execute" "commandsList")) 
         (transcript (ht-get* message "transcript")) 
         (command-list (append command-vector nil))) 
    (if transcript (serenade--info (concat "received command: " transcript))) 
    (dolist (command command-list ) 
      (serenade--handle-command command message callback)) 
    (if serenade--websocket (serenade--send-completed callback))))

(defun serenade--handle-command (command message callback) 
  (serenade--set-serenade-buffer) 
  (let* ((type (ht-get*  command "type")) 
         (limited (ht-get* command "limited" )) 
         (log-info (concat type ": limited: "  (prin1-to-string limited) ))) 
    (serenade--info log-info) 
    (cond ((equal type "COMMAND_TYPE_GET_EDITOR_STATE") 
           (if (buffer-file-name) 
               (serenade--get-editor-state callback limited))) 
          ((equal type "COMMAND_TYPE_DIFF") 
           (if serenade-buffer (serenade--diff command))) 
          ((cond ((equal type "COMMAND_TYPE_EVALUATE_IN_PLUGIN") 
                  (serenade--evaluate-in-plugin command)) 
                 ((equal type "COMMAND_TYPE_UNDO") 
                  (serenade--undo)) 
                 ((equal type "COMMAND_TYPE_SELECT") 
                  (serenade--select-target (+ 1 (or (ht-get* command "cursor") 
                                                    0)) 
                                           (+ 0 (ht-get* command "cursorEnd")))) 
                 ((equal type "COMMAND_TYPE_COPY") 
                  (serenade--copy-target (ht-get* command "text"))) 
                 ((equal type "COMMAND_TYPE_PRESS") 
                  (let* ((transcript  (ht-get* message "data" "response" "execute" "transcript"))) 
                    (cond ((string-equal transcript "copy") 
                           (serenade--copy-selection)) 
                          ((string-equal transcript "cut") 
                           (serenade--cut-selection))))) 
                 ((equal type "COMMAND_TYPE_CLIPBOARD") 
                  (let* ((transcript  (ht-get* message "transcript"))) 
                    (cond ((string-equal transcript "paste") 
                           (serenade--paste))))) 
                 (t (serenade--execute-default-command command)))))))

(defun serenade--diff (command) 
  (serenade--info "diffing...") 
  (serenade--update-buffer (ht-get command "source") 
                           (+(or (ht-get command "cursor") 
                                 0) 1)))

(defun serenade--evaluate-in-plugin (command) 
  (let* ((command-text (ht-get* command "text")) 
         (command-type (ht-get* command "type"))
         ;; (command-list (eval (car (read-from-string (concat "'"command-text)))))
         (command-list (concat "'"command-text))) 
    (debug)))

(defun serenade--execute-default-command (command) 
  (serenade--info "executing default command") 
  (let* ((command-text (ht-get* command "text")) 
         (command-type (ht-get* command "type")) 
         (command-transcript (ht-get* message "data" "response" "execute" "transcript")) 
         (found-command  (serenade--find-voice-binding command-transcript)) 
         (bound-fn (ht-get* found-command "command"))) 
    (funcall bound-fn)))

(defun serenade--send-completed (callback) 
  (serenade--info "sending completed") 
  (if serenade--websocket (let* ((response (ht ("message" "callback") 
                                               ("data" (ht ("callback" callback) 
                                                           ("data" (ht ("message" "completed"))))))) 
                                 (response-json (json-serialize response))) 
                            (websocket-send-text serenade--websocket response-json))))

(provide 'serenade-handler)
