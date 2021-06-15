(require 'serenade-editor-state)
(require 'serenade-buffer)
(require 'serenade-log)
(require 'test-utils)
(require 'serenade-commands)

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
                 ((equal type "COMMAND_TYPE_COPY") 
                  (serenade--execute-default-command  "copy <target>" (list (ht-get* command
                                                                                     "text"))))
                 ((equal type "COMMAND_TYPE_OPEN_FILE_LIST") 
                  (serenade--execute-default-command  "open <file>" (list (ht-get* command
                                                                                   "path"))))
                 ((equal type "COMMAND_TYPE_SWITCH_TAB") 
                  (serenade--execute-default-command  "<nth> tab"  (list (ht-get* command
                                                                                  "index"))))
                 ((equal type "COMMAND_TYPE_SELECT") 
                  (serenade--execute-default-command  "select <target>"  (list (+ 1 (or (ht-get*
                                                                                         command
                                                                                         "cursor")
                                                                                        0)) 
                                                                               (+ 0 (ht-get* command
                                                                                             "cursorEnd")))))
                 (t (serenade--execute-default-command (ht-get* message "data" "response" "execute"
                                                                "transcript"))))))))

(defun serenade--diff (command) 
  (serenade--info "diffing...") 
  (serenade--update-buffer (ht-get command "source") 
                           (+(or (ht-get command "cursor") 
                                 0) 1)))

(cl-defun 
    serenade--execute-default-command
    ( transcript &optional args)
  ;; Evaluate bindings for default commands. Default commands are commands with speech bindings that do not need registered as custom commands. Speech command with TRANSCRIPT is mapped to its functional call in the speech maps. For speech commands that have substititions, such as "<nth> tab" and "open <file>", function is applied with ARGS.
  (serenade--info "executing default command") 
  (if-let* ((found-command  (serenade--find-voice-binding transcript)) 
            (bound-fn (ht-get* found-command "command"))) 
      (if args (apply bound-fn args) 
        (funcall bound-fn))))

(defun serenade--evaluate-in-plugin (command) 
  (let* ((command-text (ht-get* command "text")) 
         (command-as-list (eval (car (read-from-string (concat "'"command-text))))) 
         (speeech-binding (car command-as-list) ) 
         (args (cdr command-as-list) ) 
         (converted-args (-map '(lambda (item) 
                                  (if (eq 'symbol (type-of item))
                                      ;; handle ordinals
                                      (symbol-name item) item)) args) ) 
         (found-command  (serenade--find-voice-binding speeech-binding)) 
         (bound-fn (ht-get* found-command "command"))) 
    (apply bound-fn converted-args)))

(defun serenade--send-completed (callback) 
  (serenade--info "sending completed") 
  (if serenade--websocket (let* ((response (ht ("message" "callback") 
                                               ("data" (ht ("callback" callback) 
                                                           ("data" (ht ("message" "completed"))))))) 
                                 (response-json (json-serialize response))) 
                            (websocket-send-text serenade--websocket response-json))))

(provide 'serenade-handler)
