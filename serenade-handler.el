(require 'serenade-modes)
(require 'serenade-log)
(require 'test-utils)
(require 'serenade-commands)

(defun serenade--handle-message (message)
  ;; Handle a serenade MESSAGE. Iterate through the messages command list calling handle command.
  (serenade--info (concat "\n" (extract-json message ))) 
  (let* ((callback (ht-get* message "data" "callback")) 
         (command-vector (ht-get* message "data" "response" "execute" "commandsList")) 
         (transcript (ht-get* message "transcript")) 
         (command-list (append command-vector nil))) 
    (if transcript (serenade--info (concat "received command: " transcript))) 
    (dolist (command command-list ) 
      (serenade--handle-command command message callback)) 
    (serenade--after-edit) 
    (if serenade--websocket (serenade--send-completed callback))))

(defun serenade--handle-command (command message callback)
  ;; Handle a serenade command COMMAND.
  (serenade--set-serenade-buffer) 
  (serenade--set-active-mode-configuration) 
  (let* ((type (ht-get*  command "type")) 
         (limited (ht-get* command "limited" )) 
         (log-info (concat type ": limited: "  (prin1-to-string limited) ))) 
    (serenade--info log-info) 
    (cond ((equal type "COMMAND_TYPE_GET_EDITOR_STATE") 
           (serenade--send-editor-state (funcall (serenade-mode-configuration-get-editor-state
                                                  serenade-active-mode-configuration) callback
                                                  limited)))
          ((equal type "COMMAND_TYPE_DIFF") 
           (funcall (serenade-mode-configuration-diff serenade-active-mode-configuration) 
                    (ht-get command "source") 
                    (+(or (ht-get command "cursor") 
                          0) 1))) 
          ((equal type "COMMAND_TYPE_CUSTOM") nil)
          ;; custom commands are sent with both COMMAND_TYPE_CUSTOM and COMMAND_TYPE_EVALUATE_IN_PLUGIN, so ignore the first of these.
          ((cond ((equal type "COMMAND_TYPE_EVALUATE_IN_PLUGIN") 
                  (serenade--execute-generated-command command)) 
                 ((equal type "COMMAND_TYPE_COPY") 
                  (serenade--execute-builtin-command  "copy <target>" (list (ht-get* command
                                                                                     "text"))))
                 ((equal type "COMMAND_TYPE_OPEN_FILE_LIST") 
                  (serenade--execute-builtin-command  "open <file>" (list (ht-get* command
                                                                                   "path"))))
                 ((equal type "COMMAND_TYPE_SWITCH_TAB") 
                  (serenade--execute-builtin-command  "<nth> tab"  (list (ht-get* command
                                                                                  "index"))))
                 ((equal type "COMMAND_TYPE_SELECT") 
                  (serenade--execute-builtin-command  "select <target>"  (list (+ 1 (or (ht-get*
                                                                                         command
                                                                                         "cursor")
                                                                                        0)) 
                                                                               (+ 0 (ht-get* command
                                                                                             "cursorEnd")))))
                 (t (serenade--execute-builtin-command (ht-get* message "data" "response" "execute"
                                                                "transcript"))))))))

(cl-defun 
    serenade--send-editor-state
    ((callback limited filename source cursor))
  ;; This function responds to a get-editor-state command with callback-id CALLBACK. If LIMITED Is true it sends only the file name. The specifics of how cursor and source are sent are determined by the mode configuration.
  (serenade--info (concat "buffer file name: " filename)) 
  (let* ((buffer-data (if limited (ht ("filename" filename)) 
                        (ht ("filename" filename) 
                            ("cursor" cursor) 
                            ("source" source)))) 
         (response (ht("data" (ht ("data" (ht ("data" buffer-data) 
                                              ("message" "editorState"))) 
                                  ("callback" callback))) 
                      ("message" "callback"))) 
         (response-json (json-serialize response)))
    ;; (debug)
    (websocket-send-text serenade--websocket response-json)))

(defun serenade--send-completed (callback)
  ;; Sends the completed message to serenade command having callback CALLBACK.
  (serenade--info "sending completed") 
  (if serenade--websocket (let* ((response (ht ("message" "callback") 
                                               ("data" (ht ("callback" callback) 
                                                           ("data" (ht ("message" "completed"))))))) 
                                 (response-json (json-serialize response))) 
                            (websocket-send-text serenade--websocket response-json))))

(cl-defun 
    serenade--execute-builtin-command
    ( transcript &optional args)
  ;; Evaluate bindings for builtin commands. Builtin commands are commands with speech bindings that do not need registered as custom commands. Speech command with TRANSCRIPT is mapped to its function call in the speech maps. For speech commands that have substititions, such as "<nth> tab" and "open <file>", function is applied with ARGS.
  (serenade--info "executing builtin command") 
  (if-let* ((found-command  (serenade--find-voice-binding transcript)) 
            (bound-fn (ht-get* found-command "command"))) 
      (if args (apply bound-fn args) 
        (funcall bound-fn))))

(defun serenade--execute-generated-command (command)
  ;; This function is responsible for calling the associated function for generated commands the input text is parsed as a list and evaluated.
  (let* ((command-text (ht-get* command "text")) 
         (input-list (eval (car (read-from-string (concat "'"command-text))))) 
         (speech-binding (car input-list) ) 
         (found-command  (serenade--find-voice-binding speech-binding))) 
    (if (not found-command) 
        (message (concat "no command found for \"" speech-binding "\"" )) 
      (serenade--call-generated-command-with-args  (ht-get* found-command "command") 
                                                   (car (cdr input-list))))))

(defun serenade--call-generated-command-with-args (bound-fn args)
  ;; parses the args which are received as alist. If the argument is a symbol return the symbol, accounting for numbers. Otherwise return the name of the symbol, accounting for strings.
  (if args (let* ((converted-args (-map '(lambda (arg ) 
                                           (let* ((key (car arg)) 
                                                  (value (cdr arg))) 
                                             (if  (s-matches? "[0-9]+" value) 
                                                 (cl-parse-integer value) value)) ) args))) 
             (apply bound-fn converted-args)) 
    (funcall bound-fn)))

(provide 'serenade-handler)
