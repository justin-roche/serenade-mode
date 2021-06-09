(require 'serenade-editor-state)
(require 'serenade-buffer)
(require 'serenade-log)

(defun serenade--handle-message (message)
  ;; (message (prin1-to-string message))
  (let* ((callback (ht-get* message "data" "callback")) 
         (command-vector (ht-get* message "data" "response" "execute" "commandsList")) 
         (transcript (ht-get* message "transcript")) 
         (command-list (append command-vector nil))) 
    (if transcript (serenade--info transcript)) 
    (dolist (command command-list ) 
      (serenade--handle-command command message callback)) 
    (serenade--send-completed)))

(defun serenade--handle-command (command message callback) 
  (serenade--set-serenade-buffer)
  ;; (message (prin1-to-string command))
  (let* ((type (ht-get*  command "type")) 
         (limited (ht-get* command "limited" ))) 
    (cond ((equal type "COMMAND_TYPE_EVALUATE_IN_PLUGIN") 
           (serenade--evaluate-in-plugin command))) 
    (if serenade-buffer (cond ((equal type "COMMAND_TYPE_GET_EDITOR_STATE") 
                               (serenade--get-editor-state callback limited)) 
                              ((equal type "COMMAND_TYPE_DIFF") 
                               (serenade--diff command)) 
                              ((equal type "COMMAND_TYPE_UNDO") 
                               (serenade--undo)) 
                              ((equal type "COMMAND_TYPE_SELECT") 
                               (serenade-select-region (+ 1 (ht-get* command "cursor")) 
                                                       (+ 1 (ht-get* command "cursorEnd")))) 
                              ((equal type "COMMAND_TYPE_COPY") 
                               (serenade--copy (ht-get* command "text"))) 
                              ((equal type "COMMAND_TYPE_PRESS") 
                               (serenade--debug (prin1-to-string (json-serialize message))) 
                               (let* ((transcript  (ht-get* message "data" "response" "execute"
                                                            "transcript")))
                                 (cond ((string-equal transcript "cut") 
                                        (serenade--cut))))) 
                              ((equal type "COMMAND_TYPE_CLIPBOARD") 
                               (let* ((transcript  (ht-get* message "transcript"))) 
                                 (cond ((string-equal transcript "paste") 
                                        (serenade--paste))))) 
                              (t (serenade--execute-default-command command))))))

(defun serenade--diff (command) 
  (serenade--update-buffer (ht-get command "source") 
                           (+(or (ht-get command "cursor") 
                                 0) 1)))

(defun serenade--cut () 
  (execute-kbd-macro (kbd "x" )))

(defun serenade--copy (text) 
  (kill-new text))

(defun serenade--undo () 
  (evil-undo 1))

(defun serenade--paste () 
  (evil-paste-after))

(defun serenade--evaluate-in-plugin (command) 
  (let* ((command-text (ht-get* command "text")) 
         (command-type (ht-get* command "type"))) 
    (eval (car (read-from-string command-text)))))

(defun serenade--execute-default-command (command) 
  (message "executing default command") 
  (let* ((command-text (ht-get* command "text")) 
         (command-type (ht-get* command "type")))
    ;; (eval (car (read-from-string command-text)))
    ))

(defun serenade--send-completed () 
  (serenade--info "sending completed") 
  (let* ((response (ht("message" "completed") 
                      ("data" (ht)))) 
         (response-json (json-serialize response))) 
    (websocket-send-text serenade--websocket response-json)))

(provide 'serenade-handler)
(global-set-key (kbd "s-v" ) nil)
(global-set-key (kbd "s-c" ) nil)
(global-set-key (kbd "s-x" ) nil)

;; 23:04:34 [INFO ] diffing
;; 23:04:34 [INFO ] (0.001391 0 0.0)
;; 23:04:34 [INFO ] sending completed
;; 23:04:34 [INFO ] sending state
;; 23:04:34 [INFO ] sending completed
;; 23:04:35 [INFO ] sending state
;; 23:04:35 [INFO ] sending completed
;; 23:04:35 [INFO ] sending state
;; 23:04:35 [INFO ] sending completed
;; 23:04:36 [INFO ] sending state
;; 23:04:36 [INFO ] sending completed
;; 23:04:36 [INFO ] sending state
;; 23:04:36 [INFO ] sending completed
;; 23:04:37 [INFO ] sending state
;; 23:04:37 [INFO ] sending completed
;; 23:04:37 [INFO ] sending heartbeat
;; 23:04:37 [INFO ] sending state
;; 23:04:37 [INFO ] sending completed
;; 23:04:37 [INFO ] sending state
;; 23:04:37 [INFO ] sending completed
;; 23:04:37 [INFO ] sending state
;; 23:04:37 [INFO ] sending completed
;; 23:04:38 [INFO ] sending state
;; 23:04:38 [INFO ] sending completed
;; 23:04:38 [INFO ] sending state
;; 23:04:38 [INFO ] sending completed
;; 23:04:38 [INFO ] sending state
;; 23:04:38 [INFO ] sending completed
;; 23:04:38 [INFO ] diffing
;; 23:04:38 [INFO ] (0.001464 0 0.0)
