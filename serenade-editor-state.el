(require 'serenade-log)

(defun serenade--get-editor-state (callback limited) 
  (let* ((filename (-last-item (s-split "/" (buffer-file-name)))) 
         (buffer-data (ht ("source" 
                           (buffer-substring-no-properties 
                            (point-min) 
                            (point-max))) 
                          ("cursor" (- (point) 1)) 
                          ("filename" filename))) 
         (response (ht("message" "callback") 
                      ("data" (ht ("callback" callback) 
                                  ("data" (ht ("message" "editorState") 
                                              ("data" buffer-data))))))) 
         (response-json (json-serialize response))) 
    (serenade--save-data response-json)
    (serenade--info "sending state") 
    (message (prin1-to-string response)) 
    (websocket-send-text serenade--websocket response-json)))

(provide 'serenade-editor-state)
