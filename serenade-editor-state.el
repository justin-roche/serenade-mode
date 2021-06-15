(require 'serenade-log)

(defun serenade--get-editor-state (callback limited) 
  (serenade--info (concat "buffer file name: "(buffer-file-name))) 
  (let* ((filename (if (buffer-file-name) 
                       (-last-item (s-split "/" (buffer-file-name))) "")) 
         (buffer-data (if limited (ht ("filename" filename)) 
                        (ht ("filename" filename) 
                            ("cursor" (- (point) 1)) 
                            ("source" 
                             (buffer-substring-no-properties 
                              (point-min) 
                              (point-max)))))) 
         (response (ht("data" (ht ("data" (ht ("data" buffer-data) 
                                              ("message" "editorState"))) 
                                  ("callback" callback))) 
                      ("message" "callback"))) 
         (response-json (json-serialize response))) 
    (websocket-send-text serenade--websocket response-json)))

(provide 'serenade-editor-state)
