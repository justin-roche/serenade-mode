(require 'ht)
(require 'serenade-handler)
(require 'test-utils)

(describe "gets editor state from buffer" ;;
          (before-each (spy-on 'websocket-send-text)) 
          (it "gets editor state" ;;
              (let* ((data (load-response "getEditorState"))) 
                (switch-to-buffer (get-buffer-create "test.js")) 
                (setq buffer-file-name "test.js") 
                (insert "abc") 
                (serenade--get-editor-state "1" nil) 
                (expect   'websocket-send-text 
                          :to-have-been-called-with nil data))))
