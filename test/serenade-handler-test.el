(describe "calls get editor state" ;;
          (before-each (spy-on 'serenade--send-editor-state) 
                       (spy-on 'websocket-send-text)) 
          (it "calls get editor state if valid buffer" ;;
              (let* ((data (load-request "getEditorState"))) 
                (create-test-buffer "test.js" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--send-editor-state 
                        :to-have-been-called)) 
          (it "calls get editor state with correct arguments" ;;
              (let* ((data (load-request "getEditorState"))) 
                (create-test-buffer "test11.js" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--send-editor-state 
                        :to-have-been-called-with  "956e2039-88fd-4d35-9e71-ba42abdde6bb" 
                        :false "test11.js" 
                        "" 0)) 
          (it "gets editor state if invalid buffer" ;;
              (let* ((data (load-request "getEditorState"))) 
                (create-test-buffer "test.xx" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--send-editor-state 
                        :to-have-been-called)))
(describe "calls diff" ;;
          (before-each (spy-on 'serenade--get-editor-state) 
                       (spy-on 'serenade--diff) 
                       (spy-on 'websocket-send-text) 
                       (spy-on 'serenade--evaluate-in-plugin)) 
          (it "calls diff if valid buffer" ;;
              (create-test-buffer "test.js" "") 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect   'serenade--diff 
                        :to-have-been-called)) 
          (it "calls diff if invalid buffer" ;;
              (create-test-buffer "test.xx" "") 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect   'serenade--diff 
                        :to-have-been-called)))
(describe "sends completed" ;;
          (before-each (spy-on 'websocket-send-text) 
                       (setq serenade--websocket t) 
                       (spy-on 'serenade--diff)) 
          (it "sends completed after diffing" ;;
              (let* ((req (load-request "diff")) 
                     (res (load-response "completed"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (setq serenade--websocket nil) 
                (expect   'websocket-send-text 
                          :to-have-been-called-with t res))))
