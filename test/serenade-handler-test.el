;; (setq lexical-binding t)
(require 'ht)
(require 'serenade-handler)
(require 'json)
(require 'test-utils)

(describe "Parses incoming message" ;;
          (before-each (spy-on-fn 'serenade--get-editor-state) 
                       (spy-on-fn 'serenade--diff) 
                       (spy-on-fn 'websocket-send-text) 
                       (spy-on-fn 'serenade--evaluate-in-plugin)) 
          (it "calls get editor state if valid buffer" ;;
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "getEditorState"))) 
                (create-test-buffer "test.js" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--get-editor-state 
                        :to-have-been-called)) 
          (it "does not call get editor state if invalid buffer" ;;
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "getEditorState"))) 
                (create-test-buffer "test.xx" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--get-editor-state 

                        :not 
                        :to-have-been-called)) 
          (it "calls diff if valid buffer" ;;
              (create-test-buffer "test.js" "") 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect   'serenade--diff 
                        :to-have-been-called)) 
          (it "does not call diff if invalid buffer" ;;
              (create-test-buffer "test.xx" "") 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect   'serenade--diff
                        :not 
                        :to-have-been-called)) 
          (it "calls evaluateInPlugin if there is no valid buffer" ;;
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "evaluateInPlugin"))) 
                (create-test-buffer "test.xx" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--evaluate-in-plugin 
                        :to-have-been-called)))
(describe "sends completed" ;;
          (before-each (spy-on-fn 'websocket-send-text) 
                       (spy-on-fn 'serenade--diff)) 
          (it "sends completed after diffing" ;;
              (let* ((req (ht-get* (json-parse-string (load-json-commands)) "diff")) 
                     (res (json-serialize (ht-get* (json-parse-string (load-json-responses))
                                                   "completed"))))
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'websocket-send-text 
                          :to-have-been-called-with serenade--websocket res))))
