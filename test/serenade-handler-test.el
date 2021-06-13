;; (setq lexical-binding t)
(require 'ht)
(require 'serenade-handler)
(require 'json)
(require 'evil)
(require 'test-utils)

(describe "calls get editor state" ;;
          (before-each (spy-on 'serenade--get-editor-state) 
                       (spy-on 'serenade--diff) 
                       (spy-on 'websocket-send-text) 
                       (spy-on 'serenade--evaluate-in-plugin)) 
          (it "calls get editor state if valid buffer" ;;
              (let* ((data (load-request "getEditorState"))) 
                (create-test-buffer "test.js" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--get-editor-state 
                        :to-have-been-called)) 
          (it "gets editor state if invalid buffer" ;;
              (let* ((data (load-request "getEditorState"))) 
                (create-test-buffer "test.xx" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--get-editor-state 
                        :to-have-been-called)))

(describe "gives correct result using target functions" ;;
          (before-each (spy-on 'websocket-send-text)) 
          (it "goes to lines by number" ;;
              (create-test-buffer "test2.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "goLine2"))) 
                (serenade--handle-message req)) 
              (expect (point) 
                      :to-equal 11)) 
          (it "selects lines by number" ;;
              (create-test-buffer "test6.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "selectLine2"))) 
                (serenade--handle-message req)) 
              (expect (region-beginning) 
                      :to-equal 11) 
              (expect (region-end) 
                      :to-equal 19)) 
          (it "cuts lines by number" ;;
              (create-test-buffer "test5.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "cutLine2"))) 
                (serenade--handle-message req)) 
              (expect   (buffer-string) 
                        :to-equal "let x = 1\n")) 
          (it "copies lines by number" ;;
              (create-test-buffer "test4.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "copyLine1"))) 
                (serenade--handle-message req)) 
              (expect (car kill-ring-yank-pointer) 
                      :to-equal "let x = 1")))

(describe "gives correct result using keypress functions" ;;
          (before-each (spy-on 'websocket-send-text)) 
          (it "copies a selection" ;;
              (create-test-buffer "test3.js" "let x = 1\nlet y = 2\n") 
              (serenade--select-target 11 20) 
              (let* ((req2 (load-request "copy"))) 
                (serenade--handle-message req2)) 
              (expect    (car kill-ring-yank-pointer) 
                         :to-equal "let y = 2"))
          ;; (it "copies evil" ;;
          ;;     (create-test-buffer "test3.js" "let x = 1\nlet y = 2\n")
          ;;     (setq serenade-evil t)
          ;;     (serenade--select-target 11 20)
          ;;     (let* ((req2 (load-request "copy")))
          ;;       (serenade--handle-message req2))
          ;;     (expect    (car kill-ring)
          ;;                :to-equal "let y = 2")
          (it "pastes evil" ;;
              (create-test-buffer "test7.js" "let x = 1\nlet y = 2") 
              (setq serenade-evil t) 
              (let* ((req1 (load-request "copyLine1")) 
                     (req2 (load-request "paste"))) 
                (serenade--handle-message req1) 
                (serenade--handle-message req2)) 
              (expect   (buffer-string) 
                        :to-equal "let x = 1\nlet y = 2\n\nlet x = 1")))
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
          (it "does not call diff if invalid buffer" ;;
              (create-test-buffer "test.xx" "") 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect   'serenade--diff
                        :not 
                        :to-have-been-called)))

(describe "calls diff" ;;
          (before-each (spy-on 'serenade--get-editor-state) 
                       (spy-on 'websocket-send-text) 
                       (spy-on 'serenade--evaluate-in-plugin)) 
          (it "calls evaluateInPlugin if there is no valid buffer" ;;
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "evaluateInPlugin"))) 
                (create-test-buffer "test.xx" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--evaluate-in-plugin 
                        :to-have-been-called)))
(describe "Calls cut" ;;
          (before-each (spy-on 'websocket-send-text) 
                       (spy-on 'serenade--cut-selection)) 
          (it "calls cut if valid buffer" ;;
              (let* ((req (load-request "cut"))) 
                (create-test-buffer "test.js" "") 
                (serenade--handle-message req)) 
              (expect   'serenade--cut-selection 
                        ':to-have-been-called)))

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
(describe "calls default commands" ;;
          (before-each (spy-on 'serenade--send-completed) 
                       (spy-on 'serenade--execute-default-command)) 
          (it "calls default command handler for save" ;;
              (let* ((req (load-request "save"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'serenade--execute-default-command 
                          :to-have-been-called))))
