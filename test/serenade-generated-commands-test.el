(require 'json)
(require 'evil)

(describe "handler calls execute-generated-command" ;;
          (before-each (spy-on 'serenade--get-editor-state) 
                       (spy-on 'websocket-send-text) 
                       (spy-on 'switch-to-buffer) 
                       (spy-on 'serenade--execute-generated-command)) 
          (it "calls execute-generated-command if there is no valid buffer" ;;
              (let* ((data (ht-get* (json-parse-string (load-json-custom-commands))
                                    "evaluateInPlugin")))
                (create-test-buffer "test.xx" "") 
                (serenade--handle-message data)) 
              (expect   'serenade--execute-generated-command 
                        :to-have-been-called)))

(describe "calls generated functions" ;;
          (before-each (spy-on 'websocket-send-text) 
                       (defun test-fn-2 (a b)) 
                       (defun test-fn-3 (a b c d e)) 
                       (spy-on 'test-fn-2) 
                       (spy-on 'test-fn-3) 
                       (spy-on 'switch-to-buffer)) 
          (it "calls the function assigned to the custom speech binding with single text argument" ;;
              (serenade-define-speech 'global "open buffer <name>" 'switch-to-buffer) 
              (let* ((data (ht-get* (json-parse-string (load-json-custom-commands))
                                    "openBufferText")))
                (serenade--handle-message data)) 
              (expect   'switch-to-buffer 
                        :to-have-been-called-with "index")) 
          (it
           "calls the function assigned to the custom speech binding with multiple text arguments"
           ;;
           (serenade-define-speech 'global "open buffer <name> <direction>" 'test-fn-2) 
           (let* ((data (ht-get* (json-parse-string (load-json-custom-commands))
                                 "openBufferTextText")))
             (serenade--handle-message data)) 
           (expect   'test-fn-2 
                     :to-have-been-called-with "index" "left")) 
          (it
           "calls the function assigned to the custom speech binding with text argument and number argument" ;;
           (serenade-define-speech 'global "open buffer <name> <n>" 'test-fn-2) 
           (let* ((data (ht-get* (json-parse-string (load-json-custom-commands))
                                 "openBufferTextAndNumber")))
             (serenade--handle-message data)) 
           (expect   'test-fn-2 
                     :to-have-been-called-with "index" 3)) 
          (it
           "calls the function assigned to the custom speech binding with reordered text arguments"
           ;;
           (serenade-define-speech 'global "open buffer <%2 name> <%1 direction>" 'test-fn-2) 
           (let* ((data (ht-get* (json-parse-string (load-json-custom-commands))
                                 "openBufferTextText")))
             (serenade--handle-message data)) 
           (expect   'test-fn-2 
                     :to-have-been-called-with "left" "index")) 
          (it
           "calls the function assigned to the custom speech binding with many reordered text arguments"
           ;;
           (serenade-define-speech 'global "open buffer <%2 a> <%55 b> <%3 c> <%20 d> <%7 e>"
                                   'test-fn-3) 
           (let* ((data (ht-get* (json-parse-string (load-json-custom-commands)) "manyCommands"))) 
             (serenade--handle-message data)) 
           (expect   'test-fn-3 
                     :to-have-been-called) 
           (expect   'test-fn-3 
                     :to-have-been-called-with "aa" 
                     "cc" "ee" "dd" "bb")))

;; (describe "gives correct result using undo" ;;
;;           ;; (before-each (spy-on 'websocket-send-text))
;;           ;; (it "performs undo" ;;
;;           ;;     (create-test-buffer "test8.js" "")
;;           ;;     (let* ((req1 (load-request "addLetX"))
;;           ;;            (req2 (load-request "undo")))
;;           ;;       (serenade--handle-message req1)
;;           ;;       ;; (serenade--handle-message req2)
;;           ;;       )
;;           ;;     (expect    (buffer-string)
;;           ;;                :to-equal ""))
;;           )
