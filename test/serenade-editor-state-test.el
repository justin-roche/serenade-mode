(setq lexical-binding t)
(require 'ht)
(require 'serenade-handler)
(require 'test-utils)
(require 'json)

(defun load-json-responses () 
  (with-temp-buffer (insert-file-contents "test/responses.json") 
                    (buffer-string)))
(describe "gets editor state from buffer" ;;
          (before-each (bc/set-spy 'websocket-send-text)) 
          (it "gets editor state" ;;
              (let* ((data (json-serialize (ht-get* (json-parse-string (load-json-responses))
                                                    "getEditorState"))))
                (switch-to-buffer (get-buffer-create "test.js")) 
                (setq buffer-file-name "test.js") 
                (insert "abc") 
                (serenade--get-editor-state "1" nil) 
                (expect   'websocket-send-text 
                          :to-have-been-called-with nil data))))
