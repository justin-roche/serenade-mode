;; (require 'ht)

;; (require 'serenade-modes)
;; (require 'serenade-handler)
;; (require 'test-utils)

;; (describe "gets editor state from buffer" ;;
;;           (before-each (serenade--set-active-mode-configuration)
;;                        (spy-on 'websocket-send-text))
;;           (it "gets editor state" ;;
;;               (let* ((req (load-request "getEditorState"))(data (load-response "getEditorState")))
;;                 (switch-to-buffer (get-buffer-create "test.js"))
;;                 (setq buffer-file-name "test.js")
;;                 (insert "abc")
;;                 (serenade--get-editor-state "1" nil)
;;                 (expect   'websocket-send-text
;;                           :to-have-been-called-with nil data))))
