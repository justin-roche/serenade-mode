
;; (provide 'serenade-builtin-commands)
;; (setq lexical-binding t)
;; (require 'ht)

;; (describe "Calls assigned commands for builtin speech commands" ;;
;;           (before-each (spy-on 'serenade--get-editor-state)
;;                        (spy-on-fn 'serenade--diff)
;;                        (spy-on-fn 'serenade--send-completed)
;;                        (spy-on-fn 'serenade--evaluate-in-plugin))
;;           (it "gets editor state if buffer has a filename" (let* ((data (ht-get* (json-parse-string
;;                                                                                   (load-json-commands))
;;                                                                                  "getEditorState")))
;;                                                              (setf (symbol-value 'buffer-file-name)
;;                                                                    "x")
;;                                                              (serenade--handle-message data))
;;               (expect   'serenade--get-editor-state
;;                         :to-have-been-called))
;;           (it "diffs" (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff")))
;;                         (setf (symbol-value 'buffer-file-name) "x")
;;                         (serenade--handle-message data))
;;               (expect   'serenade--diff
;;                         :to-have-been-called)
;;               (expect   'serenade--send-completed
;;                         :to-have-been-called))
;;           (it "calls evaluateInPlugin" (let* ((data (ht-get* (json-parse-string
;;                                                               (load-json-commands))
;;                                                              "evaluateInPlugin")))
;;                                          (setf (symbol-value 'buffer-file-name) "x")
;;                                          (serenade--handle-message data))
;;               (expect   'serenade--evaluate-in-plugin
;;                         :to-have-been-called)))
