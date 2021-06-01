(setq lexical-binding t)
(require 'ht)
(require 'serenade-handler)
(require 'json)

(defun load-json-commands () 
  (with-temp-buffer (insert-file-contents "test/commands.json") 
                    (buffer-string)))
(describe "Parses incoming message"
          ;; :var ()
          (before-each (setf (symbol-function 'serenade--get-editor-state) 
                             (lambda ())) 
                       (setf (symbol-function 'serenade--diff) 
                             (lambda ())) 
                       (setf (symbol-function 'serenade--send-completed) 
                             (lambda ())) 
                       (setf (symbol-function 'serenade--evaluate-in-plugin) 
                             (lambda ())) 
                       (spy-on 'serenade--diff) 
                       (spy-on 'serenade--evaluate-in-plugin) 
                       (spy-on 'serenade--send-completed) 
                       (spy-on 'serenade--get-editor-state))
          ;; (message data)
          (it "gets editor state if buffer has a filename" (let* ((data 
                                                                   (ht-get* (json-parse-string
                                                                             (load-json-commands))
                                                                            "getEditorState")))
                                                             (setf (symbol-value 'buffer-file-name)
                                                                   "x")
                                                             (serenade--handle-message data)) 
              (expect   'serenade--get-editor-state 
                        :to-have-been-called)) 
          (it "diffs" (let* ((data
                              (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                        (setf (symbol-value 'buffer-file-name) "x") 
                        (serenade--handle-message data)) 
              (expect   'serenade--diff 
                        :to-have-been-called)) 
          (it "diffs" (let* ((data (ht-get* (json-parse-string (load-json-commands))
                                            "evaluateInPlugin")))
                        (setf (symbol-value 'buffer-file-name) "x") 
                        (serenade--handle-message data)) 
              (expect   'serenade--evaluate-in-plugin 
                        :to-have-been-called)))
;; (let* ((data
;;         ;; (load-json-commands)
;;         (json-parse-string(load-json-commands))))
;;   (debug)
;;   (serenade--handle-message data))
