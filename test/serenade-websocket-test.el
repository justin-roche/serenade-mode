(describe "Prompts for start on error" ;;
          (before-each (spy-on 'serenade-start-prompt)) 
          (it "calls open socket from connect" ;;
              (setq serenade-prompt-for-application-start t) 
              (setq serenade-port 000) 
              (serenade--connect) 
              (expect 'serenade-start-prompt 
                      :to-have-been-called)))
(describe "register sends valid message" ;;
          (before-each (spy-on 'websocket-send-text) 
                       (bc/set-var 'serenade-reuse-id-on-connect t) 
                       (bc/set-var 'serenade-id 9999)) 
          (after-each (bc/revert-serenade-id) 
                      (bc/revert-serenade-reuse-id-on-connect)) 
          (it "sends register message" ;;
              (let* ((data (json-serialize (ht-get* (json-parse-string (load-json-responses))
                                                    "register")))) 
                (setq serenade-reuse-id-on-connect t ) 
                (serenade--register ) 
                (expect   'websocket-send-text 
                          :to-have-been-called-with nil data))))
(describe "registers plugin" ;;
          (before-each  (spy-on 'serenade--register) 
                        (spy-on 'serenade--heartbeat-stop) 
                        (spy-on 'serenade--heartbeat-start)) 
          (it "calls register and heartbeat from connect" ;;
              (cl-defun 
                  fake-websocket-open
                  (url &key on-open on-message on-close) 
                (funcall on-open 1)) 
              (setf (symbol-function 'websocket-open) 'fake-websocket-open) 
              (serenade--connect) 
              (expect 'serenade--register 
                      :to-have-been-called) 
              (expect 'serenade--heartbeat-start 
                      :to-have-been-called)) 
          (it "calls heartbeat-stop on socket close" ;;
              (cl-defun 
                  fake-websocket-open
                  (url &key on-open on-message on-close) 
                (funcall on-close 1)) 
              (setf (symbol-function 'websocket-open) 'fake-websocket-open) 
              (serenade--connect) 
              (expect 'serenade--heartbeat-stop 
                      :to-have-been-called)))
(describe "Connects to Serenade" ;;
          (before-each (spy-on 'serenade--open-socket) 
                       (spy-on 'serenade--disconnect)) 
          (it "calls open socket from connect" ;;
              (serenade--connect) 
              (expect 'serenade--open-socket 
                      :to-have-been-called)) 
          (it "sanity check" ;;
              (serenade-disconnect) 
              (expect 'serenade--disconnect 
                      :to-have-been-called)))
