
(require 'ht)
(require 'serenade-socket)
(require 'test-utils)
(setq lexical-binding t)

(describe "Prompts for start on error" (before-each ) 
          (it "calls open socket from connect" ;;
              (spy-on-fn 'serenade-start-prompt) 
              (setq serenade-prompt-for-application-start t) 
              (serenade--connect) 
              (expect 'serenade-start-prompt 
                      :to-have-been-called)))
(describe "register sends valid message" ;;
          (before-each (spy-on-fn 'websocket-send-text)) 
          (it "sends register message" ;;
              (let* ((data (json-serialize (ht-get* (json-parse-string (load-json-responses))
                                                    "register"))))
                (setq serenade-id 9999 ) 
                (serenade--register ) 
                (expect   'websocket-send-text 
                          :to-have-been-called-with nil data))))
(describe "registers plugin" (before-each ) 
          (it "calls register and heartbeat from connect" ;;
              (cl-defun 
                  fake-websocket-open
                  (url &key on-open on-message on-close) 
                (funcall on-open 1)) 
              (setf (symbol-function 'websocket-open) 'fake-websocket-open) 
              (spy-on-fn 'serenade--register) 
              (spy-on-fn 'serenade--heartbeat-start) 
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
              (spy-on-fn 'serenade--heartbeat-stop) 
              (serenade--connect) 
              (expect 'serenade--heartbeat-stop 
                      :to-have-been-called)))
(describe "Connects to Serenade" (before-each ) 
          (it "calls open socket from connect" ;;
              (spy-on-fn 'serenade--open-socket) 
              (serenade--connect) 
              (expect 'serenade--open-socket 
                      :to-have-been-called)) 
          (it "sanity check" ;;
              (spy-on-fn 'serenade--disconnect) 
              (serenade-disconnect) 
              (expect 'serenade--disconnect 
                      :to-have-been-called)))
;; (describe "Connection start"         ;;
;;           (it "calls start function" ;;
;;               (setq serenade-mode t )
;;               (spy-on-fn 'serenade--connect)
;;               (serenade-mode)
;;               (expect 'serenade--connect
;;                       :to-have-been-called)))
