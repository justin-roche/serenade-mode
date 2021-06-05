
(require 'ht)
(require 'serenade-socket)
(require 'test-utils)
(setq lexical-binding t)

(describe "registers plugin" (before-each ) 
          (it "calls open socket and heartbeat from connect" ;;
              (cl-defun 
                  fake-websocket-open
                  (url &key on-open on-message on-close) 
                (message "on open") 
                (funcall on-open 1)) 
              (setf (symbol-function 'websocket-open) 'fake-websocket-open) 
              (spy-on-fn 'serenade--register) 
              (spy-on-fn 'serenade--heartbeat-start) 
              (serenade--connect) 
              (expect 'serenade--register 
                      :to-have-been-called) 
              (expect 'serenade--heartbeat-start 
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
