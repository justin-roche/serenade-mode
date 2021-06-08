
(require 'serenade-mode)
(require 'test-utils)
(require 'serenade-socket)
(setq lexical-binding t)

(describe "Connection stop" ;;
          (before-each   ) 
          (it "calls disconnect function" (spy-on-fn 'serenade--disconnect) 
              (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade--disconnect 
                      :to-have-been-called) ))

(describe "Mode initialization" ;;
          (before-each (spy-on-fn 'serenade-mode--stop)) 
          (it "calls start function" ;;
              (spy-on-fn 'serenade-mode--start) 
              (serenade-mode) 
              (expect 'serenade-mode--start 
                      :to-have-been-called)) 
          (it "calls stop function" ;;
              (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade-mode--stop 
                      :to-have-been-called)))
