
(require 'serenade-mode)
(require 'test-utils)
(require 'serenade-socket)
(setq lexical-binding t)

(describe "Connection stop" ;;
          (before-each  (spy-on 'serenade--disconnect) ) 
          (it "calls disconnect function" (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade--disconnect 
                      :to-have-been-called) ))

(describe "Mode initialization" ;;
          (before-each (spy-on 'serenade-mode--start) 
                       (spy-on 'serenade-mode--stop)) 
          (it "calls start function" ;;
              (serenade-mode) 
              (expect 'serenade-mode--start 
                      :to-have-been-called)) 
          (it "calls stop function" ;;
              (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade-mode--stop 
                      :to-have-been-called)))
(describe "Connection start" ;;
          (before-each  (spy-on 'serenade--connect) ) 
          (it "calls start function" ;;
              (setq serenade-mode t ) 
              (serenade-mode) 
              (expect 'serenade--connect 
                      :to-have-been-called)))
