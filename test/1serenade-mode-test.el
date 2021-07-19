
(require 'test-utils)
(require 'serenade-mode)
(require 'serenade-socket)
(setq lexical-binding t)

(describe "Load behaviors" ;;
          (before-each (load "./serenade-mode.el")) 
          (it "initializes maps" ;;
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal (+ (length serenade--generated-global-defaults) 
                                   (length serenade--builtin-global-defaults)))))
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
(describe "Start function" ;;
          (before-each (spy-on 'serenade--initialize-speech-maps) 
                       (spy-on 'serenade--generate)) 
          (it "calls sync function" ;;
              (serenade-mode) 
              (expect 'serenade--generate 
                      :to-have-been-called)))
(describe "Connection start" ;;
          (before-each  (spy-on 'serenade--connect) ) 
          (it "calls connect function" ;;
              (setq serenade-mode t ) 
              (serenade-mode) 
              (expect 'serenade--connect 
                      :to-have-been-called)))

(describe "runs with default values for optional dependencies" ;;
          (before-each (spy-on 'serenade--initialize-speech-maps) 
                       (spy-on 'serenade--initialize-completion-frontend) 
                       (spy-on 'serenade--initialize-snippet-engine) 
                       (spy-on 'serenade--connect) 
                       (spy-on 'serenade--generate)) 
          (it "initializes completion frontend settings" ;;
              (serenade-mode) 
              (expect 'serenade--initialize-completion-frontend 
                      :to-have-been-called)) 
          (it "initial value of snippet-engine is nil" ;;
              (serenade-mode) 
              (expect serenade-snippet-engine 
                      :to-be nil)) 
          (it "initializes snippet engine settings" ;;
              (serenade-mode) 
              (expect 'serenade--initialize-snippet-engine 
                      :to-have-been-called)) 
          (it "initial value of completion-frontend is 'helm" ;;
              (serenade-mode) 
              (expect serenade-completion-frontend 
                      :to-be 'helm)) 
          (it "sets serenade--helm-M-x-active to true" ;;
              (serenade-mode) 
              (expect serenade--helm-M-x-active 
                      :to-be t)) 
          (it "calls connect function" ;;
              (setq serenade-mode t ) 
              (serenade-mode) 
              (expect 'serenade--connect 
                      :to-have-been-called)))

(describe "runs with nil values for optional dependencies" ;;
          (before-each (spy-on 'serenade--connect) 
                       (setq serenade-completion-frontend nil ) 
                       (setq serenade-snippet-engine nil )) 
          (it "sets serenade--helm-M-x-active to nil" ;;
              (serenade-mode) 
              (expect serenade--helm-M-x-active 
                      :to-be nil)) 
          (it "calls connect function" ;;
              (setq serenade-mode t ) 
              (serenade-mode) 
              (expect 'serenade--connect 
                      :to-have-been-called)))
(describe "Connection stop" ;;
          (before-each  (spy-on 'serenade--disconnect) ) 
          (it "calls disconnect function" (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade--disconnect 
                      :to-have-been-called) 
              (serenade-mode-toggle)))
