
(require 'test-utils)
(require 'serenade-mode)
(require 'serenade-socket)
(setq lexical-binding t)

(describe "Initial value of mode variables"     ;;
          (it "initial value of mode variables" ;;
              (expect serenade-snippet-engine 
                      :to-be nil) 
              (expect serenade-helm-M-x 
                      :to-be nil) 
              (expect serenade-completion-frontend 
                      :to-be nil) 
              (expect serenade-snippet-engine 
                      :to-be nil)))
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
          (it "initializes snippet engine settings" ;;
              (serenade-mode) 
              (expect 'serenade--initialize-snippet-engine 
                      :to-have-been-called)))

(describe "sets values for optional dependencies" ;;
          (before-each (spy-on 'serenade--connect) 
                       (setq serenade-completion-frontend nil ) 
                       (setq serenade-snippet-engine nil )) 
          (it "sets serenade--helm-M-x-active to nil" ;;
              (setq serenade-helm-M-x nil) 
              (serenade-mode) 
              (expect serenade--helm-M-x-active 
                      :to-be nil)) 
          (it "sets serenade--helm-M-x-active to true" ;;
              (setq serenade-completion-frontend 'helm ) 
              (setq serenade-helm-M-x t) 
              (serenade-mode) 
              (expect serenade--helm-M-x-active 
                      :to-be t)))
(describe "Connection stop" ;;
          (before-each  (spy-on 'serenade--disconnect) ) 
          (it "calls disconnect function" (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade--disconnect 
                      :to-have-been-called) 
              (serenade-mode-toggle)))
