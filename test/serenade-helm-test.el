
(describe "Updates serenade helm M-x map" ;;
          (before-each (setf serenade-helm-M-x-map (ht)) 
                       (serenade--clear-speech-maps)) 
          (it "adds new voice binding for commad" (serenade--update-helm-M-x-map "a" 'b) 
              (expect   (ht-get* serenade-helm-M-x-map "b") 
                        :to-equal "a")) 
          (it "adds synonyms" (serenade--update-helm-M-x-map "a" 'b) 
              (serenade--update-helm-M-x-map "c" 'b) 
              (expect   (ht-get* serenade-helm-M-x-map "b") 
                        :to-equal "c | a")) 
          (it "does not add previously added bindings" (serenade--update-helm-M-x-map "a" 'b) 
              (serenade--update-helm-M-x-map "a" 'b) 
              (expect   (ht-get* serenade-helm-M-x-map "b") 
                        :to-equal "a")))
(describe "calls to define-speech update helm M-x map" ;;
          (before-each (setf serenade-helm-M-x-map (ht)) 
                       (serenade--clear-speech-maps)) 
          (it "adds new M-x binding for command if M-x enabled" ;;
              (setq serenade-helm-M-x t) 
              (serenade-global-set-speech "a" 'b) 
              (expect   (ht-get* serenade-helm-M-x-map "b") 
                        :to-equal "a")) 
          (it "does not add new M-x binding for command if M-x disabled" ;;
              (setq serenade-helm-M-x nil) 
              (serenade-global-set-speech "a" 'b) 
              (expect   (ht-get* serenade-helm-M-x-map "b") 
                        :to-equal nil)))
(describe "gets unrestricted serenade-commands list" ;;
          (before-each (setf serenade-helm-M-x-map (ht)) 
                       (serenade--clear-speech-maps))
          ;;
          (it "includes inactive modes" ;;
              (serenade-global-set-speech "a" 'b) 
              (expect  (length (ht-items serenade-speech-maps)) 
                       :to-equal 1) 
              (serenade--get-helm-candidates serenade-speech-maps) 
              (expect  (length serenade--helm-candidates) 
                       :to-equal 1)))
(describe "gets active serenade-commands list" ;;
          (before-each (setf serenade-helm-M-x-map (ht)) 
                       (serenade--clear-speech-maps))
          ;;
          (it "excludes inactive modes" ;;
              (serenade-define-speech 'a-mode "a" 'b) 
              (serenade--get-helm-active-candidates serenade-speech-maps) 
              (expect  (length serenade--helm-candidates) 
                       :to-equal 0)) 
          (it "excludes inactive modes and includes active modes" ;;
              (serenade-define-speech 'a-mode "a" 'b) 
              (serenade-define-speech 'b-mode "c" 'd) 
              (setq a-mode t ) 
              (setq b-mode nil ) 
              (serenade--get-helm-active-candidates serenade-speech-maps) 
              (expect  (length serenade--helm-candidates) 
                       :to-equal 1)) 
          (it "excludes global bindings" ;;
              (serenade-define-speech 'global "c" 'd) 
              (serenade--get-helm-active-candidates serenade-speech-maps) 
              (expect  (length serenade--helm-candidates) 
                       :to-equal 1)) 
          (it "excludes current major mode bindings" ;;
              (serenade-define-speech 'z-mode "c" 'd) 
              (setq z-mode t ) 
              (serenade--get-helm-active-candidates serenade-speech-maps) 
              (expect  (length serenade--helm-candidates) 
                       :to-equal 1)))
