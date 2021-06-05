

(provide 'serenade-helm)
(setq lexical-binding t)
(require 'ht)

(describe "Updates serenade helm map" ;;
          (before-each (setf serenade-helm-map (ht))) 
          (it "adds new voice binding for commad" (serenade--update-helm-map "a" 'b) 
              (expect   (ht-get* serenade-helm-map "b") 
                        :to-equal "a")) 
          (it "adds synonyms" (serenade--update-helm-map "a" 'b) 
              (serenade--update-helm-map "c" 'b) 
              (expect   (ht-get* serenade-helm-map "b") 
                        :to-equal "c | a")) 
          (it "does not add previously added bindings" (serenade--update-helm-map "a" 'b) 
              (serenade--update-helm-map "a" 'b) 
              (expect   (ht-get* serenade-helm-map "b") 
                        :to-equal "a")))
(describe "calls to define-speech update helm map" ;;
          (before-each                             ;;
           (setq serenade-helm-map (ht))) 
          (it "adds new M-x binding for command if M-x enabled" ;;
              (setq serenade-helm-M-x t) 
              (serenade-global-set-speech "a" 'b) 
              (expect   (ht-get* serenade-helm-map "b") 
                        :to-equal "a")) 
          (it "does not add new M-x binding for command if M-x disabled" ;;
              (setq serenade-helm-M-x nil) 
              (serenade-global-set-speech "a" 'b) 
              (expect   (ht-get* serenade-helm-map "b") 
                        :to-equal nil)))
