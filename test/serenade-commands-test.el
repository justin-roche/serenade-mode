
(require 'ht)
(require 'serenade-mode)
(require 'serenade-commands)
(require 'serenade-helm)
(require 'test-utils)

(describe "Global Builtin Commands" ;;
          (before-each (serenade--clear-mode-maps) 
                       (serenade--initialize-mode-maps)) 
          (it "contains builtin commands in global speech map" ;;
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal (length serenade--global-defaults)) 
              (expect (ht-get* serenade-speech-maps "global" "save" "command") 
                      :to-equal 'save-buffer )) 
          (it "adds items to helm map" ;;
              (expect (length (ht-items serenade-helm-map)) 
                      :to-equal 12)))
(describe "Global Custom Commands" ;;
          (before-each (serenade--clear-mode-maps)) 
          (it "adds to global speech map" ;;
              (serenade-global-set-speech "a" 'b) 
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "a"  "command") 
                      :to-equal 'b )) 
          (it "adds a list of speech command pairs to global speech map" (serenade-global-set-speech
                                                                          '( ("a" . b) ))
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "a"  "command") 
                      :to-equal 'b )) 
          (it "adds a list of multiple speech command pairs to global speech map"
              (serenade-global-set-speech '( ("a" . b)
                                             ("c" . d) 
                                             ("e" . f) 
                                             ("g" . h))) 
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 4) 
              (expect (ht-get*  (serenade--get-global-map) "a"  "command") 
                      :to-equal 'b ) 
              (expect (ht-get*  (serenade--get-global-map) "c"  "command") 
                      :to-equal 'd ) 
              (expect (ht-get*  (serenade--get-global-map) "e"  "command") 
                      :to-equal 'f ) 
              (expect (ht-get*  (serenade--get-global-map) "g"  "command") 
                      :to-equal 'h )) 
          (it "determines speech value when command only is provided" ;;
              (serenade-global-set-speech 'uncomment-region) 
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "uncomment region"  "command") 
                      :to-equal 'uncomment-region )))

(describe "Mode Custom Commands" ;;
          (before-each (serenade--initialize-mode-maps)) 
          (it "adds to existing mode voice maps" ;;
              (progn (serenade-define-speech 'org-mode "a" 'b) 
                     (serenade-define-speech 'org-mode "c" 'd)) 
              (expect(ht-get* serenade-speech-maps "org-mode"  "a" "command") 
                     :to-equal 'b) 
              (expect(ht-get* serenade-speech-maps "org-mode"  "c" "command") 
                     :to-equal 'd)))

(describe "Finding voice bindings" ;;
          (before-each 
           (setq minor-mode-map-alist '()) 
           (serenade--initialize-mode-maps) ) 
          (it "finds voice binding for minor mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'b)) 
              (setq minor-mode-map-alist '((edebug-mode nil))) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for major mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'b)) 
              (setq major-mode 'edebug-mode ) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for minor mode before major mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'b) 
                     (serenade-define-speech 'org-mode "a" 'c)) 
              (setq minor-mode-map-alist '((org-mode nil))) 
              (defun major-mode () 
                'edebug-mode) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'c)) 
          (it "finds voice binding for minor mode before global mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'b) 
                     (serenade-global-set-speech "a" 'c)) 
              (setq minor-mode-map-alist '((edebug-mode nil))) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)))
