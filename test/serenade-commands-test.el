
(require 'ht)
(require 'serenade-mode)
(require 'serenade-commands)
(require 'serenade-helm)
(require 'test-utils)

(describe "Default Global Builtin Commands" ;;
          (before-each (serenade--clear-mode-maps) 
                       (setq serenade--add-custom-global-defaults nil ) 
                       (serenade--initialize-mode-maps)) 
          (it "contains builtin commands in global speech map" ;;
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal (length serenade--builtin-global-defaults)) 
              (expect (ht-get* serenade-speech-maps "global" "save" "command") 
                      :to-equal 'save-buffer )) 
          (it "adds builtin items to helm map" ;;
              (expect (length (ht-items serenade-helm-M-x-map))
                      ;; subtract 1 for the homophone scroll/scroll down
                      :to-equal (-  (length (-filter '(lambda (item) 
                                                        (not (eq nil (cdr item))))
                                                     serenade--builtin-global-defaults)) 1))))

(describe "Default Global Custom Commands" ;;
          (before-each 
           (setq serenade--add-custom-global-defaults t ) 
           (setq serenade--add-builtin-global-defaults nil ) 
           (serenade--clear-mode-maps) 
           (serenade--initialize-mode-maps)) 
          (after-each 
           (setq serenade--add-builtin-global-defaults t )) 
          (it "contains default custom commands in global speech map" ;;
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal (length serenade--custom-global-defaults)) 
              (expect (ht-get* serenade-speech-maps "global" "snippet <name>" "command") 
                      :to-equal 'serenade--insert-yasnippet )) 
          (it "adds default custom items to helm map" ;;
              (expect (length (ht-items serenade-helm-M-x-map)) 
                      :to-equal (length (-filter '(lambda (item) 
                                                    (not (eq nil (cdr item))))
                                                 serenade--custom-global-defaults)))))
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
                      :to-equal 'h )))
(describe "Define-speech" ;;
          (before-each (serenade--initialize-mode-maps)) 
          (it "define-speech can be called with a list" ;;
              (progn (serenade-define-speech 'org-mode '(("a" . b) 
                                                         ("c" . d)))) 
              (expect(ht-get* serenade-speech-maps "org-mode"  "a" "command") 
                     :to-equal 'b) 
              (expect(ht-get* serenade-speech-maps "org-mode"  "c" "command") 
                     :to-equal 'd)))
(describe "Auto-define-speech" ;;
          (before-each (serenade--clear-mode-maps)) 
          (it "determines speech value when one command only is provided" ;;
              (serenade-auto-define-speech 'global 'uncomment-region) 
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "uncomment region"  "command") 
                      :to-equal 'uncomment-region )) 
          (it "determines speech value when multiple commands are provided" ;;
              (serenade-auto-define-speech 'global '(uncomment-region clear-buffer)) 
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 2) 
              (expect (ht-get*  (serenade--get-global-map) "clear buffer"  "command") 
                      :to-equal 'clear-buffer ) 
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
           (setq minor-mode-map-alist '((rjsx-mode nil) 
                                        (edebug-mode nil))) 
           (setq edebug-mode t) 
           (setq rjsx-mode nil) 
           (setq major-mode 'org-mode ) 
           (setq org-mode t ) 
           (serenade--initialize-mode-maps) ) 
          (it "finds voice binding for minor mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'c)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'c)) 
          (it "finds voice binding for major mode" ;;
              (progn (serenade-define-speech 'org-mode "a" 'b)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for minor mode before major mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'c) 
                     (serenade-define-speech 'org-mode "a" 'b)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'c)) 
          (it "finds voice binding for active minor mode before inactive minor mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'y) 
                     (serenade-define-speech 'rjsx-mode "a" 'z)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'y)) 
          (it "finds voice binding for major mode before inactive minor mode" ;;
              (progn (serenade-define-speech 'org-mode "a" 'p) 
                     (serenade-define-speech 'rjsx-mode "a" 'z)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'p)) 
          (it "finds voice binding for minor mode before global mode" ;;
              (progn (serenade-define-speech 'edebug-mode "a" 'b) 
                     (serenade-global-set-speech "a" 'c)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for major mode before global mode" ;;
              (progn (serenade-define-speech 'org-mode "a" 'd) 
                     (serenade-global-set-speech "a" 'c)) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'd)))
