
(require 'ht)
(require 'serenade-custom-commands)

(describe "Global Builtin Commands" (before-each (serenade--initialize-mode-maps)) 
          (it "contains builtin commands in global speech map" (expect (ht-get* serenade-mode-maps
                                                                                "global" "save"
                                                                                "command")
                                                                       :to-equal 'save-buffer )))
(describe "Global Custom Commands" (before-each (setf (symbol-function
                                                       'serenade--add-default-bindings)
                                                      (lambda ())) 
                                                (spy-on 'serenade--add-default-bindings) 
                                                (serenade--initialize-mode-maps)) 
          (it "adds to global speech map" (serenade-global-set-speech "treemacs rename"
                                                                      'treemacs-rename)
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "treemacs rename"  "command") 
                      :to-equal 'treemacs-rename )) 
          (it "adds a list of speech command pairs to global speech map" 
              (defun serenade--read-data () 
                nil) 
              (serenade-global-set-speech '( ("treemacs rename" . treemacs-rename) )) 
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "treemacs rename"  "command") 
                      :to-equal 'treemacs-rename )) 
          (it "determines speech value when command only is provided" (serenade-global-set-speech
                                                                       'uncomment-region)
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "uncomment region"  "command") 
                      :to-equal 'uncomment-region )))

(describe "Mode Custom Commands" (before-each (serenade--initialize-mode-maps)) 
          (it "adds to existing mode voice maps" (progn (serenade-define-speech 'org-mode "a" 'b) 
                                                        (serenade-define-speech 'org-mode "c" 'd)) 
              (expect(ht-get* serenade-mode-maps "org-mode"  "a" "command") 
                     :to-equal 'b) 
              (expect(ht-get* serenade-mode-maps "org-mode"  "c" "command") 
                     :to-equal 'd)))

(describe "Finding voice bindings" (before-each 
                                    (setq minor-mode-map-alist '()) 
                                    (serenade--initialize-mode-maps) ) 
          (it "finds voice binding for minor mode" (progn (serenade-define-speech 'edebug-mode "a"
                                                                                  'b))
              (setq minor-mode-map-alist '((edebug-mode nil))) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for major mode" (progn (serenade-define-speech 'edebug-mode "a"
                                                                                  'b))
              (defun major-mode () 
                'edebug-mode) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for minor mode before major mode" (progn (serenade-define-speech
                                                                             'edebug-mode "a" 'b)
                                                                            (serenade-define-speech
                                                                             'org-mode "a" 'c))
              (setq minor-mode-map-alist '((org-mode nil))) 
              (defun major-mode () 
                'edebug-mode) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'c)) 
          (it "finds voice binding for minor mode before global mode" (progn (serenade-define-speech
                                                                              'edebug-mode "a" 'b)
                                                                             (serenade-global-set-speech
                                                                              "a" 'c)) 
              (setq minor-mode-map-alist '((edebug-mode nil))) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)))
