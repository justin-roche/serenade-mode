
(require 'ht)
(require 'serenade-custom-commands)

(describe "Global Custom Commands" (before-each (serenade-initialize-mode-maps)) 
          (it "adds to global speech map" (serenade-global-set-speech "treemacs rename"
                                                                      'treemacs-rename)
              (expect (length (ht-items (serenade--get-global-map))) 
                      :to-equal 1) 
              (expect (ht-get*  (serenade--get-global-map) "treemacs rename"  "command") 
                      :to-equal 'treemacs-rename )) 
          (it "adds a list of speech command pairs to global speech map" (serenade-global-set-speech
                                                                          '( ("treemacs rename" .
                                                                              treemacs-rename) ))
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
(describe "Mode Custom Commands" (before-each (serenade-initialize-mode-maps)) 
          (it "adds to existing mode voice maps" (progn (serenade-define-speech 'org-mode "a" 'b) 
                                                        (serenade-define-speech 'org-mode "c" 'd)) 
              (expect(ht-get* serenade-mode-maps "org-mode"  "a" "command") 
                     :to-equal 'b) 
              (expect(ht-get* serenade-mode-maps "org-mode"  "c" "command") 
                     :to-equal 'd)))
(describe "Finding voice bindings" (before-each (serenade-initialize-mode-maps) ) 
          (it "finds voice binding for minor mode" (progn (serenade-define-speech 'edebug-mode "a"
                                                                                  'b))
              (setq minor-mode-map-alist '((edebug-mode nil))) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)) 
          (it "finds voice binding for minor mode before global mode" (progn (serenade-define-speech
                                                                              'edebug-mode "a" 'b)
                                                                             (serenade-global-set-speech
                                                                              "a" 'c))
              (setq minor-mode-map-alist '((edebug-mode nil))) 
              (expect (ht-get* (serenade--find-voice-binding "a") "command") 
                      :to-equal 'b)))
;;
;; (serenade-global-set-speech "describe face" 'describe-face)
;; (serenade-global-set-speech "select window %s %s %s" 'select-window)
;; ;; universal arg
;; (serenade-global-set-speech "org promote <level>" 'org-do-promote)
;; (serenade-global-set-speech "promote <level>" 'org-do-promote)
;; (serenade-global-set-speech "promote <level>" 'org-do-promote)
;; ;; (serenade-global-set-speech "org promote <level>" 'org-do-promote)
;; (serenade-define-speech 'org-mode-speech-map "promote %s"  'org-do-promote
;;                         :repeat-arg)

                                        ; (setq serenade-global-map (ht ("comment-dwim" "comment")
                                        ;                                          ("describe-face" "d")))
;; (serenade-global-set-speech "describe face" 'describe-face)
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
