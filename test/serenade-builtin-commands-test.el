(describe "calls builtin commands without arguments" ;;
          (before-each (spy-on 'serenade--send-completed) 
                       (spy-on 'serenade--open-file) 
                       (spy-on 'serenade--execute-builtin-command)) 
          (it "calls builtin command handler for save" ;;
              (let* ((req (load-request "save"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'serenade--execute-builtin-command 
                          :to-have-been-called))))
(describe "calls builtin commands with arguments" ;;
          (before-each (spy-on 'serenade--send-completed) 
                       (spy-on 'serenade--switch-tab) 
                       (spy-on 'scroll-down-command) 
                       (spy-on 'scroll-up-command) 
                       (spy-on 'serenade--open-file)) 
          (it "calls builtin command handler for scroll" ;;
              (let* ((req (load-request "scroll"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'scroll-up-command 
                          :to-have-been-called))) 
          (it "calls builtin command handler for scroll down" ;;
              (let* ((req (load-request "scrollDown"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'scroll-up-command 
                          :to-have-been-called))) 
          (it "calls builtin command handler for open <file>" ;;
              (let* ((req (load-request "openIndexjs"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'serenade--open-file 
                          :to-have-been-called-with "index.js"))) 
          (it "calls builtin command handler for <nth> tab" ;;
              (let* ((req (load-request "firstTab"))) 
                (create-test-buffer "test.js" "abc") 
                (serenade--handle-message req) 
                (expect   'serenade--switch-tab 
                          :to-have-been-called-with 1))))

(describe "Calls cut" ;;
          (before-each (spy-on 'websocket-send-text) 
                       (spy-on 'serenade--cut-selection)) 
          (it "calls cut if valid buffer" ;;
              (let* ((req (load-request "cut"))) 
                (create-test-buffer "test.js" "") 
                (serenade--handle-message req)) 
              (expect   'serenade--cut-selection 
                        ':to-have-been-called)))
(describe "gives correct result using target functions" ;;
          (before-each (serenade--initialize-mode-config-map) 
                       (setq serenade-evil nil) 
                       (spy-on 'websocket-send-text)) 
          (it "selects lines by number with emacs default functions" ;;
              (create-test-buffer "test6.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "selectLine2"))) 
                (serenade--handle-message req)) 
              (expect (region-beginning) 
                      :to-equal 11) 
              (expect (region-end) 
                      :to-equal 20)) 
          (it "goes to lines by number" ;;
              (create-test-buffer "test2.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "goLine2"))) 
                (serenade--handle-message req)) 
              (expect (point) 
                      :to-equal 11)) 
          (it "selects lines by number in evil-mode" ;;
              (create-test-buffer "test9.js" "let x = 1\n let y = 2") 
              (setq serenade-evil t ) 
              (let* ((req (load-request "selectLine2"))) 
                (serenade--handle-message req)) 
              (expect (region-beginning) 
                      :to-equal 11) 
              (expect (region-end) 
                      :to-equal 19)) 
          (it "cuts lines by number" ;;
              (create-test-buffer "test5.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "cutLine2"))) 
                (serenade--handle-message req)) 
              (expect   (buffer-string) 
                        :to-equal "let x = 1\n")))

(describe "gives correct result using keypress functions" ;;
          (before-each (spy-on 'websocket-send-text)) 
          (it "copies a selection" ;;
              (create-test-buffer "test3.js" "let x = 1\nlet y = 2\n") 
              (serenade--select-target 11 20) 
              (let* ((req2 (load-request "copy"))) 
                (serenade--handle-message req2)) 
              (expect    (car kill-ring-yank-pointer) 
                         :to-equal "let y = 2\n")) 
          (it "pastes evil" ;;
              (create-test-buffer "test7.js" "let x = 1\nlet y = 2") 
              (setq serenade-evil t) 
              (let* ((req1 (load-request "copyLine1")) 
                     (req2 (load-request "paste"))) 
                (serenade--handle-message req1) 
                (serenade--handle-message req2)) 
              (expect   (buffer-string) 
                        :to-equal "let x = 1\nlet y = 2\n\nlet x = 1")))

;; (describe "gives correct result using undo" ;;
;;           ;; (before-each (spy-on 'websocket-send-text))
;;           ;; (it "performs undo" ;;
;;           ;;     (create-test-buffer "test8.js" "")
;;           ;;     (let* ((req1 (load-request "addLetX"))
;;           ;;            (req2 (load-request "undo")))
;;           ;;       (serenade--handle-message req1)
;;           ;;       ;; (serenade--handle-message req2)
;;           ;;       )
;;           ;;     (expect    (buffer-string)
;;           ;;                :to-equal ""))
;;           )
