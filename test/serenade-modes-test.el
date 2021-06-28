
(describe "initializes mode config" ;;
          (before-each ) 
          (it "contains global config" ;;
              (let* ((x  (ht-get* serenade-mode-config-map "global"))) 
                (expect  (serenade-mode-configuration-mode x) 
                         :to-equal 'global))) 
          (it "sets mode config to global by default" ;;
              (serenade--set-active-mode-configuration) 
              (expect  (serenade-mode-configuration-mode serenade-active-mode-configuration) 
                       :to-equal 'global)) 
          (it "global config uses default functions" ;;
              (serenade--set-active-mode-configuration) 
              (expect  (serenade-mode-configuration-diff serenade-active-mode-configuration) 
                       :to-equal 'serenade--diff) 
              (expect  (serenade-mode-configuration-get-editor-state
                        serenade-active-mode-configuration)
                       :to-equal 'serenade--get-editor-state)))
(describe "custom mode config" ;;
          (before-each (serenade--initialize-mode-config-map) 
                       (spy-on 'message)) 
          (it "adds mode configs" (serenade--configure-mode :mode 'org-mode) 
              (expect  (length (ht-items serenade-mode-config-map) ) 
                       :to-equal 5)
              ;; (expect  (ht-keys serenade-mode-config-map)
              ;;          :to-equal '("org-mode" "global"))
              ) 
          (it "sets active mode to current major mode" (serenade--configure-mode :mode 'org-mode) 
              (create-test-buffer "test.org" "") 
              (org-mode) 
              (serenade--set-active-mode-configuration) 
              (expect  (serenade-mode-configuration-mode serenade-active-mode-configuration) 
                       :to-equal 'org-mode)))
(describe "calls handlers" ;;
          (before-each (serenade--initialize-mode-config-map) 
                       (spy-on 'serenade--get-editor-state) 
                       (spy-on 'serenade--send-editor-state) 
                       (spy-on 'message)) 
          (it "calls get-editor-state fn" (serenade--configure-mode :mode 'org-mode 
                                                                    :get-editor-state (lambda () 
                                                                                        (message
                                                                                         "test2")))
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "getEditorState"))) 
                (serenade--handle-message data)) 
              (expect  'serenade--get-editor-state 

                       :not 
                       :to-have-been-called) 
              (expect  'message 
                       :to-have-been-called-with "test2")) 
          (it "calls diff fn" (serenade--configure-mode :mode 'org-mode 
                                                        :diff '(lambda (s c) 
                                                                 (message "test"))) 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect  'serenade--diff 

                       :not 
                       :to-have-been-called) 
              (expect  'message 
                       :to-have-been-called-with "test")) 
          (it "calls post-edit fn" (serenade--configure-mode :mode 'org-mode 
                                                             :post-edit '(lambda () 
                                                                           (message "3"))) 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect  'message 
                       :to-have-been-called-with "3")) 
          (it "calls pre-edit fn" (serenade--configure-mode :mode 'org-mode 
                                                            :pre-edit '(lambda () 
                                                                         (message "4"))) 
              (let* ((data (ht-get* (json-parse-string (load-json-commands)) "diff"))) 
                (serenade--handle-message data)) 
              (expect  'message 
                       :to-have-been-called-with "4")))
(describe "determines serenade buffer"     ;;
          (it "returns nil if no filetype" ;;
              (switch-to-buffer (get-buffer-create "test")) 
              (serenade--set-serenade-buffer) 
              (expect  serenade-buffer 
                       :to-equal nil)) 
          (it "returns nil if invalid filetype" ;;
              (switch-to-buffer (get-buffer-create "test.xx")) 
              (setq buffer-file-name "test.xx") 
              (serenade--set-serenade-buffer) 
              (expect  serenade-buffer 
                       :to-equal nil)) 
          (it "sets if valid filetype" ;;
              (let* ((buff  (get-buffer-create "test.js"))) 
                (switch-to-buffer buff) 
                (setq buffer-file-name "test.js") 
                (serenade--set-serenade-buffer) 
                (expect  serenade-buffer 
                         :to-equal buff))))
