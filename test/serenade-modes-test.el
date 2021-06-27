(require 'serenade-mode)
(require 'serenade-modes)
(require 'serenade-handler)
(require 'test-utils)
(require 'ht)

(describe "initializes mode config"    ;;
          (it "contains global config" ;;
              (serenade--initialize-mode-config-map) 
              (let* ((x  (ht-get* serenade-mode-config-map "global"))) 
                (expect  (serenade-mode-configuration-mode x) 
                         :to-equal 'global)) 
              (expect  (length (ht-items serenade-mode-config-map) ) 
                       :to-equal 1)) 
          (it "sets mode config to global by default" ;;
              (serenade--initialize-mode-config-map) 
              (serenade--set-active-mode-configuration) 
              (expect  (serenade-mode-configuration-mode serenade-active-mode-configuration) 
                       :to-equal 'global)) 
          (it "global config uses default functions" ;;
              (serenade--initialize-mode-config-map) 
              (serenade--set-active-mode-configuration) 
              (expect  (serenade-mode-configuration-diff serenade-active-mode-configuration) 
                       :to-equal 'serenade--diff) 
              (expect  (serenade-mode-configuration-get-editor-state
                        serenade-active-mode-configuration)
                       :to-equal 'serenade--get-editor-state)) 
          (it "goes to lines by number" ;;
              (serenade--initialize-mode-config-map) 
              (create-test-buffer "test2.js" "let x = 1\n let y = 2") 
              (let* ((req (load-request "goLine2"))) 
                (serenade--handle-message req)) 
              (expect (point) 
                      :to-equal 11)))
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
