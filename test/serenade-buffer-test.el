(require 'serenade-buffer)

(describe "determines serenade buffer"     ;;
          (it "returns nil if no filetype" ;;
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
