
(require 'ht)
(require 'serenade-persistence)

(describe "Persists custom commands to file" (before-each (serenade-initialize-mode-maps)) 
          (it "writes a hashtable to saved data" ;;
              (serenade--save-data (ht ("global" (ht ("b" (ht))))))

              ;; (message serenade-saved-data)
              ;; (debug)
              ;; (expect (ht-get* serenade-saved-data "global" "b"  )
              ;;         :not
              ;;         :to-be-truthy)
              ;; (expect   (serenade--read-data)
              ;;           :to-be-truthy)
              ))
