

(require 'serenade-custom-commands)
(require 'serenade-synchronize)
;; (setq lexical-binding t)
(require 'test-utils)

(describe "formats commands with no arguments " ;;
          (before-each (reset-maps)) 
          (it "formats a single command with no arguments" ;;
              (serenade-define-speech 'global "a" 'b) 
              ( serenade--format-commands ) 
              (expect   (serenade-list-to-string serenade--formatted-commands--no-slots) 
                        :to-equal "\"a\":\"(b)\",")) 
          (it "formats multiple simple commands" ;;
              (serenade-define-speech 'global "a" 'b) 
              (serenade-define-speech 'global "c" 'd) 
              ( serenade--format-commands ) 
              (expect    serenade--formatted-commands--no-slots 
                         :to-equal '("\"c\":\"(d)\"," "\"a\":\"(b)\","))) 
          (it "resets formatted commands between invocations" ;;
              (serenade-define-speech 'global "c" 'd) 
              ( serenade--format-commands ) 
              (expect   (length serenade--formatted-commands--no-slots) 
                        :to-equal 1) 
              (reset-maps) 
              (serenade-define-speech 'global "a" 'b) 
              ( serenade--format-commands ) 
              (expect   (length serenade--formatted-commands--no-slots) 
                        :to-equal 1)))

(describe "formats commands with named arguments " ;;
          (before-each (reset-maps)) 
          (it "formats a single command with named arguments" ;;
              (serenade-define-speech 'global "a <z>" 'b) 
              ( serenade--format-commands ) 
              (expect   (length serenade--formatted-commands--named-slots) 
                        :to-equal 1) 
              (expect   (serenade-list-to-string serenade--formatted-commands--named-slots) 
                        :to-equal
                        "serenade.app(\"emacs\").command(`a <%z%>`, async (api, matches) => { api.evaluateInPlugin(`(b ${matches.z} )`) });"))
          (it "formats a single command with multiple named arguments" ;;
              (serenade-define-speech 'global "a <z> <x>" 'b) 
              ( serenade--format-commands ) 
              (expect   (length serenade--formatted-commands--named-slots) 
                        :to-equal 1) 
              (expect   (serenade-list-to-string serenade--formatted-commands--named-slots) 
                        :to-equal
                        "serenade.app(\"emacs\").command(`a <%z%> <%x%>`, async (api, matches) => { api.evaluateInPlugin(`(b ${matches.z} ${matches.x} )`) });" )))
