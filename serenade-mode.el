
;; scroll-triggers press
;; paste- probably s-v
;; cut- probably s-x
;; copy- probably s-x

;; undo- loses cursor
;; redo- loses cursor

;; (load "~/.spacemacs.d/apps/ht.el")
;; (load "~/.spacemacs.d/apps/serenade-spacemacs.el")
;; (load "~/.spacemacs.d/apps/serenade-custom-commands.el")
;; (load "~/.spacemacs.d/apps/serenade-commands.el")
;; (require 'websocket)
;; (require 's)
;; (require 'dash)

;;;###autoload
(setq s-websocket nil)




(defun serenade-send-completed () 
  (print "sending completed") 
  (let* ((response (ht("message" "complete") 
                      ("data" nil))) 
         (response-json (json-serialize response))) 
    (websocket-send-text s-websocket response-json)))



(defun log-to-scratch (message) 
  (print message (get-buffer "*scratch*")))


(defun serenade-mode-stop () 
  (interactive) 
  (serenade-toggle-double-line-numbers-off) 
  (if (not (equal nil serenade-heartbeat-timer)) 
      (cancel-timer serenade-heartbeat-timer)) 
  (if (not ( equal s-websocket nil )) 
      (progn (print "disconnecting from serenade") 
             (serenade-close-socket))))


(defun serenade-mode-start () 
  "Major mode for using Serenade" 
  (interactive) 
  (print "connecting to serenade") 
  (serenade-open-socket) 
  (serenade-toggle-double-line-numbers-on) 
  (serenade-connect)
  ;; (setq serenade-heartbeat-timer (run-with-timer 0 10 'serenade-heartbeat))
  )

(defun serenade-mode-toggle ()
  ;; (serenade-set-double-line-numbers)
  ;; (debug)
  (if (eq nil serenade-mode) 
      (call-interactively 'serenade-mode-stop) 
    (serenade-mode-start)))

(define-minor-mode serenade-mode "Toggle Serenade mode." 
  nil
  " Serenade" 
  :lighter " serenade " 
  :keymap nil 
  :global t
  (serenade-mode-toggle))


(provide 'serenade-mode)

(defun serenade-reset () 
  (if (not (equal nil serenade-heartbeat-timer)) 
      (cancel-timer serenade-heartbeat-timer)))
(serenade-reset)

;; (serenade-mode-toggle)
