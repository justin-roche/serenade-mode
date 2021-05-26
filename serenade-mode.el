
(defun serenade-reset () 
  (if (not (equal nil serenade-heartbeat-timer)) 
      (cancel-timer serenade-heartbeat-timer)))
(serenade-reset)

(defun serenade-mode-start () 
  "Major mode for using Serenade" 
  (interactive) 
  (print "connecting to serenade") 
  (serenade-open-socket) 
  (serenade-toggle-double-line-numbers-on) 
  (serenade-connect)
  ;; (setq serenade-heartbeat-timer (run-with-timer 0 10 'serenade-heartbeat))
  )

(defun serenade-mode-stop () 
  (interactive)
  ;; (serenade-toggle-double-line-numbers-off)
  (serenade--heartbeat-stop) 
  (if (not ( equal s-websocket nil )) 
      (progn (print "disconnecting from serenade") 
             (serenade-close-socket))))

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
;; (serenade-mode-toggle)
