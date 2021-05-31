(defun serenade-start-application () 
  (interactive) 
  (serenade--start-application))

(defun serenade--start-application () 
  (shell-command (concat "open -a Serenade " ) nil nil) 
  (run-at-time "3 sec" nil (lambda nil  (shell-command (concat "open -a Emacs " ) nil nil))))

(provide 'serenade-desktop)
