(defun serenade-start-application () 
  (interactive) 
  (shell-command (concat "open -a Serenade " ) nil nil) 
  (run-at-time "3 sec" nil (lambda nil  (shell-command (concat "open -a Emacs " ) nil nil))))
