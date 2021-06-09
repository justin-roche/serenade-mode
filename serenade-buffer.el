(defvar serenade-evil t 
  "If true, use evil commands where possible for default commands")

(defcustom serenade-mode-filetypes '(".js" ".py" ) 
  "The filetypes that can be used as serenade buffers")

(defun serenade--set-serenade-buffer () 
  (if (buffer-file-name) 
      (let* ((ext (format ".%s" (file-name-extension (buffer-file-name))))) 
        (if (member ext serenade-mode-filetypes) 
            (setq serenade-buffer (current-buffer) ) 
          (setq serenade-buffer nil ))) 
    (setq serenade-buffer nil )))

(defun serenade--update-buffer (source cursor)
  ;; (message (prin1-to-string source))
  ;; (message (prin1-to-string cursor))
  (delete-region (point-min) 
                 (point-max)) 
  (insert source) 
  (goto-char cursor))

(defun serenade-select-region (min max) 
  (message "selecting region") 
  (if serenade-evil (serenade--select-region-evil min max) 
    (serenade--select-region min max)))

(defun serenade--select-region (min max) 
  (message "evil serenade--select-region") 
  (goto-char  min ) 
  (push-mark max) 
  (setq mark-active t))

(defun serenade--select-region-evil (min max) 
  (message "evil serenade--select-region")
  ;; (message (prin1-to-string min))
  ;; (message (prin1-to-string max))
  (evil-visual-make-region min (- max 1)))

(provide 'serenade-buffer)
