(defvar serenade-evil nil 
  "If true, use evil commands where possible for default commands")

(defcustom serenade-mode-filetypes '(".js" ".py" ) 
  "The filetypes that can be used as serenade buffers")

(defun serenade--set-serenade-buffer ()
  ;; (debug)
  (if (and (buffer-file-name) 
           (file-name-extension (buffer-file-name))) 
      (let* ((ext (format ".%s" (file-name-extension (buffer-file-name))))) 
        (if (member ext serenade-mode-filetypes) 
            (setq serenade-buffer (current-buffer) ) 
          (setq serenade-buffer nil ))) 
    (setq serenade-buffer nil )))

(defun serenade--update-buffer (source cursor) 
  (delete-region (point-min) 
                 (point-max)) 
  (insert source) 
  (goto-char cursor))

(defun serenade--select-target (min max) 
  (if serenade-evil (progn (goto-char min) 
                           (evil-visual-state ) 
                           (goto-char max)) 
    (progn (goto-char  min ) 
           (push-mark max) 
           (setq mark-active t))))

(defun serenade--cut-selection () 
  (if serenade-evil (execute-kbd-macro (kbd "x" )) 
    (kill-region (region-beginning) 
                 (region-end)) 
    (setq mark-active nil)))

(defun serenade--copy-target (text) 
  (kill-new text))

(defun serenade--copy-selection () 
  (if serenade-evil (progn (execute-kbd-macro (kbd "y")) 
                           (evil-normal-state)) 
    (progn (kill-ring-save nil nil t ))))

(defun serenade--undo () 
  (if serenade-evil (evil-undo 1) 
    (undo)))

(defun serenade--paste () 
  (if serenade-evil (progn (evil-normal-state) 
                           (execute-kbd-macro (kbd "p"))) 
    (yank)))

(provide 'serenade-buffer)
