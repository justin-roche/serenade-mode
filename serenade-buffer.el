(defvar serenade-evil nil 
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
  (delete-region (point-min) 
                 (point-max)) 
  (insert source) 
  (goto-char cursor))

(defun serenade--select-region (min max) 
  (message "selecting region") 
  (if serenade-evil (evil-visual-make-region min (- max 1)) 
    (progn (goto-char  min ) 
           (push-mark max) 
           (setq mark-active t))))

(defun serenade--cut () 
  (if serenade-evil (execute-kbd-macro (kbd "x" )) 
    (kill-region (region-beginning) 
                 (region-end))))

(defun serenade--copy (text) 
  (kill-new text))

(defun serenade--undo () 
  (if serenade-evil (evil-undo 1) 
    (undo)))

(defun serenade--paste () 
  (if serenade-evil (evil-paste-after) 
    (yank)))

(provide 'serenade-buffer)

(global-set-key (kbd "s-v" ) nil)
(global-set-key (kbd "s-c" ) nil)
(global-set-key (kbd "s-x" ) nil)
