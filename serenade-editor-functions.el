
(defvar serenade-evil nil 
  "If true, use evil commands where possible for default commands")

(defun serenade--select-target (min max) 
  (if serenade-evil (progn (goto-char min) 
                           (evil-visual-state ) 
                           (goto-char max)) 
    (progn (goto-char  min ) 
           (push-mark (+ 1 max)) 
           (setq mark-active t))))

(defun serenade--cut-selection () 
  (if serenade-evil (execute-kbd-macro (kbd "x" )) 
    (kill-region (region-beginning) 
                 (region-end)) 
    (setq mark-active nil)))

(defun serenade--copy-selection () 
  (if serenade-evil (progn (execute-kbd-macro (kbd "y")) 
                           (evil-normal-state)) 
    (progn (kill-ring-save nil nil t ))))

(defun serenade--undo () 
  (if serenade-evil (evil-undo 1) 
    (undo)) 
  (goto-char serenade--undo-position))

(defun serenade--redo () 
  (undo-tree-redo))

(defun serenade--switch-tab (index) 
  (winum-select-window-by-number index))

(defun serenade--get-buffer-by-regex (fragment)
  ;; search for buffer by name and switch to it
  (let* ((matching-buffers (-filter (lambda (elt) 
                                      (s-contains? fragment (buffer-name elt) t)) 
                                    (buffer-list)))) matching-buffers))

(defun serenade--open-file (fragment) 
  (let* ((b (serenade--get-buffer-by-regex fragment)))
    ;; (split-window-right-and-focus)
    (switch-to-buffer (first b))))

(provide 'serenade-editor-functions)
