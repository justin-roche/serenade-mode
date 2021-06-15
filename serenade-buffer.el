
(defcustom serenade-mode-filetypes 
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go"
    "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh"
    "bash" "ts" "tsx" "tsx" "ts""vue" "html")
  "The filetypes that can be used as serenade buffers")

(defun serenade--set-serenade-buffer () 
  (if (and (buffer-file-name) 
           (file-name-extension (buffer-file-name))) 
      (let* ((ext (file-name-extension (buffer-file-name)))) 
        (if (member ext serenade-mode-filetypes) 
            (setq serenade-buffer (current-buffer) ) 
          (setq serenade-buffer nil ))) 
    (setq serenade-buffer nil )))

(defun serenade--update-buffer (source cursor) 
  (delete-region (point-min) 
                 (point-max)) 
  (insert source) 
  (goto-char cursor))

(provide 'serenade-buffer)
