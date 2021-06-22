
(defcustom serenade-mode-filetypes 
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go"
    "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh"
    "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
  "The filetypes that can be used as serenade buffers, which are buffers subject to the diff operation.")

(defun serenade--set-serenade-buffer ()
  ;; Determines if the current buffers file extension is a valid member of SERENADE-MODE-FILE-TYPES. If it is set SERENADE-BUFFER to the current buffer, otherwise set it to nil.
  (if (and (buffer-file-name) 
           (file-name-extension (buffer-file-name))) 
      (let* ((ext (file-name-extension (buffer-file-name)))) 
        (if (member ext serenade-mode-filetypes) 
            (setq serenade-buffer (current-buffer) ) 
          (setq serenade-buffer nil ))) 
    (setq serenade-buffer nil )))

(defun serenade--update-buffer (source cursor)
  ;; This function replaces the current buffer contents and cursor with the provided SOURCE and CURSOR position from the diff command.
  (if (not serenade-evil) 
      (undo-boundary)) 
  (setq serenade--undo-position (point)) 
  (if serenade-evil (evil-insert-state)) 
  (delete-region (point-min) 
                 (point-max)) 
  (if source (insert source)) 
  (if serenade-evil (evil-normal-state)) 
  (goto-char cursor))

(defun serenade--after-edit () 
  (if (or(eq major-mode 'rjsx-mode)(eq major-mode 'js2-mode)) 
      (js2-reparse)))

(provide 'serenade-buffer)
