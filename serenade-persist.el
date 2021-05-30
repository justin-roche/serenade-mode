
(defcustom serenade-storage-directory user-emacs-directory 
  "location to store serenade data between sessions")

(defun serenade--save-data (file data) 
  (with-temp-file file (prin1 data (current-buffer))))

(defun serenade--read-data (file symbol) 
  (when (boundp symbol) 
    (with-temp-buffer (insert-file-contents file) 
                      (goto-char (point-min)) 
                      (set symbol (read (current-buffer))))))

;; (my-write "~/test.txt" emacs-version)

;; (my-read "~/test.txt" 'my-emacs-version)
