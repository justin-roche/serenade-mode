;; (setq serenade-saved-data nil )

;; (defcustom serenade-storage-directory user-emacs-directory 
;;   "location to store serenade data between sessions")

;; (setq serenade-storage-directory "/Users/justinroche/emacs-projects/")

;; (defun serenade--save-data ( data) 
;;   (with-temp-file (concat serenade-storage-directory "/serenade-commands.el" ) 
;;     (prin1 data (current-buffer))))

;; (defun serenade--read-data () 
;;   (with-temp-buffer  (insert-file-contents (concat serenade-storage-directory
;;                                                    "/serenade-commands.el"))
;;                      (goto-char (point-min))
;;                      ;; (print (current-buffer))
;;                      ;; (set 'serenade-saved-data (read (current-buffer)))
;;                      ;; (debug)
;;                      (read (current-buffer))))

;; (provide 'serenade-persistence)

;; ;; (serenade--save-data (ht ("global" (ht ("a" (ht))))))
;; (let* ((x (serenade--read-data)))
;;   ;; (debug)
;;   )
