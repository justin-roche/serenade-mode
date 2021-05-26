(defun serenade-update-buffer (source cursor)
  ;; (delete-region (point-min)
  ;;                (point-max))
  (message source)
  ;; (insert source)
  ;; (goto-char cursor)
  )

(defun serenade-select-region (min max) 
  (goto-char  min ) 
  (push-mark max) 
  (setq mark-active t))

(defun serenade-select-region-evil (min max) 
  (evil-visual-make-region min (- max 1)))

(provide 'serenade-buffer)
