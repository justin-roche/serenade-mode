
(defvar serenade-evil t 
  "If true, use evil commands where possible for default commands")

(defun serenade--get-source () 
  (buffer-substring-no-properties 
   (point-min) 
   (point-max)))

(defun serenade--set-source (source)
  ;; This function replaces the current buffer contents and cursor with the provided SOURCE and CURSOR position from the diff command.
  (let ((tmp-buf (generate-new-buffer " *serenade-temp*"))) 
    (with-current-buffer tmp-buf (insert source)) 
    (replace-buffer-contents tmp-buf) 
    (kill-buffer tmp-buf)))

(defun serenade--get-filename () 
  (if (buffer-file-name) 
      (-last-item (s-split "/" (buffer-file-name))) ""))

(defun serenade--get-cursor () 
  (- (point) 1))

(defun serenade--set-cursor (loc) 
  (goto-char loc))

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
    (undo)))

(defun serenade--redo ()
  ;;TODO: does not return cursor position precisely
  (if serenade-evil (evil-redo 1) 
    (redo)))

(defun serenade--switch-tab (index) 
  (winum-select-window-by-number index))

(defun serenade--get-buffer-by-regex (fragment)
  ;; search for buffer by name and switch to it
  (let* ((matching-buffers (-filter (lambda (elt) 
                                      (s-contains? fragment (buffer-name elt) t)) 
                                    (buffer-list)))) matching-buffers))

(defun serenade--open-file (fragment) 
  (let* ((b (serenade--get-buffer-by-regex fragment))) 
    (switch-to-buffer (first b))))

(defun serenade--set-source (source)
  ;; This function replaces the current buffer contents and cursor with the provided SOURCE and CURSOR position from the diff command.
  (let ((tmp-buf (generate-new-buffer " *serenade-temp*"))) 
    (with-current-buffer tmp-buf (insert source)) 
    (replace-buffer-contents tmp-buf) 
    (kill-buffer tmp-buf)))

;; (serenade--configure-mode :post-edit '(lambda ()
;;                                         ;;
;;                                         (message "wowo"))
;;                           :pre-edit

;;                           :get-source '(lambda ()
;;                                           ;;
;;                                           )

;;                           :get-cursor
;;                           :update-source
;;                           :update-cursor )

(provide 'serenade-editor-functions)

;; (defun serenade--jump-to-nearest-match (str)
;;   (interactive)
;;   (let* ((matchposforward(or (re-search-forward "a" nil t)
;;                              (point-max)))
;;          (matchposback(result-search-backward "a" nil t))
;;          (distance-forward (- matchposforward (point)))
;;          (distance-back (- (point) matchposback)))
;;     (debug)
;;     (if (and (not (eq 0 distance-back))
;;              (not (eq 0 distance-forward)))
;;         (goto-char (if (< distance-forward distance-back) matchposforward matchposback))
;;       (if (and (not (eq 0 distance-back)))
;;           (goto-char matchposback)
;;         (if (and (not (eq 0 distance-forward)))
;;             (goto-char matchposforward))))
                                        ;(defun jr/ser-t ()
;; (interactive)
;; [[(if (and (not (eq 0 distance-back)))
;;       (goto-char (distance-back)))]]
;; (serenade--jump-to-nearest-match "a"));     ))
