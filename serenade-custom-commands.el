(require 'ht)
(require 'cl)
(require 's)
(require 'serenade-helm)
;;
(defun serenade-initialize-mode-maps () 
  (setq serenade-mode-maps (ht("global" (ht)) ) ))

(serenade-initialize-mode-maps)

(defun serenade--get-global-map () 
  (ht-get serenade-mode-maps "global"))

(cl-defun 
    serenade-global-set-speech
    (speech &optional command ) 
  (if (and (listp speech)) 
      (dolist (item speech ) 
        (serenade-global-set-speech (car item) 
                                    (cdr item)))) 
  (if (and (symbolp speech) 
           (not command)) 
      (let* ((split-command  (s-replace  "-" " " (symbol-name speech)))) 
        (serenade-global-set-speech split-command speech))) 
  (if (and (stringp speech) 
           (symbolp command)) 
      (progn (ht-set (ht-get serenade-mode-maps "global") speech (ht("command" command))  ))) 
  (if serenade-helm-M-x (serenade--update-helm-map speech command)))

(defun serenade-define-speech (mode-name speech command)
  ;; https://emacs.stackexchange.com/questions/9930/what-does-symbol-value-do
  (let* ((name (symbol-name mode-name)) 
         (voice-map (ht-get serenade-mode-maps name ))) 
    (if voice-map (ht-set voice-map speech (ht ("command" command))) 
      (progn (ht-set serenade-mode-maps name (ht)) 
             (ht-set (ht-get serenade-mode-maps name ) speech (ht ("command" command)))))))

;; (serenade-define-speech 'rjsx-mode-voice-map "c" 'd)
;; (serenade-define-speech 'rjsx-mode-voice-map "e" 'f)
(defun serenade--find-voice-binding (speech) 
  (or (serenade--find-in-active-minor-maps) 
      (serenade--find-in-global-map)))

(defun serenade--find-in-active-minor-maps () 
  (catch 'bbb 
    (mapc (lambda (x) 
            (let* ((mode (symbol-name (car x))) 
                   (voice-map (ht-get* serenade-mode-maps mode))) 
              (if voice-map (let* ((command (ht-get* voice-map speech))) 
                              (if command 
                                  (throw 'bbb command))))
              ;; (debug)
              )) minor-mode-map-alist)
    nil))

(defun serenade--find-in-global-map (speech) 
  (ht-get* serenade-mode-maps "global" speech))

(defun serenade--find-active-major-map () 
  (format "%s-voice-map" major-mode))
;; (put 'voice-map (intern-soft (symbol-name mode-map ))   5))
(provide 'serenade-custom-commands)

;; (serenade-define-speech 'edebug-mode "a" 'b)
;; (message  (serenade--find-voice-binding "a"))

;; (serenade-global-set-speech "treemacs rename" 'treemacs-rename)
;; (serenade-global-set-speech 'uncomment-region)
;; (serenade-global-set-speech '( "treemacs rename" 'treemacs-rename ))
;; (serenade-global-set-speech '( ("treemacs rename" . treemacs-rename) ))
;; current-active-maps
;; Function: Return a list of the currently active keymaps.
;; current-global-map
;; Function: Return the current global keymap.
;; current-local-map
;; Function: Return current buffer's local keymap, or nil if it has
;; none.
;; current-minor-mode-maps
;; Function: Return a list of keymaps for the minor modes of the
;; current buffer.
;; (serenade-define-speech 'org-mode-map "promote" 'org-do-promote)
;; (progn (serenade-define-speech 'org-mode-map "a" 'b)
;; (serenade--find-voice-binding "a")
;; (or (if overriding-terminal-local-map
;;         (find-in overriding-terminal-local-map))
;;     (if overriding-local-map
;;         (find-in overriding-local-map)
;;       (or (find-in (get-char-property (point) 'keymap))
;;           (find-in-any emulation-mode-map-alists)
;;           (find-in-any minor-mode-overriding-map-alist)
;;           (find-in-any minor-mode-map-alist)
;;           (if (get-text-property (point) 'local-map)
;;               (find-in (get-char-property (point) 'local-map))
;;             (find-in (current-local-map)))))
;;     (find-in (current-global-map)))
