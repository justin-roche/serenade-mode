(require 'ht)
(require 'cl)
(require 's)
(require 'serenade-helm)
(require 'serenade-persistence)

(defvar serenade-mode-maps (ht("global" (ht)) ) 
  "hashtable of Serenade voice maps")

(defun serenade--initialize-mode-maps () 
  (setq serenade-mode-maps (ht("global" (ht)) ) ) 
  (serenade--add-default-bindings))

(defun serenade--clear-mode-maps () 
  (setq serenade-mode-maps (ht("global" (ht)) ) ))

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
      (serenade-define-speech 'global speech command)))

(defun serenade-define-speech (mode-name speech command) 
  (let* ((name (symbol-name mode-name)) 
         (voice-map (ht-get serenade-mode-maps name ))) 
    (if (string-equal name "global") 
        (ht-set (ht-get serenade-mode-maps "global") speech (ht("command" command))  ) 
      (if voice-map (ht-set voice-map speech (ht ("command" command))) 
        (progn (ht-set serenade-mode-maps name (ht)) 
               (ht-set (ht-get serenade-mode-maps name ) speech (ht ("command" command)))))) 
    (if serenade-helm-M-x (serenade--update-helm-map speech command))))

(defun serenade--find-voice-binding (speech) 
  (or (serenade--find-in-active-minor-maps speech) 
      (serenade--find-in-active-major-map speech) 
      (serenade--find-in-global-map speech)))

(defun serenade--find-in-active-minor-maps (speech) 
  (catch 'result 
    (mapc (lambda (mode-and-map) 
            (let* ((mode (symbol-name (car mode-and-map))) 
                   (voice-map (ht-get* serenade-mode-maps mode))) 
              (if voice-map (let* ((command (ht-get* voice-map speech))) 
                              (if command 
                                  (throw 'result command)))))) minor-mode-map-alist)
    nil))

(defun serenade--find-in-active-major-map (speech) 
  (ht-get* serenade-mode-maps (symbol-name(major-mode)) speech))

(defun serenade--find-in-global-map (speech) 
  (ht-get* serenade-mode-maps "global" speech))

(defun serenade--add-default-bindings () 
  (serenade-global-set-speech '(;; ( "press". nil  )
                                ( "close tab" . delete-window  ) 
                                ( "save"  . save-buffer  ) 
                                ( "create tab" . split-window-right-and-focus  ) 
                                ( "next tab" . next-buffer  ) 
                                ( "previous tab" . previous-buffer) 
                                ( "switch tab" . nil  ) 
                                ( "open file list" .
                                  spacemacs-layouts/non-restricted-buffer-list-helm) 
                                ( "open file" . nil  )
                                ;; ( "undo" .  undo  )
                                ( "redo" . redo  ) 
                                ( "scroll" . zz-scroll-half-page-down  ) 
                                ( "style" . nil) 
                                ( "go to definition" . spacemacs/jump-to-definition  ) 
                                ( "debugger toggle breakpoint" . nil) 
                                ( "debugger start" . nil  ) 
                                ( "debugger pause" . nil  ) 
                                ( "debugger stop" . nil  ) 
                                ( "debugger show hover" . nil) 
                                ( "debugger continue" . nil  ) 
                                ( "debugger step into" . nil  ) 
                                ( "debugger step over" . nil) 
                                ( "debugger inline breakpoint" . nil))))
(serenade--add-default-bindings)

(defun serenade-helm-commands () 
  (interactive) 
  (helm :sources (helm-build-sync-source "serenade" 
                   :candidates (serenade--get-helm-candidates serenade-mode-maps)) 
        :buffer "*helm serenade*"))

(provide 'serenade-custom-commands)
(serenade--initialize-mode-maps)

(message (ht-get* serenade-helm-map "save-buffer"))
(message (ht-get* serenade-helm-map "previous-buffer"))

