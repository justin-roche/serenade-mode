(require 'ht)

(defcustom serenade-mode-filetypes 
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go"
    "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh"
    "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
  "The filetypes that can be used as serenade buffers, which are buffers subject to the diff operation.")

(defvar serenade-mode-configurations (ht ))
(setq serenade-active-mode-configuration nil )

(defun serenade--set-active-mode-configuration ()
  ;; Set the active mode configuration based on the major-mode. If none is found, use the global default..
  (let* ((mode-name (symbol-name major-mode )) 
         (active-config  (ht-get* serenade-mode-configurations mode-name))) 
    (setq serenade-active-mode-configuration (or active-config 
                                                 (ht-get* serenade-mode-configurations "global")))))

(defun serenade--set-serenade-buffer ()
  ;; Determines if the current buffers file extension is a valid member of SERENADE-MODE-FILE-TYPES. If it is set SERENADE-BUFFER to the current buffer, otherwise set it to nil.
  (if (and (buffer-file-name) 
           (file-name-extension (buffer-file-name))) 
      (let* ((ext (file-name-extension (buffer-file-name)))) 
        (if (member ext serenade-mode-filetypes) 
            (setq serenade-buffer (current-buffer) ) 
          (setq serenade-buffer nil ))) 
    (setq serenade-buffer nil )))

(cl-defstruct 
    serenade-mode-configuration
  mode
  get-editor-state
  diff
  post-edit
  pre-edit)

(cl-defun 
    serenade--configure-mode 
    (&optional 
     &keys
     mode
     get-editor-state
     diff
     post-edit
     pre-edit) 
  (let* ((config  (make-serenade-mode-configuration ;;
                   :mode (or mode 
                             nil) 
                   :get-editor-state (or get-editor-state 
                                         'serenade--get-editor-state) 
                   :diff (or diff 
                             'serenade--diff) 
                   :post-edit (or post-edit 
                                  nil) 
                   :pre-edit (or pre-edit 
                                 nil))))
    (ht-set serenade-mode-configurations (symbol-name mode) config)))

(serenade--configure-mode :mode 'global)

;; (serenade--configure-mode :mode 'shell-mode
;;                           :set-source 'serenade-shell/set-source
;;                           :get-source 'serenade--shell/get-source)

(defun serenade--after-edit ()
  ;; This function is called after a speech command is handled
  (if (or(eq major-mode 
             'rjsx-mode) 
         (eq major-mode 'js2-mode)) 
      (js2-reparse)) 
  (run-hooks 'serenade-mode-after-edit-hook))

(provide 'serenade-buffer)

;; abnormal mode
;; get editor state strategy
;; fake filetype, partial contents
;; diff strategy: update special, cursor only
(defun serenade-shell/set-source 
    (&optional 
     source) 
  (let ((proc (get-buffer-process ( current-buffer )))) 
    (goto-char (process-mark proc)) 
    (kill-whole-line) 
    (insert (or contents 
                "<contents>"))))

(defun serenade--shell/get-source (name) 
  (let ((proc (get-buffer-process ( current-buffer )))) 
    (goto-char (process-mark proc)) 
    (thing-at-point 'line t)))

;; (setq serenade--default-mode-configuration (make-serenade-mode-configuration ))

(serenade--configure-mode :mode 'global )
