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

(cl-defstruct 
    serenade-mode-configuration
  mode
  get-cursor
  get-filename
  get-source
  post-edit
  pre-edit
  set-cursor
  set-source)

(cl-defun 
    serenade--configure-mode 
    (&optional 
     &keys
     mode
     get-cursor
     get-filename
     get-source
     post-edit
     pre-edit
     set-cursor
     set-source) 
  (let* ((config  (make-serenade-mode-configuration ;;
                   :mode (or mode 
                             nil) 
                   :get-cursor (or get-cursor 
                                   'serenade--get-cursor) 
                   :get-filename (or get-filename 
                                     'serenade--get-filename) 
                   :get-source (or get-source 
                                   'serenade--get-source) 
                   :post-edit (or post-edit 
                                  nil) 
                   :pre-edit (or pre-edit 
                                 nil) 
                   :set-cursor (or set-cursor 
                                   'serenade--set-cursor) 
                   :set-source (or set-source 
                                   'serenade--set-source)))) 
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
