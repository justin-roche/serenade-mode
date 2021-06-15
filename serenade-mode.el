
;;; serenade-mode.el --- serenade mode -*- lexical-binding: t -*-

;; Author: Justin Roche
;; Maintainer: Justin Roche
;; Version: 0.0.1
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

(require 'serenade-socket)
(require 'serenade-commands)
(require 'serenade-log)
(require 'serenade-lines)
(require 'serenade-helm)
(require 'serenade-keys-patch)
(require 'serenade-spacemacs)
(require 'serenade-synchronize)
(require 'serenade-editor-functions)

(defface helm-serenade-command '((t :foreground "plum3" 
                                    :underline t)) 
  "Face for serenade helm.")
(defcustom serenade-mode-init-hook nil 
  "The list of functions to be called after `serenade-mode' has initialized all variables, before connecting fer for the first time." 
  :type 'hook 
  :group 'serenade-mode)

(defun serenade-mode--start () 
  (run-hooks 'serenade-mode-init-hook) 
  (serenade--info "connecting to serenade") 
  (if (eq evil-mode t) 
      (setq serenade-evil t )) 
  (serenade--info (concat "evil mode" (prin1-to-string serenade-evil))) 
  (if serenade-enable-double-line-numbers (serenade--double-line-numbers-on)) 
  (serenade--initialize-mode-maps) 
  (if serenade-sync-on-start (serenade-synchronize)) 
  (if serenade-helm-M-x (serenade--advise-helm-transformer)) 
  (serenade--connect))

(defun serenade-mode-start () 
  (interactive) 
  (serenade-mode--start))

(defun serenade-mode--stop () 
  (serenade--info "disconnecting from serenade") 
  (if serenade-enable-double-line-numbers (serenade--double-line-numbers-off)) 
  (if serenade-helm-M-x (serenade--unadvise-helm-transformer)) 
  (serenade--disconnect))

(defun serenade-mode-stop () 
  (interactive) 
  (serenade-mode--stop))

(defun serenade-mode-toggle () 
  (if serenade-mode (serenade-mode--start) 
    (serenade-mode--stop)))

(defvar serenade-mode-map (let ((map (make-sparse-keymap))) map))

(define-minor-mode serenade-mode "Toggle Serenade mode." 
  nil
  " Serenade" 
  :global t 
  :lighter " serenade" 
  :keymap serenade-mode-map 
  :group 'serenade-mode
  (serenade-mode-toggle))

(provide 'serenade-mode)

;;; serenade-mode.el ends here
