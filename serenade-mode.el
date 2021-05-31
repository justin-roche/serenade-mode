
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
(require 'serenade-custom-commands)

(defcustom serenade-mode-init-hook nil 
  "List of functions to be called after `serenade-mode'
has initialized all variables, before connecting
fer for the first time." 
  :type 'hook 
  :group 'serenade-mode)

(defun serenade-mode--start () 
  (message "connecting to serenade") 
  (run-hooks 'serenade-mode-init-hook) 
  (if serenade-enable-double-line-numbers (serenade-double-line-numbers-on)) 
  (serenade-initialize-mode-maps) 
  (serenade-connect))

(defun serenade-mode-start () 
  (interactive) 
  (serenade-mode--start))

(defun serenade-mode--stop () 
  (message "disconnecting from serenade")
  ;; (serenade-toggle-double-line-numbers-off)
  ;; (serenade--heartbeat-stop)
  ;; (if (not ( equal serenade--websocket nil ))
  ;;     (progn (print "disconnecting from serenade")
  ;;            (serenade--close-socket)))
  )

(defun serenade-mode-stop () 
  (interactive)
  ;; (serenade-toggle-double-line-numbers-off)
  ;; (serenade--heartbeat-stop)
  (serenade-mode--stop))

(defun serenade-mode-toggle () 
  (if (bound-and-true-p serenade-mode) 
      (serenade-mode--stop) 
    (serenade-mode--start)))

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

;; (serenade-mode-toggle)

;;; serenade-mode.el ends here
