
(if (boundp 'spacemacs-version) 
    (progn (spacemacs/set-leader-keys (kbd "av") 'serenade-mode) 
           (spacemacs/set-leader-keys (kbd "al") 'serenade--log-open-log) 
           (spacemacs/set-leader-keys (kbd "ac") 'serenade--log-clear-log) 
           (setq serenade-evil t )))
(provide 'serenade-spacemacs)
