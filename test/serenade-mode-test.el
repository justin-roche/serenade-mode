
(require 'serenade-mode)
(require 'serenade-socket)

(describe "Connection start"  (before-each (setf (symbol-function 'serenade--connect) 
                                                 (lambda ())) 
                                           (spy-on 'serenade--connect)) 
          (it "calls start function" (serenade-mode) 
              (expect 'serenade--connect 
                      :to-have-been-called) ))

(describe "Connection stop"  (before-each (setf (symbol-function 'serenade--disconnect) 
                                                (lambda ())) 
                                          (spy-on 'serenade--disconnect)) 
          (it "calls disconnect function" (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade--disconnect 
                      :to-have-been-called) ))

(describe "Mode initialization"  (before-each (setf (symbol-function 'serenade-mode--start) 
                                                    (lambda (value) 
                                                      (setq bar value))) 
                                              (setf (symbol-function 'serenade-mode--stop) 
                                                    (lambda (value) 
                                                      (setq bar value))) 
                                              (setf (symbol-function 'serenade-connect) 
                                                    (lambda (value) 
                                                      (setq bar value))) 
                                              (spy-on 'serenade-connect) 
                                              (spy-on 'serenade-mode--start) 
                                              (spy-on 'serenade-mode--stop)) 
          (it "calls start function" (serenade-mode) 
              (expect 'serenade-mode--start 
                      :to-have-been-called)) 
          (it "calls stop function" (setf (symbol-value 'serenade-mode) nil) 
              (serenade-mode-toggle) 
              (expect 'serenade-mode--stop 
                      :to-have-been-called)))
