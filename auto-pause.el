;; -*- lexical-binding: t; -*-
(defun auto-pause--pause-process (proc)
  ""
  (when (processp proc) 
    (signal-process proc 'SIGSTOP)))

(defun auto-pause--resume-process (proc)
  ""
  (when (processp proc)
    (signal-process proc 'SIGCONT)))

(defun auto-pause-pause-process (proc)
  ""
  (auto-pause--pause-process proc)
  (add-hook 'post-command-hook (lambda ()
                                 (auto-pause-resume-process proc))))

(defun auto-pause-resume-process (proc delay-seconds)
  ""
  (auto-pause--resume-process proc)
  (remove-hook 'post-command-hook (lambda ()
                                    (auto-pause-resume-process proc)))
  (run-with-idle-timer delay-seconds nil (lambda ()
                                           (auto-pause-pause-process proc))))

(defun auto-pause-mark-process (proc delay-seconds)
  (run-with-idle-timer delay-seconds nil (lambda ()
                                           (auto-pause-pause-process proc)))
  proc)

(defmacro with-auto-pause (delay-seconds &rest body)
  (declare (debug t) (indent 1))
  `(let ((advise-name  "start-process-auto-pause-advise"))
     (advice-add start-process
                 :filter-return
                 (lambda (proc)
                   (auto-pause-mark-process proc delay-seconds))
                 :name advise-name)
     (unwind-protect (progn ,@body)
       (advice-remove start-process advise-name))))
