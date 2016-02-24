;; -*- lexical-binding: t; -*-
(require 'cl-lib)
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

;; (defun auto-pause-make-pause-and-resume-functions (pause-fn resume-fn)
;;   (let (pause-function resume-function)
;;     (setq pause-function (lambda ()
;;                            (apply pause-fn)
;;                            (add-hook 'post-command-hook resume-function)))
;;     (setq resume-function (lambda (delay-seconds)
;;                             (apply resume-fn)
;;                             (remove-hook 'post-command-hook resume-function)
;;                             (run-with-idle-timer delay-seconds nil pause-function)))
;;     (list pause-function resume-function)))

(defmacro auto-pause (pause-fn resume-fn delay-seconds)
  (let (;; (pause-fn (function pause-fn))
        ;; (resume-fn (function resume-fn))
        (pause-function-name (cl-gensym "auto-pause-pause-"))
        (resume-function-name (cl-gensym "auto-pause-resume-"))
        (abort-function-name (cl-gensym "auto-pause-abort-"))
        (idle-timer (cl-gensym "auto-pause-idle-timer-"))
        (delay-seconds delay-seconds))
    `(progn
       (defun ,pause-function-name ()
         (funcall ,pause-fn )
         (add-hook 'post-command-hook #',resume-function-name))
       (defun ,resume-function-name ()
         (funcall ,resume-fn )
         (remove-hook 'post-command-hook #',resume-function-name)) 
       (setq ,idle-timer (run-with-idle-timer ,delay-seconds t #',pause-function-name))
       (defun ,abort-function-name ()
         (cancel-timer ,idle-timer)
         (unintern ',idle-timer nil)
         (unintern ',pause-function-name nil)
         (unintern ',resume-function-name nil)
         (unintern ',abort-function-name nil)))))
