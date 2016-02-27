;;; auto-pause.el --- library for creating auto-pause process which will be paused when Emacs idle for specific time and be resumed when emacs become busy again  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-02-23
;; Version: 0.1
;; Keywords: convenience, menu
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/lujun9972/auto-pause

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; auto-pause's code can be found here:
;;   http://github.com/lujun9972/auto-pause

;;; Commentary:
 
;; auto-pause is a llibrary for creating auto-pause process which will be paused when Emacs idle for specific time and be resumed when emacs become busy again 

;; Quick start:

;; see the doc-string of `with-auto-pause'

;;; Code:
(require 'cl-lib)

(defmacro auto-pause (pause-fn resume-fn delay-seconds)
  (declare (debug t) (indent defun))
  (let ((pause-function-name (cl-gensym "auto-pause-pause-"))
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

(defun auto-pause-pause-process (proc)
  "Pause PROC by send SIGSTOP signal. PROC should be subprocess of emacs"
  (when (processp proc) 
    (signal-process proc 'SIGSTOP)))

(defun auto-pause-resume-process (proc)
  "Resume PROC by send SIGCONT signal. PROC should be subprocess of emacs"
  (when (processp proc)
    (signal-process proc 'SIGCONT)))

(defun auto-pause-process-p (proc)
  "Return t if PROC is a auto-pause process"
  (and (processp proc)
       (process-get proc 'auto-pause-abort-function)))

(defun auto-pause--set-process-sentinel (proc sentinel)
  "Give PROC the sentinel SENTINEL, PROC should be an auto-pause process"
  (when (and (auto-pause-process-p proc)
             (ad-is-active 'set-process-sentinel))
    (set-process-sentinel proc (lambda (proc event)
                                 (funcall sentinel proc event)
                                 (when (eq 'exit (process-status proc))
                                   (funcall (process-get proc 'auto-pause-abort-function)))))))

(defun auto-pause--reset-process-sentinel (proc)
  "Reset the sentinel function of PROC which should be an auto-pause function"
  (auto-pause--set-process-sentinel proc (process-sentinel proc)))

(defun auto-pause-mark-process (proc delay-seconds)
  "Pause the PROC the next time Emacs is idle for DELAY-SECONDS, and resume the PROC when emacs become busy again"
  (process-put proc 'auto-pause-abort-function
               (auto-pause (lambda ()
                             (auto-pause-pause-process proc))
                 (lambda ()
                   (auto-pause-resume-process proc))
                 delay-seconds))
  (auto-pause--reset-process-sentinel proc)
  proc)

(defmacro with-auto-pause (delay-seconds &rest body)
  "Evalute BODY, if BODY created an asynchronous subprocess, it will be an auto-pause-process"
  (declare (debug t) (indent 1))
  `(progn
     (advice-add 'start-process
                 :filter-return
                 (lambda (proc)
                   (auto-pause-mark-process proc ,delay-seconds)))
     (advice-add 'set-process-sentinel
                 :after
                 (lambda (proc sentinel)
                   (auto-pause--reset-process-sentinel proc)))
     (unwind-protect (progn ,@body)
       (advice-remove 'start-process (lambda (proc)
                                       (auto-pause-mark-process proc ,delay-seconds)))
       (advice-remove 'set-process-sentinel (lambda (proc sentinel)
                                              (auto-pause--reset-process-sentinel proc))))))

(provide 'auto-pause)

;;; auto-pause.el ends here
