#|
This file is a part of trivial-main-thread
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:trivial-main-thread)

(defvar *runner* (make-instance 'simple-tasks:queued-runner))

(defun find-main-thread ()
  (or
   #+sbcl (find "main thread" (bt:all-threads) :from-end T :key #'sb-thread:thread-name)
   #+ecl (find 'si:top-level (bt:all-threads) :from-end T :key #'mp:process-name)
   #+ccl ccl::*initial-process*
   (progn (warn "Couldn't find main thread reliably, choosing last thread.")
          (car (last (bt:all-threads))))))

(defvar *main-thread* (find-main-thread))

(defun swap-main-thread (new-function &optional (main-thread *main-thread*))
  (let ((new-main #+ccl (bt:make-thread #'ccl::housekeeping-loop :name "housekeeping")
                  #-(or ccl) NIL))
    (bt:interrupt-thread
     main-thread
     #+ccl (lambda ()
             (ccl:%set-toplevel
              (lambda ()
                (ccl:%set-toplevel NIL)
                (funcall new-function))))
     #-(or ccl) new-function)
    new-main))

(defun start-main-runner (&key (main-thread *main-thread*) (runner *runner*))
  (setf *runner* runner)
  (swap-main-thread (lambda () (simple-tasks:start-runner runner)) main-thread)
  *runner*)

(defun ensure-main-runner ()
  (unless (eql (simple-tasks:status *runner*) :running)
    (start-main-runner)))

(defun schedule-task (task)
  (ensure-main-runner)
  (simple-tasks:schedule-task task *runner*))

(defun call-in-main-thread (function &key blocking)
  (simple-tasks:call-as-task
   function *runner*
   (if blocking
       'simple-tasks:blocking-call-task
       'simple-tasks:call-task)))

(defmacro with-body-in-main-thread ((&key blocking) &body body)
  `(call-in-main-thread (lambda () ,@body) :blocking ,blocking))
