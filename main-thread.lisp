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
#+ccl (defvar *housekeeping* NIL)

#+ccl (defun ensure-housekeeping ()
        (or *housekeeping*
            (setf *housekeeping*
                  (bt:make-thread #'ccl::housekeeping-loop :name "housekeeping"))))

(defun swap-main-thread (new-function &optional (main-thread *main-thread*))
  (let ((new-main (or #+ccl (ensure-housekeeping)
                      NIL)))
    (bt:interrupt-thread
     main-thread
     (or
      #+ccl (lambda ()
              (ccl:%set-toplevel
               (lambda ()
                 (ccl:%set-toplevel NIL)
                 (funcall new-function)))
              (ccl:toplevel))
      new-function))
    new-main))

(defun runner-starter (runner)
  (lambda ()
    (handler-bind ((error (lambda (err)
                            (declare (ignore err))
                            (invoke-restart 'simple-tasks:skip))))
      (simple-tasks:start-runner runner))))

(defun start-main-runner (&key (main-thread *main-thread*) (runner *runner*))
  (setf *runner* runner)
  (swap-main-thread (runner-starter runner) main-thread)
  *runner*)

(defun stop-main-runner (&key (main-thread *main-thread*) (runner *runner*))
  (unless (eql (simple-tasks:status runner) :running)
    (error "Main runner ~a already stopped!" runner))
  (bt:interrupt-thread main-thread (lambda () (invoke-restart 'simple-tasks:abort))))

(defun ensure-main-runner (&key (runner *runner*))
  (unless (eql (simple-tasks:status runner) :running)
    (start-main-runner)))

(defun ensure-main-runner-started (&key (runner *runner*))
  (ensure-main-runner :runner runner)
  (loop until (eql (simple-tasks:status runner) :running)
        do (sleep 0.01)))

(defun schedule-task (task &optional (runner *runner*))
  (ensure-main-runner-started :runner runner)
  (simple-tasks:schedule-task task runner))

(defun call-in-main-thread (function &key blocking (runner *runner*))
  (ensure-main-runner-started :runner runner)
  (simple-tasks:call-as-task
   function runner
   (if blocking
       'simple-tasks:blocking-call-task
       'simple-tasks:call-task)))

(defmacro with-body-in-main-thread ((&key blocking (runner '*runner*)) &body body)
  `(call-in-main-thread (lambda () ,@body) :blocking ,blocking :runner ,runner))
