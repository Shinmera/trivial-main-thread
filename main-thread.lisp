#|
This file is a part of trivial-main-thread
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:trivial-main-thread)

(defvar *runner* (make-instance 'simple-tasks:queued-runner))

(defun find-main-thread ()
  (or
   #+sbcl (sb-thread:main-thread)
   #+ecl (find 'si:top-level (bt:all-threads) :from-end T :key #'mp:process-name)
   #+clasp (find 'si:top-level (bt:all-threads) :from-end T :key #'mp:process-name)
   #+ccl ccl::*initial-process*
   ;; https://github.com/rtoy/cmucl/blob/master/src/code/multi-proc.lisp#L1530 suggests this should be reliable
   #+cmucl (find "Initial" mp:*all-processes* :test #'equal :key #'mp:process-name)
   #+allegro (find "Initial Lisp Listener" mp:*all-processes* :test #'equal :key #'mp:process-name)
   #+mkcl (find "Initial" (mt:all-threads) :test #'equal :key #'mt:thread-name)
   #+lispworks mp:*main-process*
   (progn (warn "Couldn't find main thread reliably, choosing last thread.")
          (car (last (bt:all-threads))))))

(defvar *main-thread* NIL)
#+ccl (defvar *housekeeping* NIL)

#+ccl (defun ensure-housekeeping ()
        (or (when (and *housekeeping* (bt:thread-alive-p *housekeeping*))
              *housekeeping*)
            (setf *housekeeping*
                  (bt:make-thread #'ccl::housekeeping-loop :name "housekeeping"))))

(defmacro %ensure-mt (var)
  `(setf ,var (or ,var *main-thread* (find-main-thread))))

(defun swap-main-thread (new-function &optional main-thread)
  (%ensure-mt main-thread)
  (if (eql (bt:current-thread) main-thread)
      (funcall new-function)
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
        new-main)))

(defun runner-starter (runner)
  (lambda ()
    (handler-bind ((error (lambda (err)
                            (declare (ignore err))
                            (when (find-restart 'simple-tasks:skip)
                              (invoke-restart 'simple-tasks:skip)))))
      (simple-tasks:start-runner runner))))

(defun start-main-runner (&key main-thread (runner *runner*))
  (%ensure-mt main-thread)
  (setf *runner* runner)
  (setf *main-thread* main-thread)
  (swap-main-thread (runner-starter runner) main-thread)
  runner)

(defun stop-main-runner (&key main-thread (runner *runner*))
  (%ensure-mt main-thread)
  (unless (eql (simple-tasks:status runner) :running)
    (error "Main runner ~a already stopped!" runner))
  (unless (eql (bt:current-thread) main-thread)
    (bt:interrupt-thread main-thread (lambda () (invoke-restart 'simple-tasks:abort)))
    #+ccl (when (and *housekeeping* (bt:thread-alive-p *housekeeping*))
            (bt:destroy-thread *housekeeping*)))
  runner)

(defun ensure-main-runner (&key main-thread (runner *runner*))
  (%ensure-mt main-thread)
  (unless (eql (simple-tasks:status runner) :running)
    (when (eql (bt:current-thread) main-thread)
      (change-class runner 'simple-tasks:runner))
    (start-main-runner :main-thread main-thread :runner runner)))

(defun ensure-main-runner-started (&key main-thread (runner *runner*))
  (ensure-main-runner :main-thread main-thread :runner runner)
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
