(in-package #:trivial-main-thread)

(defvar *main-runner-running-p* NIL)
(defvar *task-queue* (make-array 32 :fill-pointer 0))
(defvar *task-lock* (bt:make-lock "MAIN-THREAD-TASK-LOCK"))
(defvar *task-cvar* (bt:make-condition-variable :name "MAIN-THREAD-CONDITION"))
(defvar *on-error* :resignal)

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

(defun main-thread ()
  (or *main-thread*
      (setf *main-thread* (find-main-thread))))

(defun main-thread-p (&optional (thread (bt:current-thread)))
  (eq thread (main-thread)))

(defun swap-main-thread (new-function)
  (if (main-thread-p)
      (funcall new-function)
      (let ((new-main (or #+ccl
                          (ensure-housekeeping)
                          NIL)))
        (unless (bt:thread-alive-p (main-thread))
          (error "The main thread seems to have died."))
        (bt:interrupt-thread
         (main-thread)
         (or
          #+ccl (lambda ()
                  (ccl:%set-toplevel
                   (lambda ()
                     (ccl:%set-toplevel NIL)
                     (funcall new-function)))
                  (ccl:toplevel))
          new-function))
        new-main)))

(defun main-error-handler (e)
  (etypecase *on-error*
    ((member :ignore NIL))
    ((member :resignal)
     (if (find-restart 'resignal-in-caller)
         (invoke-restart 'resignal-in-caller e)
         (invoke-debugger e)))
    ((member :quit) (uiop:quit 1))
    ((or symbol function) (funcall *on-error* e))))

(defun main-runner ()
  (setf *main-runner-running-p* T)
  (with-simple-restart (stop-main-runner "Exit the main thread loop")
    (handler-bind ((error #'main-error-handler))
      (unwind-protect
           (loop (when (bt:with-lock-held (*task-lock*)
                         (bt:condition-wait *task-cvar* *task-lock* :timeout 0.5))
                   (let ((tasks (bt:with-lock-held (*task-lock*)
                                  (shiftf *task-queue* (make-array 32 :fill-pointer 0)))))
                     (dotimes (i (length tasks))
                       (setf (aref tasks i)
                             (multiple-value-list
                              (with-simple-restart (resignal-in-caller "Fail the given task and re-signal the condition in the caller thread")
                                (funcall (aref tasks i)))))))))
        (bt:with-lock-held (*task-lock*)
          (let ((tasks *task-queue*))
            (dotimes (i (length tasks))
              (setf (aref tasks i) ()))))
        (setf *main-runner-running-p* NIL)))))

(defun start-main-runner ()
  (when *main-runner-running-p*
    (error "Main runner seems to already be running!"))
  (swap-main-thread #'main-runner))

(defun stop-main-runner (&optional return)
  (cond ((main-thread-p)
         (invoke-restart 'stop-main-runner return))
        ((and *main-runner-running-p* (bt:thread-alive-p (main-thread)))
         (bt:interrupt-thread (main-thread) (lambda () (invoke-restart 'stop-main-runner return)))
         #+ccl (when (and *housekeeping* (bt:thread-alive-p *housekeeping*))
                 (bt:destroy-thread *housekeeping*)))
        (T
         (error "Main runner is not running!"))))

(defun ensure-main-runner ()
  (unless *main-runner-running-p*
    (start-main-runner)))

(defun vector-push-safely (el vector)
  (when (<= (array-total-size vector) (fill-pointer vector))
    (let ((new (make-array (* 2 (array-total-size vector)) :fill-pointer (fill-pointer vector))))
      (replace new vector)
      (setf vector new)))
  (vector-push el vector)
  (values vector (1- (length vector))))

(defun call-in-main-thread (function &key blocking)
  (cond ((main-thread-p)
         (funcall function))
        (T
         (ensure-main-runner)
         (multiple-value-bind (vector i)
             (bt:with-lock-held (*task-lock*)
               (multiple-value-bind (vector i) (vector-push-safely function *task-queue*)
                 (setf *task-queue* vector)
                 (values vector i)))
           (bt:condition-notify *task-cvar*)
           (when blocking
             (loop for result = (aref vector i)
                   do (typecase result
                        (condition (signal result))
                        (list (return (values-list result))))
                      (sleep 0.01)))))))

(defmacro with-body-in-main-thread ((&key blocking) &body body)
  `(call-in-main-thread (lambda () ,@body) :blocking ,blocking))
