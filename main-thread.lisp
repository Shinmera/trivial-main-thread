#|
This file is a part of trivial-main-thread
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:trivial-main-thread)

(defvar *main-thread* (find-main-thread))

(defun find-main-thread ()
  (or
   #+sbcl (find "main thread" (bt:all-threads) :from-end T :key #'sb-thread:thread-name)
   #+ecl (find 'si:top-level (bt:all-threads) :from-end T :key #'mp:process-name)
   #+ccl ccl::*initial-process*
   (progn (warn "Couldn't find main thread reliably, choosing last thread.")
          (car (last (bt:all-threads))))))

(defun eval-in-main-thread (function &optional (main-thread *main-thread*))
  (bt:interrupt-thread main-thread function)
  main-thread)

(defmacro do-in-main-thread ((&optional (main-thread '*main-thread*)) &body body)
  `(eval-in-main-thread (lambda () ,@body) ,main-thread))

(defun swap-main-thread (new-function &optional (main-thread *main-thread*))
  (let ((new-main #+ccl (bt:make-thread #'ccl::housekeeping-loop :name "housekeeping")
                  #-(or ccl) NIL))
    (eval-in-main-thread
     #+ccl (lambda ()
             (ccl:%set-toplevel
              (lambda ()
                (ccl:%set-toplevel NIL)
                (funcall new-function))))
     #-(or ccl) new-function
     main-thread)
    new-main))
