#|
This file is a part of trivial-main-thread
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trivial-main-thread
  (:nicknames #:org.shirakumo.trivial-main-thread #:tmt)
  (:use #:cl)
  (:export
   #:*runner*
   #:*main-thread*
   #:find-main-thread
   #:swap-main-thread
   #:start-main-runner
   #:stop-main-runner
   #:ensure-main-runner
   #:ensure-main-runner-started
   #:schedule-task
   #:call-in-main-thread
   #:with-body-in-main-thread))
