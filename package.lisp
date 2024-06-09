(in-package #:cl-user)
(defpackage #:trivial-main-thread
  (:nicknames #:org.shirakumo.trivial-main-thread #:tmt)
  (:use #:cl)
  (:export
   #:*on-error*
   #:main-thread
   #:main-thread-p
   #:swap-main-thread
   #:start-main-runner
   #:stop-main-runner
   #:ensure-main-runner
   #:call-in-main-thread
   #:with-body-in-main-thread))
