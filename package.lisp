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
   #:*main-thread*
   #:find-main-thread
   #:eval-in-main-thread
   #:do-in-main-thread
   #:swap-main-thread))
