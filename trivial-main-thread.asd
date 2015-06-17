#|
This file is a part of trivial-main-thread
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(asdf:defsystem trivial-main-thread
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Compatibility library to run things in the main thread."
  :homepage "https://github.com/Shinmera/trivial-main-thread"
  :serial T
  :components ((:file "package")
               (:file "main-thread"))
  :depends-on (:trivial-features
               :bordeaux-threads))
