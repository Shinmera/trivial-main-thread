#|
 This file is a part of trivial-main-thread
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem trivial-main-thread
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Compatibility library to run things in the main thread."
  :homepage "https://Shinmera.github.io/trivial-main-thread/"
  :bug-tracker "https://github.com/Shinmera/trivial-main-thread/issues"
  :source-control (:git "https://github.com/Shinmera/trivial-main-thread.git")
  :serial T
  :components ((:file "package")
               (:file "main-thread")
               (:file "documentation"))
  :depends-on (:trivial-features
               :bordeaux-threads
               :simple-tasks))
