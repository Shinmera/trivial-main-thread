(asdf:defsystem trivial-main-thread
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
