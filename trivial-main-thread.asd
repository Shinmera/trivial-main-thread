(asdf:defsystem trivial-main-thread
  :version "2.0.1"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Compatibility library to run things in the main thread."
  :homepage "https://shinmera.com/docs/trivial-main-thread/"
  :bug-tracker "https://shinmera.com/project/trivial-main-thread/issues"
  :source-control (:git "https://shinmera.com/project/trivial-main-thread.git")
  :serial T
  :components ((:file "package")
               (:file "main-thread")
               (:file "documentation"))
  :depends-on (:trivial-features
               :documentation-utils
               :bordeaux-threads))
