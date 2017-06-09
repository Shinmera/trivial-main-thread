## About trivial-main-thread
Sometimes it is absolutely necessary to run certain tasks in the main thread of the implementation. This is particularly the case with GUI applications on OS X, where only thread 0 is allowed to issue drawing calls. This library aims to help with that.

## Why a Wrapper
Why not just call `bt:interrupt-thread` and be done with it? Well, an implementation is not required to provide the user with the main thread, and may instead choose to use it for its own purposes. For example, CCL uses the thread for housekeeping and signal handling. As such, some implementations require workarounds to make this go by smoothly. That's why this library exists.

## Basic Usage
Load trivial-main-thread through ASDF or Quicklisp.

    (ql:quickload :trivial-main-thread)

Now you can simply issue calls to be sent to the main thread by using `call-in-main-thread` or `with-body-in-main-thread`.

    (call-in-main-thread (lambda () (+ 1 1)))
    (with-body-in-main-thread (:blocking T)
      (+ 1 1))

Upon first usage of either of these two functions, trivial-main-thread will start a new thread to resume the main thread's functionality in if necessary, and interrupt the main thread with a task runner loop. From then on, call requests can be sent to the thread. For more information on the tasks and runners system, see [simple-tasks](https://shinmera.github.io/simple-tasks).

For example, if you wanted to start a Qt application, you could do the following:

    (call-in-main-thread #'qt:make-qapplication)
    (with-body-in-main-thread ()
      (qt:with-main-window (window (make-instance 'my-window))))

And the application will safely run in the main thread.

In the case where the main thread /is/ the evaluating thread, no additional threads are started, and the runner used will simply directly run tasks.

## Supported Implementations

* Allegro
* CCL
* CMUCL
* Clasp
* ECL
* LispWorks
* MKCL
* SBCL

The following are explicitly unsupported:

* ABCL --- By default the JVM steals the main thread and does not give it to you. If you want to use ABCL on OS X, you'll have to figure out how to make ABCL launch on thread0.
* Corman --- This implementation does not even give access to a thread listing.

## Also See

* [Simple-tasks](https://shinmera.github.io/simple-tasks) For remote task running.
