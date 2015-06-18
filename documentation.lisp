#|
This file is a part of trivial-main-thread
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:trivial-main-thread)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  ((*runner* variable)
   "Holds the runner object to be set into the main thread.

See SIMPLE-TASKS:RUNNER")
  
  ((*main-thread* variable)
   "Holds the main thread.")

  (find-main-thread
   "Attempts to find the main thread (thread 0) of the implementation.

If no implementation specific handling is implemented, the last thread in
the list of threads reported by BT:ALL-THREADS is primitively chosen.")
  
  (swap-main-thread
   "Swaps the main thread for our own FUNCTION.

If the implementation uses the main thread for vital tasks, this function
tries to ensure that these vital tasks are continued in a new thread instead.

If the MAIN-THREAD is EQL to BT:CURRENT-THREAD, then the FUNCTION is simply
called.")
  
  (start-main-runner
   "Starts the runner in the main thread.

If a runner or main-thread is passed explicitly, *RUNNER* and *MAIN-THREAD*
are set to those values respectively.

See SIMPLE-TASKS:START-RUNNER
See SWAP-MAIN-THREAD")
  
  (stop-main-runner
   "Stops the runner in the main thread, allowing it to continue.

This will destroy any sidestepping thread that might have been created by
SWAP-THREAD.")
  
  (ensure-main-runner
   "Ensure that the main thread runner is indeed running.

If the MAIN-THREAD is EQL to BT:CURRENT-THREAD, then the RUNNER is
CHANGE-CLASSed into SIMPLE-TASK:RUNNER. This runner class does not
use threading and instead directly executes the tasks. If we did not
do this, situations where the main thread is the currently evaluating
thread, we would block it forever, and thus stop it from ever reaching
any task scheduling calls.")
  
  (ensure-main-runner-started
   "Ensure that the main thread runner is indeed started.
This calls ENSURE-MAIN-RUNNER and then sleeps until the runner reports itself
as running.")
  
  (schedule-task
   "Schedule TASK to be run on the main thread runner.
This always calls ENSURE-MAIN-RUNNER-STARTED.

See SIMPLE-TASKS:SCHEDULE-TASK")
  
  (call-in-main-thread
   "Call FUNCTION in the main thread.
This always calls ENSURE-MAIN-RUNNER-STARTED.

If BLOCKING is non-NIL, the current thread is blocked until the function has
finished running. The function's return values are returned. Otherwise, the
task created to call the function is returned.

See SIMPLE-TASKS:CALL-AS-TASK")
  
  ((with-body-in-main-thread)
   "Evaluate the BODY in the main thread.
This always calls ENSURE-MAIN-RUNNER-STARTED.

See CALL-IN-MAIN-THREAD"))
