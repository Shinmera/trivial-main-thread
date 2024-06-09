(in-package #:trivial-main-thread)

(docs:define-docs
  (variable *on-error*
    "Sets what to do if an error is signalled in the main thread runner.

The value may be one of the following:
  :RESIGNAL --- Resignal the condition in the calling thread. If the
                condition is not caused by a task, the debugger is
                invoked instead
  :QUIT     --- The implementation exits via UIOP:QUIT
  NIL
  :IGNORE   --- The error is not handled and the behaviour is
                implementation-dependent
  FUNCTION
  SYMBOL    --- The named function is invoked with the error. Note
                that this makes the following also permissible:
    STOP-MAIN-RUNNER
    INVOKE-DEBUGGER
    CONTINUE
    ABORT

Defaults to :RESIGNAL

See START-MAIN-RUNNER
See STOP-MAIN-RUNNER
See CL:INVOKE-DEBUGGER
See CL:CONTINUE
See CL:ABORT
See UIOP:QUIT")
  
  (function main-thread
    "Returns the main thread.

See MAIN-THREAD-P")
  
  (function main-thread-p
    "Returns true if the given thread is the main thread.

See MAIN-THREAD")
  
  (function swap-main-thread
   "Swaps the main thread for our own FUNCTION.

If the implementation uses the main thread for vital tasks, this function
tries to ensure that these vital tasks are continued in a new thread instead.

If this is called from the main thread, the function is simply invoked.

See MAIN-THREAD-P")
  
  (function start-main-runner
   "Starts the runner in the main thread.

If the runner is already running, an error is signalled.

Otherwise, the main thread is hijacked via SWAP-MAIN-THREAD.
The runner keeps a queue of tasks (function thunks) to invoke. When it
gets woken up to process tasks (via CALL-IN-MAIN-THREAD), it goes
through the queue, invokes the functions in sequence, and puts the
function's return values back into the queue so the caller can process
them. If an error is signalled during task processing, the behaviour
described in *ON-ERROR* is executed.

See *ON-ERROR*
See SWAP-MAIN-THREAD
See ENSURE-MAIN-RUNNER")
  
  (function stop-main-runner
   "Stops the runner in the main thread, allowing it to continue.

If the runner is not running in the main thread, an error is
signalled. If this is called from within the runner, the function
causes an unwind.

See START-MAIN-RUNNER")
  
  (function ensure-main-runner
   "Ensure that the main thread runner is indeed running.

See START-MAIN-RUNNER
See STOP-MAIN-RUNNER")
  
  (function call-in-main-thread
   "Call FUNCTION in the main thread.

If this is called in the main thread, the function is simply invoked.

Otherwise, ENSURE-MAIN-RUNNER is called and the function is scheduled for
execution.

If BLOCKING is non-NIL, the current thread is blocked until the
function has finished running and the function's return values are
returned. If the function signals an error and *ON-ERROR* is :RESIGNAL,
the error is re-signaled in the calling thread.

See *ON-ERROR*
See MAIN-THREAD-P
See ENSURE-MAIN-RUNNER")
  
  (function with-body-in-main-thread
    "Evaluate the BODY in the main thread.

See CALL-IN-MAIN-THREAD"))
