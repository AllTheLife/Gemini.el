;;; gemini-epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2024 Enze Chi

;; Author: Enze Chi <Enze.Chi@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro gemini-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar gemini-deferred-debug nil
  "Debug output switch.")

(defvar gemini-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun gemini-deferred-log (&rest args)
  "[internal] Debug log function."
  (when gemini-deferred-debug
    (with-current-buffer (get-buffer-create "*gemini-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" gemini-deferred-debug-count (apply #'format args)))))
    (cl-incf gemini-deferred-debug-count)))

(defvar gemini-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro gemini-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`gemini-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal gemini-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar gemini-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar gemini-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `gemini-deferred-post-task' and `gemini-deferred-worker'.")

(defun gemini-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`gemini-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack gemini-deferred-queue)
    (gemini-deferred-log "QUEUE-POST [%s]: %s" (length gemini-deferred-queue) pack)
    (run-at-time gemini-deferred-tick-time nil 'gemini-deferred-worker)
    d))

(defun gemini-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when gemini-deferred-queue
    (let* ((pack (car (last gemini-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq gemini-deferred-queue (nbutlast gemini-deferred-queue))
      (condition-case err
          (setq value (gemini-deferred-exec-task d which arg))
        (error
         (gemini-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: gemini-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `gemini-deferred-resignal')
;; cancel      : a canceling function (default `gemini-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct gemini-deferred-object
  (callback 'identity)
  (errorback 'gemini-deferred-resignal)
  (cancel 'gemini-deferred-default-cancel)
  next status value)

(defun gemini-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun gemini-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (gemini-deferred-log "CANCEL : %s" d)
  (setf (gemini-deferred-object-callback d) 'identity)
  (setf (gemini-deferred-object-errorback d) 'gemini-deferred-resignal)
  (setf (gemini-deferred-object-next d) nil)
  d)

(defun gemini-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (gemini-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "gemini-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (gemini-deferred-object-callback d)
                    (gemini-deferred-object-errorback d)))
        (next-deferred (gemini-deferred-object-next d)))
    (cond
     (callback
      (gemini-deferred-condition-case err
                                      (let ((value (funcall callback arg)))
                                        (cond
                                         ((gemini-deferred-object-p value)
                                          (gemini-deferred-log "WAIT NEST : %s" value)
                                          (if next-deferred
                                              (gemini-deferred-set-next value next-deferred)
                                            value))
                                         (t
                                          (if next-deferred
                                              (gemini-deferred-post-task next-deferred 'ok value)
                                            (setf (gemini-deferred-object-status d) 'ok)
                                            (setf (gemini-deferred-object-value d) value)
                                            value))))
                                      (error
                                       (cond
                                        (next-deferred
                                         (gemini-deferred-post-task next-deferred 'ng err))
                                        (t
                                         (gemini-deferred-log "ERROR : %S" err)
                                         (message "deferred error : %S" err)
                                         (setf (gemini-deferred-object-status d) 'ng)
                                         (setf (gemini-deferred-object-value d) err)
                                         err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (gemini-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (gemini-deferred-resignal arg)))))))

(defun gemini-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (gemini-deferred-object-next prev) next)
  (cond
   ((eq 'ok (gemini-deferred-object-status prev))
    (setf (gemini-deferred-object-status prev) nil)
    (let ((ret (gemini-deferred-exec-task
                next 'ok (gemini-deferred-object-value prev))))
      (if (gemini-deferred-object-p ret) ret
        next)))
   ((eq 'ng (gemini-deferred-object-status prev))
    (setf (gemini-deferred-object-status prev) nil)
    (let ((ret (gemini-deferred-exec-task next 'ng (gemini-deferred-object-value prev))))
      (if (gemini-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun gemini-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-gemini-deferred-object :callback callback)
    (make-gemini-deferred-object)))

(defun gemini-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (gemini-deferred-exec-task d 'ok arg))

(defun gemini-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (gemini-deferred-exec-task d 'ng arg))

(defun gemini-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (gemini-deferred-post-task d 'ok arg))

(defun gemini-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (gemini-deferred-callback-post (gemini-deferred-new callback))."
  (let ((d (if callback
               (make-gemini-deferred-object :callback callback)
             (make-gemini-deferred-object))))
    (gemini-deferred-callback-post d arg)
    d))

(defun gemini-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-gemini-deferred-object :callback callback)))
    (gemini-deferred-set-next d nd)))

(defun gemini-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-gemini-deferred-object :errorback callback)))
    (gemini-deferred-set-next d nd)))

(defvar gemini-epc-debug nil)

(defun gemini-epc-log (&rest args)
  (when gemini-epc-debug
    (with-current-buffer (get-buffer-create "*gemini-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun gemini-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar gemini-epc-uid 1)

(defun gemini-epc-uid ()
  (cl-incf gemini-epc-uid))

(defvar gemini-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct gemini-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun gemini-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return gemini-epc-connection object."
  (gemini-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (gemini-epc-uid))
         (connection-name (format "gemini-epc con %s" connection-id))
         (connection-buf (gemini-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-gemini-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (gemini-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (gemini-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (gemini-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun gemini-epc-process-sentinel (connection process msg)
  (gemini-epc-log "!! Process Sentinel [%s] : %S : %S"
                  (gemini-epc-connection-name connection) process msg)
  (gemini-epc-disconnect connection))

(defun gemini-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (gemini-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (gemini-epc-connection-process connection)))
    (gemini-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun gemini-epc-disconnect (connection)
  (let ((process (gemini-epc-connection-process connection))
        (buf (gemini-epc-connection-buffer connection))
        (name (gemini-epc-connection-name connection)))
    (gemini-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (gemini-epc-log "!! Disconnected finished [%s]" name)))

(defun gemini-epc-process-filter (connection process message)
  (gemini-epc-log "INCOMING: [%s] [%S]" (gemini-epc-connection-name connection) message)
  (with-current-buffer (gemini-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (gemini-epc-process-available-input connection process)))

(defun gemini-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (gemini-deferred-new callback)
             (gemini-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun gemini-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (gemini-deferred-callback-post d event))))

(defun gemini-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (gemini-epc-net-have-input-p)
      (let ((event (gemini-epc-net-read-or-lose process))
            (ok nil))
        (gemini-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'gemini-epc-signal-send
                         (cons (gemini-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (gemini-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (gemini-epc-process-available-input connection process)))))))

(defun gemini-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (gemini-epc-net-decode-length))))

(defun gemini-epc-net-read-or-lose (_process)
  (condition-case error
      (gemini-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun gemini-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (gemini-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun gemini-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun gemini-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct gemini-epc-manager
  "Root object that holds all information related to an EPC activity.

`gemini-epc-start-epc' returns this object.

title          : instance name for displaying on the `gemini-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : gemini-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct gemini-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar gemini-epc-live-connections nil
  "[internal] A list of `gemini-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun gemini-epc-server-process-name (uid)
  (format "gemini-epc-server:%s" uid))

(defun gemini-epc-server-buffer-name (uid)
  (format " *%s*" (gemini-epc-server-process-name uid)))

(defun gemini-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (gemini-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (gemini-epc-disconnect (gemini-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 gemini-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq gemini-epc-live-connections (delete mngr gemini-epc-live-connections))
    ))

(defun gemini-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun gemini-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an gemini-epc-connection instance."
  (let* ((mngr mngr)
         (conn (gemini-epc-manager-connection mngr))
         (channel (gemini-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (gemini-epc-log "SIG CALL: %S" args)
                    (apply 'gemini-epc-handler-called-method ,mngr (gemini-epc-args args))))
               (return
                . (lambda (args)
                    (gemini-epc-log "SIG RET: %S" args)
                    (apply 'gemini-epc-handler-return ,mngr (gemini-epc-args args))))
               (return-error
                . (lambda (args)
                    (gemini-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'gemini-epc-handler-return-error ,mngr (gemini-epc-args args))))
               (epc-error
                . (lambda (args)
                    (gemini-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'gemini-epc-handler-epc-error ,mngr (gemini-epc-args args))))
               (methods
                . (lambda (args)
                    (gemini-epc-log "SIG METHODS: %S" args)
                    (gemini-epc-handler-methods ,mngr (caadr args))))
               ) do
             (gemini-epc-signal-connect channel method body))
    (push mngr gemini-epc-live-connections)
    mngr))

(defun gemini-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (gemini-epc-manager-connection mngr)))
    (gemini-epc-net-send conn (cons method messages))))

(defun gemini-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (gemini-epc-manager-methods mngr)
           if (eq method-name (gemini-epc-method-name i))
           do (cl-return i)))

(defun gemini-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (gemini-epc-manager-methods mngr)
                  collect
                  (list
                   (gemini-epc-method-name i)
                   (or (gemini-epc-method-arg-specs i) "")
                   (or (gemini-epc-method-docstring i) "")))))
    (gemini-epc-manager-send mngr 'return uid info)))

(defun gemini-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (gemini-epc-manager-methods mngr))
           (method (gemini-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (gemini-epc-log "ERR: No such method : %s" name)
        (gemini-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (gemini-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((gemini-deferred-object-p ret)
                (gemini-deferred-nextc ret
                                       (lambda (xx) (gemini-epc-manager-send mngr 'return uid xx))))
               (t (gemini-epc-manager-send mngr 'return uid ret))))
          (error
           (gemini-epc-log "ERROR : %S" err)
           (gemini-epc-manager-send mngr 'return-error uid err))))))))

(defun gemini-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (gemini-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (gemini-epc-manager-sessions mngr) ret)))

(defun gemini-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (gemini-epc-manager-sessions mngr))))
    (cond
     (pair
      (gemini-epc-log "RET: id:%s [%S]" uid args)
      (gemini-epc-manager-remove-session mngr uid)
      (gemini-deferred-callback (cdr pair) args))
     (t                                 ; error
      (gemini-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun gemini-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (gemini-epc-manager-sessions mngr))))
    (cond
     (pair
      (gemini-epc-log "RET-ERR: id:%s [%S]" uid args)
      (gemini-epc-manager-remove-session mngr uid)
      (gemini-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (gemini-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun gemini-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (gemini-epc-manager-sessions mngr))))
    (cond
     (pair
      (gemini-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (gemini-epc-manager-remove-session mngr uid)
      (gemini-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (gemini-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun gemini-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (gemini-epc-uid))
        (sessions (gemini-epc-manager-sessions mngr))
        (d (gemini-deferred-new)))
    (push (cons uid d) sessions)
    (setf (gemini-epc-manager-sessions mngr) sessions)
    (gemini-epc-manager-send mngr 'call uid method-name args)
    d))

(defun gemini-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-gemini-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (gemini-epc-manager-methods mngr))))
    (setf (gemini-epc-manager-methods mngr) methods)
    method))

(defun gemini-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'gemini-epc-nothing))
    (gemini-deferred-chain
     d
     (gemini-deferred-nextc it
                            (lambda (x) (setq result x)))
     (gemini-deferred-error it
                            (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'gemini-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (gemini-epc-connection-process (gemini-epc-manager-connection mngr))
         0 gemini-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun gemini-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (gemini-epc-sync mngr (gemini-epc-call-deferred mngr method-name args)))

(defun gemini-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (gemini-epc-connection-process (gemini-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar gemini-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`gemini-epc-manager' instance]).
When the server process accepts the client connection, the
`gemini-epc-manager' instance is created and stored in this variable
`gemini-epc-server-client-processes'. This variable is used for the management
purpose.")

;; gemini-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `gemini-epc-manager' instances
(cl-defstruct gemini-epc-server name process port connect-function)

(defvar gemini-epc-server-processes nil
  "[internal] A list of ([process object] . [`gemini-epc-server' instance]).
This variable is used for the management purpose.")

(defun gemini-epc-server-get-manager-by-process (proc)
  "[internal] Return the gemini-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in gemini-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun gemini-epc-server-accept (process)
  "[internal] Initialize the process and return gemini-epc-manager object."
  (gemini-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (gemini-epc-uid))
         (connection-name (format "gemini-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-gemini-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (gemini-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (gemini-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (gemini-epc-process-sentinel connection p e)))
    (make-gemini-epc-manager :server-process process :port t
                             :connection connection)))

(defun gemini-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (gemini-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (gemini-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (gemini-epc-server-accept process)))
            (push (cons process mngr) gemini-epc-server-client-processes)
            (gemini-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (gemini-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (gemini-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process gemini-epc-server-client-processes)) _d)
        (when pair
          (gemini-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (gemini-epc-stop-epc (cdr pair))
          (setq gemini-epc-server-client-processes
                (assq-delete-all process gemini-epc-server-client-processes))
          ))
      nil))))

(defun gemini-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "Gemini EPC Server %s" (gemini-epc-uid)))
       (buf (gemini-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (gemini-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-gemini-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          gemini-epc-server-processes)
    main-process))

(provide 'gemini-epc)
;;; gemini-epc.el ends here
