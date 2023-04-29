;;; bard-epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
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
(cl-defmacro bard-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar bard-deferred-debug nil
  "Debug output switch.")

(defvar bard-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun bard-deferred-log (&rest args)
  "[internal] Debug log function."
  (when bard-deferred-debug
    (with-current-buffer (get-buffer-create "*bard-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" bard-deferred-debug-count (apply #'format args)))))
    (cl-incf bard-deferred-debug-count)))

(defvar bard-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro bard-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`bard-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal bard-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar bard-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar bard-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `bard-deferred-post-task' and `bard-deferred-worker'.")

(defun bard-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`bard-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack bard-deferred-queue)
    (bard-deferred-log "QUEUE-POST [%s]: %s" (length bard-deferred-queue) pack)
    (run-at-time bard-deferred-tick-time nil 'bard-deferred-worker)
    d))

(defun bard-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when bard-deferred-queue
    (let* ((pack (car (last bard-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq bard-deferred-queue (nbutlast bard-deferred-queue))
      (condition-case err
          (setq value (bard-deferred-exec-task d which arg))
        (error
         (bard-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: bard-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `bard-deferred-resignal')
;; cancel      : a canceling function (default `bard-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct bard-deferred-object
  (callback 'identity)
  (errorback 'bard-deferred-resignal)
  (cancel 'bard-deferred-default-cancel)
  next status value)

(defun bard-deferred-resignal (err)
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

(defun bard-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (bard-deferred-log "CANCEL : %s" d)
  (setf (bard-deferred-object-callback d) 'identity)
  (setf (bard-deferred-object-errorback d) 'bard-deferred-resignal)
  (setf (bard-deferred-object-next d) nil)
  d)

(defun bard-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (bard-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "bard-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (bard-deferred-object-callback d)
                    (bard-deferred-object-errorback d)))
        (next-deferred (bard-deferred-object-next d)))
    (cond
     (callback
      (bard-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((bard-deferred-object-p value)
                                             (bard-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (bard-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (bard-deferred-post-task next-deferred 'ok value)
                                               (setf (bard-deferred-object-status d) 'ok)
                                               (setf (bard-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (bard-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (bard-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (bard-deferred-object-status d) 'ng)
                                            (setf (bard-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (bard-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (bard-deferred-resignal arg)))))))

(defun bard-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (bard-deferred-object-next prev) next)
  (cond
   ((eq 'ok (bard-deferred-object-status prev))
    (setf (bard-deferred-object-status prev) nil)
    (let ((ret (bard-deferred-exec-task
                next 'ok (bard-deferred-object-value prev))))
      (if (bard-deferred-object-p ret) ret
        next)))
   ((eq 'ng (bard-deferred-object-status prev))
    (setf (bard-deferred-object-status prev) nil)
    (let ((ret (bard-deferred-exec-task next 'ng (bard-deferred-object-value prev))))
      (if (bard-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun bard-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-bard-deferred-object :callback callback)
    (make-bard-deferred-object)))

(defun bard-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (bard-deferred-exec-task d 'ok arg))

(defun bard-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (bard-deferred-exec-task d 'ng arg))

(defun bard-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (bard-deferred-post-task d 'ok arg))

(defun bard-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (bard-deferred-callback-post (bard-deferred-new callback))."
  (let ((d (if callback
               (make-bard-deferred-object :callback callback)
             (make-bard-deferred-object))))
    (bard-deferred-callback-post d arg)
    d))

(defun bard-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-bard-deferred-object :callback callback)))
    (bard-deferred-set-next d nd)))

(defun bard-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-bard-deferred-object :errorback callback)))
    (bard-deferred-set-next d nd)))

(defvar bard-epc-debug nil)

(defun bard-epc-log (&rest args)
  (when bard-epc-debug
    (with-current-buffer (get-buffer-create "*bard-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun bard-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar bard-epc-uid 1)

(defun bard-epc-uid ()
  (cl-incf bard-epc-uid))

(defvar bard-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct bard-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun bard-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return bard-epc-connection object."
  (bard-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (bard-epc-uid))
         (connection-name (format "bard-epc con %s" connection-id))
         (connection-buf (bard-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-bard-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (bard-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (bard-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (bard-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun bard-epc-process-sentinel (connection process msg)
  (bard-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (bard-epc-connection-name connection) process msg)
  (bard-epc-disconnect connection))

(defun bard-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (bard-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (bard-epc-connection-process connection)))
    (bard-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun bard-epc-disconnect (connection)
  (let ((process (bard-epc-connection-process connection))
        (buf (bard-epc-connection-buffer connection))
        (name (bard-epc-connection-name connection)))
    (bard-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (bard-epc-log "!! Disconnected finished [%s]" name)))

(defun bard-epc-process-filter (connection process message)
  (bard-epc-log "INCOMING: [%s] [%S]" (bard-epc-connection-name connection) message)
  (with-current-buffer (bard-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (bard-epc-process-available-input connection process)))

(defun bard-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (bard-deferred-new callback)
             (bard-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun bard-epc-signal-send (channel event-sym &rest args)
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
             do (bard-deferred-callback-post d event))))

(defun bard-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (bard-epc-net-have-input-p)
      (let ((event (bard-epc-net-read-or-lose process))
            (ok nil))
        (bard-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'bard-epc-signal-send
                         (cons (bard-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (bard-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (bard-epc-process-available-input connection process)))))))

(defun bard-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (bard-epc-net-decode-length))))

(defun bard-epc-net-read-or-lose (_process)
  (condition-case error
      (bard-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun bard-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (bard-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun bard-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun bard-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct bard-epc-manager
  "Root object that holds all information related to an EPC activity.

`bard-epc-start-epc' returns this object.

title          : instance name for displaying on the `bard-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : bard-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct bard-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar bard-epc-live-connections nil
  "[internal] A list of `bard-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun bard-epc-server-process-name (uid)
  (format "bard-epc-server:%s" uid))

(defun bard-epc-server-buffer-name (uid)
  (format " *%s*" (bard-epc-server-process-name uid)))

(defun bard-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (bard-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (bard-epc-disconnect (bard-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 bard-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq bard-epc-live-connections (delete mngr bard-epc-live-connections))
    ))

(defun bard-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun bard-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an bard-epc-connection instance."
  (let* ((mngr mngr)
         (conn (bard-epc-manager-connection mngr))
         (channel (bard-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (bard-epc-log "SIG CALL: %S" args)
                    (apply 'bard-epc-handler-called-method ,mngr (bard-epc-args args))))
               (return
                . (lambda (args)
                    (bard-epc-log "SIG RET: %S" args)
                    (apply 'bard-epc-handler-return ,mngr (bard-epc-args args))))
               (return-error
                . (lambda (args)
                    (bard-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'bard-epc-handler-return-error ,mngr (bard-epc-args args))))
               (epc-error
                . (lambda (args)
                    (bard-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'bard-epc-handler-epc-error ,mngr (bard-epc-args args))))
               (methods
                . (lambda (args)
                    (bard-epc-log "SIG METHODS: %S" args)
                    (bard-epc-handler-methods ,mngr (caadr args))))
               ) do
             (bard-epc-signal-connect channel method body))
    (push mngr bard-epc-live-connections)
    mngr))

(defun bard-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (bard-epc-manager-connection mngr)))
    (bard-epc-net-send conn (cons method messages))))

(defun bard-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (bard-epc-manager-methods mngr)
           if (eq method-name (bard-epc-method-name i))
           do (cl-return i)))

(defun bard-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (bard-epc-manager-methods mngr)
                  collect
                  (list
                   (bard-epc-method-name i)
                   (or (bard-epc-method-arg-specs i) "")
                   (or (bard-epc-method-docstring i) "")))))
    (bard-epc-manager-send mngr 'return uid info)))

(defun bard-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (bard-epc-manager-methods mngr))
           (method (bard-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (bard-epc-log "ERR: No such method : %s" name)
        (bard-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (bard-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((bard-deferred-object-p ret)
                (bard-deferred-nextc ret
                                          (lambda (xx) (bard-epc-manager-send mngr 'return uid xx))))
               (t (bard-epc-manager-send mngr 'return uid ret))))
          (error
           (bard-epc-log "ERROR : %S" err)
           (bard-epc-manager-send mngr 'return-error uid err))))))))

(defun bard-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (bard-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (bard-epc-manager-sessions mngr) ret)))

(defun bard-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (bard-epc-manager-sessions mngr))))
    (cond
     (pair
      (bard-epc-log "RET: id:%s [%S]" uid args)
      (bard-epc-manager-remove-session mngr uid)
      (bard-deferred-callback (cdr pair) args))
     (t                                 ; error
      (bard-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun bard-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (bard-epc-manager-sessions mngr))))
    (cond
     (pair
      (bard-epc-log "RET-ERR: id:%s [%S]" uid args)
      (bard-epc-manager-remove-session mngr uid)
      (bard-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (bard-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun bard-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (bard-epc-manager-sessions mngr))))
    (cond
     (pair
      (bard-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (bard-epc-manager-remove-session mngr uid)
      (bard-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (bard-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun bard-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (bard-epc-uid))
        (sessions (bard-epc-manager-sessions mngr))
        (d (bard-deferred-new)))
    (push (cons uid d) sessions)
    (setf (bard-epc-manager-sessions mngr) sessions)
    (bard-epc-manager-send mngr 'call uid method-name args)
    d))

(defun bard-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-bard-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (bard-epc-manager-methods mngr))))
    (setf (bard-epc-manager-methods mngr) methods)
    method))

(defun bard-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'bard-epc-nothing))
    (bard-deferred-chain
     d
     (bard-deferred-nextc it
                               (lambda (x) (setq result x)))
     (bard-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'bard-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (bard-epc-connection-process (bard-epc-manager-connection mngr))
         0 bard-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun bard-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (bard-epc-sync mngr (bard-epc-call-deferred mngr method-name args)))

(defun bard-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (bard-epc-connection-process (bard-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar bard-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`bard-epc-manager' instance]).
When the server process accepts the client connection, the
`bard-epc-manager' instance is created and stored in this variable
`bard-epc-server-client-processes'. This variable is used for the management
purpose.")

;; bard-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `bard-epc-manager' instances
(cl-defstruct bard-epc-server name process port connect-function)

(defvar bard-epc-server-processes nil
  "[internal] A list of ([process object] . [`bard-epc-server' instance]).
This variable is used for the management purpose.")

(defun bard-epc-server-get-manager-by-process (proc)
  "[internal] Return the bard-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in bard-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun bard-epc-server-accept (process)
  "[internal] Initialize the process and return bard-epc-manager object."
  (bard-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (bard-epc-uid))
         (connection-name (format "bard-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-bard-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (bard-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (bard-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (bard-epc-process-sentinel connection p e)))
    (make-bard-epc-manager :server-process process :port t
                                :connection connection)))

(defun bard-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (bard-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (bard-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (bard-epc-server-accept process)))
            (push (cons process mngr) bard-epc-server-client-processes)
            (bard-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (bard-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (bard-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process bard-epc-server-client-processes)) _d)
        (when pair
          (bard-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (bard-epc-stop-epc (cdr pair))
          (setq bard-epc-server-client-processes
                (assq-delete-all process bard-epc-server-client-processes))
          ))
      nil))))

(defun bard-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "BARD EPC Server %s" (bard-epc-uid)))
       (buf (bard-epc-make-procbuf (format " *%s*" name)))
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
           (bard-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-bard-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          bard-epc-server-processes)
    main-process))

(provide 'bard-epc)
;;; bard-epc.el ends here
