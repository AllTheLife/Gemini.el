;;; bard.el --- Bard in Emacs  -*- lexical-binding: t -*-

;; Filename: bard.el
;; Description: Bard in Emacs
;; Author: AllTheLife <xjn208930@gmail.com>
;; Maintainer: AllTheLife <xjn208930@gmail.com>
;; Copyright (C) 2023, AllTheLife, all rights reserved.
;; Created: 2023-04-28 19:03:14
;; Version: 0.1
;; Last-Updated: 2023-04-29 15:02:29
;;           By: AllTheLife
;; URL:
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Bard.el
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET bard RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'bard-epc)
(require 'markdown-mode)

(defgroup bard nil
  "Bard group."
  :group 'applications)

(defcustom bard-cookie-token-path (expand-file-name (concat user-emacs-directory (file-name-as-directory "bard") "bard_cookie_token.txt"))
  "The file to get the token key to Google Bard."
  :type 'string
  :group 'bard)

(defvar bard-server nil
  "The Bard Server.")

(defvar bard-python-file (expand-file-name "bard.py" (if load-file-name
                                                                   (file-name-directory load-file-name)
                                                                 default-directory)))

(defvar bard-server-port nil)

(add-to-list 'auto-mode-alist '("\\.bard$" . markdown-mode))

(defun bard--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p bard-server)
    (setq bard-server
          (bard-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (bard-epc-define-method mngr 'eval-in-emacs 'bard--eval-in-emacs-func)
               (bard-epc-define-method mngr 'get-emacs-var 'bard--get-emacs-var-func)
               (bard-epc-define-method mngr 'get-emacs-vars 'bard--get-emacs-vars-func)
               (bard-epc-define-method mngr 'get-user-emacs-directory 'bard--user-emacs-directory)
               ))))
    (if bard-server
        (setq bard-server-port (process-contact bard-server :service))
      (error "[Bard] bard-server failed to start")))
  bard-server)

(defun bard--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun bard--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun bard--get-emacs-vars-func (&rest vars)
  (mapcar #'bard--get-emacs-var-func vars))

(defvar bard-epc-process nil)

(defvar bard-internal-process nil)
(defvar bard-internal-process-prog nil)
(defvar bard-internal-process-args nil)

(defcustom bard-name "*bard*"
  "Name of Bard buffer."
  :type 'string)

(defcustom bard-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run bard.py"
  :type 'string)

(defcustom bard-enable-debug nil
  "If you got segfault error, please turn this option.
Then Bard will start by gdb, please send new issue with `*bard*' buffer content when next crash."
  :type 'boolean)

(defcustom bard-enable-log nil
  "Enable this option to print log message in `*bard*' buffer, default only print message header."
  :type 'boolean)

(defcustom bard-enable-profile nil
  "Enable this option to output performance data to ~/bard.prof."
  :type 'boolean)

(defun bard--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun bard-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (bard-epc-live-p bard-epc-process)
      (bard-deferred-chain
	(bard-epc-call-deferred bard-epc-process (read method) args))
    (bard-start-process)))

(defvar bard-is-starting nil)

(defun bard-restart-process ()
  "Stop and restart Bard process."
  (interactive)
  (setq bard-is-starting nil)

  (bard-kill-process)
  (bard-start-process)
  (message "[Bard] Process restarted."))

(defun bard-start-process ()
  "Start Bard process if it isn't started."
  (setq bard-is-starting t)
  (unless (bard-epc-live-p bard-epc-process)
    ;; start epc server and set `bard-server-port'
    (bard--start-epc-server)
    (let* ((bard-args (append
                            (list bard-python-file)
                            (list (number-to-string bard-server-port))
                            (when bard-enable-profile
                              (list "profile"))
                            )))

      ;; Set process arguments.
      (if bard-enable-debug
          (progn
            (setq bard-internal-process-prog "gdb")
            (setq bard-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" bard-python-command) bard-args)))
        (setq bard-internal-process-prog bard-python-command)
        (setq bard-internal-process-args bard-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq bard-internal-process
              (apply 'start-process
                     bard-name bard-name
                     bard-internal-process-prog bard-internal-process-args)))
      (set-process-query-on-exit-flag bard-internal-process nil))))

(defvar bard-stop-process-hook nil)

(defun bard-kill-process ()
  "Stop Bard process and kill all Bard buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'bard-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (bard--kill-python-process))

(add-hook 'kill-emacs-hook #'bard-kill-process)

(defun bard--kill-python-process ()
  "Kill Bard background python process."
  (when (bard-epc-live-p bard-epc-process)
    ;; Cleanup before exit Bard server process.
    (bard-call-async "cleanup")
    ;; Delete Bard server process.
    (bard-epc-stop-epc bard-epc-process)
    ;; Kill *bard* buffer.
    (when (get-buffer bard-name)
      (kill-buffer bard-name))
    (setq bard-epc-process nil)
    (message "[Bard] Process terminated.")))

(defun bard--first-start (bard-epc-port)
  "Call `bard--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq bard-epc-process (make-bard-epc-manager
                               :server-process bard-internal-process
                               :commands (cons bard-internal-process-prog bard-internal-process-args)
                               :title (mapconcat 'identity (cons bard-internal-process-prog bard-internal-process-args) " ")
                               :port bard-epc-port
                               :connection (bard-epc-connect "localhost" bard-epc-port)
                               ))
  (bard-epc-init-epc-layer bard-epc-process)
  (setq bard-is-starting nil)

  (message "[Bard] Process started successfully."))

(defun bard-chat-with-message (prompt)
  (save-excursion
    (goto-char (point-max))
    (insert "## User:\n")
    (insert (format "%s\n" prompt)))

  (message "[Bard] Please wait for Bard...")
  (bard-call-async "bard_chat"
		   prompt
		   (buffer-name)))

(defun bard-insert-answer (serial-number content buffer)
  (save-excursion
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (insert "\n")
    (let ((point (point-max)))
      (goto-char point)
      (insert (format "### Draft %d:\n" serial-number))
      (insert content "\n")
      (unless (equal serial-number 1)
	(goto-char point)
	(markdown-cycle)))))

(defun bard-finish-answer (buffer)
  (save-excursion
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (insert "\n\n")
    (message "[Bard] Bard finished replying.")))

(defun bard-chat ()
  (interactive)
  (let ((prompt (read-string "Chat with Bard: ")))
    (if (string-empty-p (string-trim prompt))
	(message "Please do not enter an empty prompt.")
      (bard-chat-with-message prompt))))

(unless bard-is-starting
  (bard-start-process))


(provide 'bard)

;;; bard.el ends here
