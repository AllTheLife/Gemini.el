;;; bard.el --- Bard in Emacs  -*- lexical-binding: t -*-

;; Filename: bard.el
;; Description: Bard in Emacs
;; Author: AllTheLife <xjn208930@gmail.com>
;; Maintainer: AllTheLife <xjn208930@gmail.com>
;; Copyright (C) 2023, AllTheLife, all rights reserved.
;; Created: 2023-04-28 19:03:14
;; Version: 0.3
;; Last-Updated: 2023-04-30 12:55:19
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

(defun bard-response (serial-number content buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (unless (= serial-number 1)
	(insert "\n"))
      (let ((point (point-max)))
	(insert (format "### Draft %d:\n" serial-number))
	(insert content "\n")
	(unless (equal serial-number 1)
	  (goto-char point)
	  (markdown-cycle))))))

(define-derived-mode bard-edit-mode text-mode "bard/edit"
  "The major mode to edit focus text input.")

(setq bard-edit-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c C-c") #'bard-edit-mode-confirm)
	(define-key map (kbd "C-c C-k") #'bard-edit-mode-cancel)
	map))

(defun bard-chat ()
  (interactive)
  (let ((prompt (read-string "Chat with Bard: ")))
    (if (string-empty-p (string-trim prompt))
	(message "Please do not enter an empty prompt.")
      (bard-chat-with-message prompt))))

(defun bard-chat-with-multiline ()
  (interactive)
  (let* ((bufname (buffer-name))
	 (edit-buffer (generate-new-buffer (format "*bard-edit-buffer-%s*" bufname))))
    (split-window-below -12)
    (other-window 1)
    (with-current-buffer edit-buffer
      (bard-edit-mode)
      (set (make-local-variable 'bard-edit-buffer-name) bufname))
    (switch-to-buffer edit-buffer)
    (bard--edit-set-header-line)))

(defun bard--edit-set-header-line ()
  "Set header line."
  (setq header-line-format
        (substitute-command-keys
         (concat
          " Bard Edit Mode: "
          "Confirm with  C-c C-c, "
          "Cancel with  C-c C-k. "
          ))))

(defun bard-get-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun bard-edit-mode-cancel ()
  (interactive)
  (kill-buffer)
  (delete-window)
  (message "[Bard] Edit cancelled!"))

(defun bard-edit-mode-confirm ()
  (interactive)
  (let* ((bufname bard-edit-buffer-name)
	 (prompt (bard-get-buffer-string)))
    (kill-buffer)
    (delete-window)

    (switch-to-buffer bufname)
    (bard-chat-with-message prompt)))

(defun bard-return-code (content buffer begin end)
  (let* ((block-start (string-match "```" content))
	 (code-begin (string-match "\n" content (+ block-start 3)))
	 (code-end (string-match "```" content code-begin)))
    (with-current-buffer buffer
      (setq code-begin (+ code-begin 1))
      (delete-region begin end)
      (goto-char begin)
      (insert (substring content code-begin code-end)))))

(defun bard-generate-code ()
  (interactive)
  (let* ((selection (if (region-active-p)
                        (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (prompt (if (= (length selection) 0)
                     (format "%s, please only output the code, without any explanations or instructions." (read-string "Prompt: "))
                   (format "%s, please only output the code, without any explanations or instructions." (concat mode " " selection)))))
    (insert "\n")
    (bard-call-async "bard_text"
		     prompt
                     (buffer-name)
                     ""
                     "Generating..."
                     "Generate code done."
                     (point)
                     (point)
		     "bard-return-code")))

(defun bard-adjust-code ()
  (interactive)
  (let* ((selection (if (region-active-p)
                        (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (prompt (format "%s in the %s code below, please only output the code, without any explanations or instructions."
			 (read-string "Adjust: ") mode)))
    (bard-call-async "bard_text"
		     prompt
                     (buffer-name)
                     selection
                     "Adgjusting."
                     "Adjust code done."
                     (region-beginning)
                     (region-end)
		     "bard-return-code")))

(defun bard-polish-document ()
  (interactive)
  (let* ((document (if (region-active-p)
		       (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		     (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	 (buffer (generate-new-buffer (format "*bard-doc-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (bard-call-async "bard_text"
		     "Please help me proofread and polish the following text:\n"
		     (buffer-name)
		     document
		     "Polishing."
		     "Polish document done.")))

(defun bard-explain-code ()
  (interactive)
  (let* ((code (if (region-active-p)
			(string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		      (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	 (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
	 (buffer (generate-new-buffer (format "*bard-explain-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (bard-call-async "bard_text"
		     (format "Please explain in detail the meaning of the following %s code, leave a blank line between each sentence:\n" mode)
		     (buffer-name)
		     code
		     "Explaining"
		     "Explain code done.")))

(defun bard-comment-code ()
  (interactive)
  (let* ((code (if (region-active-p)
			(string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		      (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	 (begin (if (region-active-p)
		    (region-beginning)
		  (point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))
    (bard-call-async "bard_text"
		     (format "Please add code comments to the following %s code, with the comments written in English within the code, and output the code including the comments." mode)
		     (buffer-name)
		     code
		     "Commenting"
		     "Comment code done."
		     begin
		     end
		     "bard-return-code")))

(defun bard-refactory-code ()
  (interactive)
  (let* ((code (if (region-active-p)
			(string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		      (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	 (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
	 (buffer (generate-new-buffer (format "*bard-explain-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (bard-call-async "bard_text"
		     (format "Please help me refactor the following %s code. Please reply with the refactoring explanation, refactored code, and diff between two versions. Please ignore the comments and strings in the code during the refactoring. If the code remains unchanged after refactoring, please say 'No need to refactor'." mode)
		     (buffer-name)
		     code
		     "Refactorying"
		     "Refactory code done.")))

(defun bard-generate-commit-message ()
  (interactive)
  (bard-call-async "git_commit"
		   default-directory
		   (buffer-name)
		   (point)
		   (point)))

(unless bard-is-starting
  (bard-start-process))


(provide 'bard)
;;; bard.el ends here
