;;; gemini.el --- Gemini in Emacs  -*- lexical-binding: t -*-

;; Filename: gemini.el
;; Description: Gemini in Emacs
;; Author: AllTheLife <xjn208930@gmail.com>
;; Maintainer: AllTheLife <xjn208930@gmail.com>
;; Copyright (C) 2023, AllTheLife, all rights reserved.
;; Created: 2023-04-28 19:03:14
;; Version: 0.4
;; Last-Updated: 2023-06-17 19:04:39
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
;; gemini.el
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;

;;; Change log:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET bard RET
;;


;;; Acknowledgements:
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
(require 'gemini-epc)
(require 'markdown-mode)

(defgroup gemini nil
  "Gemini group."
  :group 'applications)

(defcustom gemini-api-token nil
  "The Google Gemini API token."
  :type 'string
  :group 'gemini)

(defcustom gemini-drafts (list)
  "The drafts that Gemini returned."
  :type 'listp
  :group 'gemini)

(defcustom gemini-draft--begin 0
  "Where the draft begins."
  :type 'numberp
  :group 'gemini)

(defcustom gemini-draft--end 0
  "Where the draft ends."
  :type 'numberp
  :group 'gemini)

(defvar gemini-server nil
  "The Gemini Server.")

(defvar gemini-python-file (expand-file-name "gemini.py" (if load-file-name
                                                             (file-name-directory load-file-name)
                                                           default-directory)))

(defvar gemini-server-port nil)

(defvar gemini-lang (or (ignore-errors (car (split-string (getenv "LANG") "\\.")))
                        (car (split-string current-language-environment "-"))))

(defun gemini-output-lang ()
  (pcase gemini-lang
    ("zh_CN" "中文")
    ("Chinese" "中文")
    (_ "English")))

(add-to-list 'auto-mode-alist '("\\.gemini$" . markdown-mode))

(defun gemini--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p gemini-server)
    (setq gemini-server
          (gemini-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (gemini-epc-define-method mngr 'eval-in-emacs 'gemini--eval-in-emacs-func)
               (gemini-epc-define-method mngr 'get-emacs-var 'gemini--get-emacs-var-func)
               (gemini-epc-define-method mngr 'get-emacs-vars 'gemini--get-emacs-vars-func)
               (gemini-epc-define-method mngr 'get-user-emacs-directory 'gemini--user-emacs-directory)
               ))))
    (if gemini-server
        (progn
          (setq gemini-server-port (process-contact gemini-server :service))
          (message (format "[Gemini] server:port = %s:%d" gemini-server gemini-server-port)))
      (error "[Gemini] gemini-server failed to start"))))

(defun gemini--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun gemini--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun gemini--get-emacs-vars-func (&rest vars)
  (mapcar #'gemini--get-emacs-var-func vars))

(defvar gemini-epc-process nil)

(defvar gemini-internal-process nil)
(defvar gemini-internal-process-prog nil)
(defvar gemini-internal-process-args nil)

(defcustom gemini-name "*gemini*"
  "Name of Gemini buffer."
  :type 'string)

(defcustom gemini-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run gemini.py"
  :type 'string)

(defcustom gemini-http-proxy ""
  "A proxy to access gemini.google.com. "
  :type 'string)

(defcustom gemini-enable-debug nil
  "If you got segfault error, please turn this option.
Then Gemini will start by gdb, please send new issue with `*gemini*' buffer content when next crash."
  :type 'boolean)

(defcustom gemini-enable-log nil
  "Enable this option to print log message in `*gemini*' buffer, default only print message header."
  :type 'boolean)

(defcustom gemini-enable-profile nil
  "Enable this option to output performance data to ~/gemini.prof."
  :type 'boolean)

(defun gemini--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun gemini-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (gemini-epc-live-p gemini-epc-process)
      (gemini-deferred-chain
	   (gemini-epc-call-deferred gemini-epc-process (read method) args))
    (gemini-start-process)))

(defvar gemini-is-starting nil)

(defun gemini-restart-process ()
  "Stop and restart Gemini process."
  (interactive)
  (setq gemini-is-starting nil)

  (gemini-kill-process)
  (gemini-start-process)
  (message "[Gemini] Process restarted."))

(defun gemini-start-process ()
  "Start Gemini process if it isn't started."
  (setq gemini-is-starting t)
  (unless (gemini-epc-live-p gemini-epc-process)
    ;; start epc server and set `gemini-server-port'
    (gemini--start-epc-server)
    (let* ((gemini-args (append
                         (list gemini-python-file)
                         (list (number-to-string gemini-server-port))
                         (when gemini-enable-profile
                           (list "profile"))
                         )))

      ;; Set process arguments.
      (if gemini-enable-debug
          (progn
            (setq gemini-internal-process-prog "gdb")
            (setq gemini-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" gemini-python-command) gemini-args)))
        (setq gemini-internal-process-prog gemini-python-command)
        (setq gemini-internal-process-args gemini-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq gemini-internal-process
              (apply 'start-process
                     gemini-name gemini-name
                     gemini-internal-process-prog gemini-internal-process-args)))
      (set-process-query-on-exit-flag gemini-internal-process nil))))

(defvar gemini-stop-process-hook nil)

(defun gemini-kill-process ()
  "Stop Gemini process and kill all Gemini buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'gemini-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (gemini--kill-python-process))

(add-hook 'kill-emacs-hook #'gemini-kill-process)

(defun gemini--kill-python-process ()
  "Kill Gemini background python process."
  (when (gemini-epc-live-p gemini-epc-process)
    ;; Cleanup before exit Gemini server process.
    (gemini-call-async "cleanup")
    ;; Delete Gemini server process.
    (gemini-epc-stop-epc gemini-epc-process)
    ;; Kill *gemini* buffer.
    (when (get-buffer gemini-name)
      (kill-buffer gemini-name))
    (setq gemini-epc-process nil)
    (message "[Gemini] Process terminated.")))

(defun gemini--first-start (gemini-epc-port)
  "Call `gemini--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq gemini-epc-process (make-gemini-epc-manager
                            :server-process gemini-internal-process
                            :commands (cons gemini-internal-process-prog gemini-internal-process-args)
                            :title (mapconcat 'identity (cons gemini-internal-process-prog gemini-internal-process-args) " ")
                            :port gemini-epc-port
                            :connection (gemini-epc-connect "localhost" gemini-epc-port)
                            ))
  (gemini-epc-init-epc-layer gemini-epc-process)
  (setq gemini-is-starting nil)

  (message "[Gemini] Process started successfully."))

(defun gemini-chat-with-message (prompt)
  (save-excursion
    (goto-char (point-max))
    (insert "## User:\n")
    (insert (format "%s\n" prompt)))

  (message "[Gemini] Please wait for Gemini...")
  (gemini-call-async "gemini_chat"
		             prompt
		             (buffer-name)))

(defun gemini-finish-answer (buffer)
  (save-excursion
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (insert "\n\n")
    (message "[Gemini] Gemini finished replying.")))

(defun gemini-response (serial-number content buffer)
  (if (equal serial-number 1)
      (progn
	    (setq gemini-drafts (list))
	    (push content gemini-drafts)
	    (with-current-buffer buffer
	      (save-excursion
	        (goto-char (point-max))
	        (insert "\n### Gemini:\n")
	        (setq gemini-draft--begin (point-max))
	        (insert content)
	        (setq gemini-draft--end (point-max)))))
    (push content gemini-drafts)))

(define-derived-mode gemini-edit-mode text-mode "gemini/edit"
  "The major mode to edit focus text input.")

(setq gemini-edit-mode-map
      (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-c") #'gemini-edit-mode-confirm)
	    (define-key map (kbd "C-c C-k") #'gemini-edit-mode-cancel)
	    map))

(defun gemini-chat ()
  (interactive)
  (let ((prompt (read-string "Chat with Gemini: ")))
    (if (string-empty-p (string-trim prompt))
	    (message "Please do not enter an empty prompt.")
      (gemini-chat-with-message prompt))))

(defun gemini-chat-with-multiline ()
  (interactive)
  (let* ((bufname (buffer-name))
	     (edit-buffer (generate-new-buffer (format "*gemini-edit-buffer-%s*" bufname))))
    (split-window-below -12)
    (other-window 1)
    (with-current-buffer edit-buffer
      (gemini-edit-mode)
      (set (make-local-variable 'gemini-edit-buffer-name) bufname))
    (switch-to-buffer edit-buffer)
    (gemini--edit-set-header-line)))

(defun gemini--edit-set-header-line ()
  "Set header line."
  (setq header-line-format
        (substitute-command-keys
         (concat
          " Gemini Edit Mode: "
          "Confirm with  C-c C-c, "
          "Cancel with  C-c C-k. "
          ))))

(defun gemini-get-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun gemini-edit-mode-cancel ()
  (interactive)
  (kill-buffer)
  (delete-window)
  (message "[Gemini] Edit cancelled!"))

(defun gemini-edit-mode-confirm ()
  (interactive)
  (let* ((bufname gemini-edit-buffer-name)
	     (prompt (gemini-get-buffer-string)))
    (kill-buffer)
    (delete-window)

    (switch-to-buffer bufname)
    (gemini-chat-with-message prompt)))

(defun gemini-return-code (serial-number content buffer begin end)
  (let* ((block-start (string-match "```" content))
	     (code-begin (+ (string-match "\n" content (+ block-start 3)) 1))
	     (code-end (string-match "```" content code-begin))
	     (code (substring content code-begin code-end)))
    (if (equal serial-number 1)
	    (progn
	      (setq gemini-drafts (list))
	      (push code gemini-drafts)
	      (with-current-buffer buffer
	        (delete-region begin end)
	        (goto-char begin)
	        (setq gemini-draft--begin begin)
	        (insert code)
	        (setq gemini-draft--end (+ gemini-draft--begin (length code)))))
      (push code gemini-drafts))))

(defun gemini-generate-code ()
  (interactive)
  (let* ((selection (if (region-active-p)
                        (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (prompt (if (= (length selection) 0)
                     (format "%s, please only output the code, without any explanations or instructions." (read-string "Prompt: "))
                   (format "%s, please only output the code, without any explanations or instructions." (concat mode " " selection)))))
    (gemini-call-async "gemini_text"
		               prompt
                       (buffer-name)
                       ""
                       "Generating..."
                       "Generate code done."
                       (point)
                       (point)
		               "gemini-return-code")))

(defun gemini-adjust-code ()
  (interactive)
  (let* ((selection (if (region-active-p)
                        (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (prompt (format "%s the %s code below, please only output the code, without any explanations or instructions."
			             (read-string "Adjust: ") mode)))
    (gemini-call-async "gemini_text"
		               prompt
                       (buffer-name)
                       selection
                       "Adgjusting..."
                       "Adjust code done."
                       (region-beginning)
                       (region-end)
		               "gemini-return-code")))

(defun gemini-polish-document ()
  (interactive)
  (let* ((document (if (region-active-p)
		               (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		             (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	     (buffer (generate-new-buffer (format "*gemini-doc-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (gemini-call-async "gemini_text"
		               "Please help me proofread and polish the following text:\n"
		               (buffer-name)
		               document
		               "Polishing..."
		               "Polish document done.")))

(defun gemini-explain-code ()
  (interactive)
  (let* ((code (if (region-active-p)
			       (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		         (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	     (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
	     (buffer (generate-new-buffer (format "*gemini-explain-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (gemini-call-async "gemini_text"
		               (format "Please explain in detail the meaning of the following %s code, in %s, leave a blank line between each sentence:\n" mode (gemini-output-lang))
		               (buffer-name)
		               code
		               "Explaining..."
		               "Explain code done.")))

(defun gemini-comment-code ()
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
    (gemini-call-async "gemini_text"
		               (format "Please add code comments to the following %s code, with the comments written in %s within the code, and output the code including the comments." mode (gemini-output-lang))
		               (buffer-name)
		               code
		               "Commenting..."
		               "Comment code done."
		               begin
		               end
		               "gemini-return-code")))

(defun gemini-refactory-code ()
  (interactive)
  (let* ((code (if (region-active-p)
			       (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		         (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	     (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
	     (buffer (generate-new-buffer (format "*gemini-refactory-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (gemini-call-async "gemini_text"
		               (format "Please help me refactor the following %s code, in %s. Please reply with the refactoring explanation, refactored code, and diff between two versions. Please ignore the comments and strings in the code during the refactoring. If the code remains unchanged after refactoring, please say 'No need to refactor'." mode (gemini-output-lang))
		               (buffer-name)
		               code
		               "Refactorying..."
		               "Refactory code done.")))

(defun gemini-generate-commit-message ()
  (interactive)
  (gemini-call-async "git_commit"
		             (vc-root-dir)
		             (buffer-name)
		             (point)
		             (point)))

(defun gemini-translate-into-chinese ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*gemini-translate-buffer*")))
	     (content (if (region-active-p)
		              (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		            (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (gemini-call-async "gemini_text"
		               (format "请把下面的文段翻译成中文:\n%s" content)
		               (buffer-name)
		               content
		               "Translating..."
		               "Translate text done.")))

(defun gemini-translate-into-english ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*gemini-translate-buffer*")))
	     (content (if (region-active-p)
		              (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
		            (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (gemini-call-async "gemini_text"
		               (format "Please translate the following passage into English:\n%s" content)
		               (buffer-name)
		               content
		               "Translating..."
		               "Translate text done.")))

(defun gemini-choose-drafts (draft)
  (interactive (list (completing-read "Choose Draft: " gemini-drafts nil t)))
  (delete-region gemini-draft--begin gemini-draft--end)
  (save-excursion
    (goto-char gemini-draft--begin)
    (insert draft))
  (setq gemini-draft--end (+ gemini-draft--begin (length draft))))

(unless gemini-is-starting
  (gemini-start-process))


(provide 'gemini)
;;; gemini.el ends here
