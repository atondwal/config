;;; nvim-bridge.el --- Bridge Emacs APIs to Neovim -*- lexical-binding: t; -*-

;;; Commentary:
;; This provides a shim layer that intercepts Emacs buffer/window operations
;; and forwards them to Neovim via JSON-RPC over stdio.

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Configuration

(defvar nvim-bridge-debug t
  "Enable debug logging.")

(defvar nvim-bridge-request-id 0
  "Counter for JSON-RPC request IDs.")

(defvar nvim-bridge-pending-requests (make-hash-table :test 'equal)
  "Hash table of pending requests.")

(defvar nvim-bridge-buffer-map (make-hash-table :test 'equal)
  "Map Emacs buffers to Neovim buffer numbers.")

(defvar nvim-bridge-active nil
  "Whether the bridge is active.")

;;; Logging

(defun nvim-bridge--log (msg)
  "Log MSG if debugging is enabled."
  (when nvim-bridge-debug
    (message "[nvim-bridge] %s" msg)))

(defun nvim-bridge--error (msg)
  "Log error MSG."
  (message "[nvim-bridge] ERROR: %s" msg))

;;; JSON-RPC Communication

(defun nvim-bridge--send-request (method params)
  "Send JSON-RPC request with METHOD and PARAMS."
  (setq nvim-bridge-request-id (1+ nvim-bridge-request-id))
  (let ((id nvim-bridge-request-id)
        (request `((jsonrpc . "2.0")
                   (id . ,nvim-bridge-request-id)
                   (method . ,method)
                   (params . ,params))))
    (nvim-bridge--log (format "Sending request: %s" method))
    (princ (json-encode request))
    (princ "\n")
    id))

(defun nvim-bridge--send-notification (method params)
  "Send JSON-RPC notification with METHOD and PARAMS."
  (let ((notification `((jsonrpc . "2.0")
                        (method . ,method)
                        (params . ,params))))
    (princ (json-encode notification))
    (princ "\n")))

(defun nvim-bridge--send-response (id result)
  "Send JSON-RPC response with ID and RESULT."
  (let ((response `((jsonrpc . "2.0")
                    (id . ,id)
                    (result . ,result))))
    (princ (json-encode response))
    (princ "\n")))

(defun nvim-bridge--send-error (id error-message)
  "Send JSON-RPC error response with ID and ERROR-MESSAGE."
  (let ((response `((jsonrpc . "2.0")
                    (id . ,id)
                    (error . ((code . -32603)
                             (message . ,error-message))))))
    (princ (json-encode response))
    (princ "\n")))

;;; RPC Method Handlers (called by Neovim)

(defvar nvim-bridge-handlers (make-hash-table :test 'equal)
  "Hash table of RPC method handlers.")

(defun nvim-bridge-register-handler (method handler)
  "Register HANDLER for METHOD."
  (puthash method handler nvim-bridge-handlers))

(defun nvim-bridge--handle-request (request)
  "Handle incoming REQUEST from Neovim."
  (let* ((method (cdr (assoc 'method request)))
         (params (cdr (assoc 'params request)))
         (id (cdr (assoc 'id request)))
         (handler (gethash method nvim-bridge-handlers)))
    (nvim-bridge--log (format "Received request: %s" method))
    (if handler
        (condition-case err
            (let ((result (funcall handler params)))
              (nvim-bridge--send-response id result))
          (error
           (nvim-bridge--send-error id (error-message-string err))))
      (nvim-bridge--send-error id (format "Unknown method: %s" method)))))

(defun nvim-bridge--handle-response (response)
  "Handle incoming RESPONSE from Neovim."
  (let* ((id (cdr (assoc 'id response)))
         (result (cdr (assoc 'result response)))
         (error (cdr (assoc 'error response))))
    (nvim-bridge--log (format "Received response for id: %s" id))
    ;; Store result for synchronous calls
    (puthash id (cons result error) nvim-bridge-pending-requests)))

;;; Core RPC Handlers

(nvim-bridge-register-handler
 "eval"
 (lambda (params)
   (let* ((code (cdr (assoc 'code params)))
          (result (eval (read code))))
     `((result . ,(format "%S" result))))))

(nvim-bridge-register-handler
 "ping"
 (lambda (params)
   `((status . "ok"))))

;;; Buffer API Shims
;; These override Emacs functions to forward to Neovim

(defun nvim-bridge--get-nvim-buffer (buffer)
  "Get Neovim buffer number for BUFFER."
  (or (gethash buffer nvim-bridge-buffer-map)
      (let ((bufnr (nvim-bridge--call-nvim
                    "nvim_get_current_buffer" nil)))
        (puthash buffer bufnr nvim-bridge-buffer-map)
        bufnr)))

(defun nvim-bridge--call-nvim (method params)
  "Call Neovim METHOD with PARAMS synchronously."
  (let ((id (nvim-bridge--send-request method params)))
    ;; Wait for response (blocking - not ideal but simple)
    ;; In real implementation, use async/callbacks
    (let ((timeout 50)
          (result nil))
      (while (and (> timeout 0)
                  (not (setq result (gethash id nvim-bridge-pending-requests))))
        (sleep-for 0.01)
        (setq timeout (1- timeout)))
      (when result
        (remhash id nvim-bridge-pending-requests)
        (if (cdr result)
            (error "Neovim error: %s" (cdr result))
          (car result))))))

;; Override buffer-string to read from Neovim
(defun nvim-bridge-buffer-string ()
  "Get buffer contents from Neovim."
  (if (not nvim-bridge-active)
      (buffer-string)
    (let* ((response (nvim-bridge--call-nvim
                      "nvim_buf_get_lines"
                      `((buffer . nil)
                        (start . 0)
                        (end . -1))))
           (lines (cdr (assoc 'lines response))))
      (mapconcat 'identity (append lines nil) "\n"))))

;; Override insert to write to Neovim
(defun nvim-bridge-insert (&rest args)
  "Insert ARGS into Neovim buffer."
  (if (not nvim-bridge-active)
      (apply 'insert args)
    (let* ((text (mapconcat 'identity args ""))
           (lines (split-string text "\n")))
      (nvim-bridge--call-nvim
       "nvim_buf_set_lines"
       `((buffer . nil)
         (start . -1)
         (end . -1)
         (lines . ,lines))))))

;;; Message Loop

(defun nvim-bridge--process-message (line)
  "Process a single JSON-RPC LINE."
  (when (> (length line) 0)
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (let ((msg (json-read-from-string line)))
            (cond
             ;; Request from Neovim
             ((and (assoc 'method msg) (assoc 'id msg))
              (nvim-bridge--handle-request msg))
             ;; Response from Neovim
             ((assoc 'id msg)
              (nvim-bridge--handle-response msg))
             ;; Notification
             ((assoc 'method msg)
              (nvim-bridge--log (format "Notification: %s"
                                       (cdr (assoc 'method msg))))))))
      (error
       (nvim-bridge--error (format "Failed to parse JSON: %s" line))))))

(defun nvim-bridge-start-server ()
  "Start the bridge server and enter message loop."
  (nvim-bridge--log "Starting Neovim bridge server...")
  (setq nvim-bridge-active t)

  ;; Disable buffering on stdout/stderr
  (setq buffer-file-coding-system 'utf-8-unix)

  ;; Send ready notification
  (nvim-bridge--send-notification "ready" nil)

  ;; Enter message loop - read from stdin line by line
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((input-buffer "")
          (char nil))
      (while (condition-case nil
                 (setq char (read-char))
               (end-of-file nil))
        ;; Accumulate characters
        (setq input-buffer (concat input-buffer (char-to-string char)))
        ;; Process complete lines
        (when (eq char ?\n)
          (let ((line (substring input-buffer 0 -1))) ; Remove trailing newline
            (nvim-bridge--process-message line)
            (setq input-buffer "")))))))

;;; Dired Integration Example

(defun nvim-bridge-dired (dirname)
  "Run dired on DIRNAME and display in Neovim buffer."
  (interactive "DDired directory: ")
  ;; Create dired buffer
  (let ((dired-buf (dired-noselect dirname)))
    (with-current-buffer dired-buf
      (let ((content (buffer-string)))
        ;; Send to Neovim
        (nvim-bridge--call-nvim
         "nvim_command"
         `((command . "enew")))
        (nvim-bridge--call-nvim
         "nvim_buf_set_name"
         `((buffer . nil)
           (name . ,(format "*dired: %s*" dirname))))
        (let ((lines (split-string content "\n")))
          (nvim-bridge--call-nvim
           "nvim_buf_set_lines"
           `((buffer . nil)
             (start . 0)
             (end . -1)
             (lines . ,lines))))
        (nvim-bridge--call-nvim
         "nvim_echo"
         `((message . ,(format "Dired: %s" dirname))))))))

(provide 'nvim-bridge)
;;; nvim-bridge.el ends here
