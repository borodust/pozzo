;;; pozzo.el --- A minor mode with helpers for Common Lisp Pozzo project -*- lexical-binding: t; -*-

;; This file defines `pozzo-mode`, a buffer-local minor mode that manages
;; a global TCP server that listens for incoming information about available
;; REPL servers.
;;
;; To automatically enable this mode when entering lisp-mode,
;; add the following to your Emacs configuration:
;;
;;   (add-hook 'lisp-mode-hook #'pozzo-mode)
;;
;; The server starts when the first buffer enables the mode and stops when
;; the last buffer disables it.


(defgroup pozzo nil
  "Customization group for pozzo tools."
  :group 'network)

(defcustom pozzo-port 9000
  "The port number for the server to listen on."
  :type 'integer
  :group 'pozzo)

(defvar pozzo--active-buffers 0
  "The number of buffers where `pozzo-mode' is active.")

(defvar pozzo--process nil
  "The current server process.")

(defvar pozzo--repl-info-buffer-displayed-p nil
  "Non-nil if the requests buffer has been displayed during this session.")

(defvar pozzo-repl-info-buffer-name "*pozzo-repl-servers*"
  "The name of the buffer that maintains connection requests.")


(defun pozzo--connect (repl-server port)
  "Connect to the REPL server."
  (cond
   ((eq repl-server :slynk)
    (if (fboundp 'sly-connect)
        (sly-connect "localhost" port)
      (message "Pozzo: Slynk requested but Sly mode is not available")))
   ((eq repl-server :swank)
    (if (fboundp 'slime-connect)
        (slime-connect "localhost" port)
      (message "Pozzo: Swank requested but SLIME is not available")))))


(defun pozzo--delete-repl-info-at-point ()
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position) (1+ (line-end-position)))))


(defun pozzo--delete-repl-info-by-port (port)
  "Delete all lines for the given PORT from the pozzo requests buffer."
  (let ((buf (get-buffer pozzo-repl-info-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (if (eq (get-text-property (point) 'pozzo-port) port)
                  (delete-region (line-beginning-position) (1+ (line-end-position)))
                (forward-line 1)))))))))


(defun pozzo--port-open-p (port)
  "Check if PORT on localhost is open."
  (condition-case nil
      (let ((proc (make-network-process :name "pozzo-port-check"
                                        :host "localhost"
                                        :service port
                                        :nowait nil
                                        :noquery t)))
        (when proc
          (delete-process proc)
          t))
    (error nil)))


(defun pozzo--refresh-repl-info ()
  "Refresh all entries in the pozzo requests buffer, removing those with closed ports."
  (interactive)
  (let ((buf (get-buffer pozzo-repl-info-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let ((port (get-text-property (point) 'pozzo-port)))
                (if (and port (not (pozzo--port-open-p port)))
                    (delete-region (line-beginning-position) (1+ (line-end-position)))
                  (forward-line 1))))))))))


(defun pozzo--get-repl-info-buffer ()
  "Get or create the pozzo buffer with repl servers, ensuring it's in `special-mode`."
  (let ((buf (get-buffer-create pozzo-repl-info-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'special-mode)
        (special-mode)
        (setq buffer-read-only t))
      (let ((inhibit-read-only t))
        (when (= (buffer-size) 0)
          (insert-button "[refresh]"
                         'action (lambda (_) (pozzo--refresh-repl-info))
                         'follow-link t)
          (insert "\n\n"))))
    buf))


(defun pozzo--add-repl-info-to-buffer (repl-server port comment)
  "Add a request to the pozzo requests buffer."
  (let ((buf (pozzo--get-repl-info-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((start (point)))
          (insert (format-time-string "[%H:%M:%S] "))
          (insert (format "%s at %d (%s) " repl-server port comment))
          (insert-button "[delete]"
                         'action (lambda (_) (pozzo--delete-repl-info-at-point))
                         'follow-link t)
          (insert " ")
          (insert-button "[connect]"
                         'action (lambda (_) (pozzo--connect repl-server port))
                         'follow-link t)
          (insert " ")
          (insert-button "[connect and delete]"
                         'action (lambda (_)
                                   (pozzo--connect repl-server port)
                                   (pozzo--delete-repl-info-at-point))
                         'follow-link t)
          (insert "\n")
          (add-text-properties start (point) (list 'pozzo-port port)))))
    (unless pozzo--repl-info-buffer-displayed-p
      (display-buffer buf)
      (setq pozzo--repl-info-buffer-displayed-p t))))


(defun pozzo--remove-repl-info-from-buffer (port)
  "Remove all lines from the buffer if attached port metadata equals PORT."
  (pozzo--delete-repl-info-by-port port))


(defun pozzo--process-add-request (sexp)
  (let ((repl-server (plist-get sexp :repl-server))
        (port (plist-get sexp :port))
        (comment (plist-get sexp :comment)))
    (if (and (memq repl-server '(:slynk :swank))
             (integerp port)
             (stringp comment))
        (progn
          (message "Pozzo received new REPL server: server=%s, port=%d, comment=\"%s\""
                   repl-server port comment)
          (pozzo--add-repl-info-to-buffer repl-server port comment)
          t)
      (message "Pozzo error: Invalid properties in %S (expected :repl-server (:slynk|:swank), :port (int), :comment (string))"
               sexp)
      nil)))


(defun pozzo--process-remove-request (sexp)
  (let ((port (plist-get sexp :port)))
    (if (integerp port)
        (progn
          (message "Pozzo received REPL server removal: port=%d" port)
          (pozzo--remove-repl-info-from-buffer port)
          t)
      (message "Pozzo error: Invalid properties in %S (expected :port (int))" sexp)
      nil)))


(defun pozzo--dispatch-message (msg)
  (condition-case err
      (let ((read-circle nil)) ; Disable circular structures
        (let ((sexp (car (read-from-string msg))))
          (if (and (listp sexp) (keywordp (car sexp)))
              (let ((message-kind (plist-get sexp :kind)))
                (ecase message-kind
                  (:register (pozzo--process-add-request sexp))
                  (:remove (pozzo--process-remove-request sexp))))
            (message "Pozzo error: Received invalid s-expression (expected plist): %S" sexp)
            nil)))
    (error
     (message "Error parsing incoming string: %s" (error-message-string err))
     nil)))


(defun pozzo-filter (proc string)
  "Handle incoming info from the REPL server, parsing into s-expression safely."
  (let ((msg (string-trim string)))
    (unless (string-empty-p msg)
      (if (pozzo--dispatch-message msg)
          (process-send-string proc ":ok\n")
        (process-send-string proc ":fail\n"))
      (delete-process proc))))


(defun pozzo-stop-server ()
  "Stop the server if it's running."
  (interactive)
  (when (processp pozzo--process)
    (delete-process pozzo--process)
    (setq pozzo--process nil)
    (setq pozzo--repl-info-buffer-displayed-p nil)
    (message "Server stopped.")))


(defun pozzo-start-server (&optional port)
  "Start a server listening on PORT.
If PORT is nil, use `pozzo-port`."
  (interactive)
  (pozzo-stop-server)
  (pozzo--get-repl-info-buffer)
  (let ((p (or port pozzo-port)))
    (setq pozzo--process
          (make-network-process
           :name "pozzo"
           :buffer "*pozzo*"
           :family 'ipv4
           :server t
           :host 'local
           :service p
           :filter 'pozzo-filter))))


;;;###autoload
(define-minor-mode pozzo-mode
  "A minor mode that starts a TCP server to listen for REPL servers information.
The server runs on the port specified by `pozzo-port`."
  :lighter " Pozzo"
  :group 'pozzo
  (if pozzo-mode
      (progn
        (setq pozzo--active-buffers (1+ pozzo--active-buffers))
        (unless (processp pozzo--process)
          (pozzo-start-server))
        (add-hook 'kill-buffer-hook #'pozzo--on-kill-buffer nil t))
    (setq pozzo--active-buffers (1- pozzo--active-buffers))
    (when (<= pozzo--active-buffers 0)
      (setq pozzo--active-buffers 0)
      (pozzo-stop-server))
    (remove-hook 'kill-buffer-hook #'pozzo--on-kill-buffer t)))


(defun pozzo--on-kill-buffer ()
  "Handle buffer being killed while `pozzo-mode' is active."
  (when pozzo-mode
    (pozzo-mode -1)))


(provide 'pozzo)
