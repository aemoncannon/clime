;;; clime.el --- Clang Interaction Mode for Emacs
;;
;;;; License
;;
;;     Copyright (C) 2011 Aemon Cannon
;;
;;     This file includes code from slime.el of the SLIME project
;;     (also licensend under the GNU General Public License.) The
;;     following copyrights therefore apply:
;;
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(eval-and-compile
  (when (<= emacs-major-version 21)
    (error "Clime requires an Emacs version of 21, or above")))

(eval-and-compile
  (require 'cl))

(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'tooltip)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(require 'easymenu)
(require 'auto-complete)
(require 'clime-auto-complete)
(require 'clime-config)

(eval-when (compile)
  (require 'apropos)
  (require 'compile))

(defgroup clime nil
  "Interaction with CLIME."
  :group 'tools)

(defgroup clime-ui nil
  "Interaction with CLIME UI."
  :group 'clime)

(defcustom clime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
  This applies to buffers that present lines as rows of data, such as
  debugger backtraces and apropos listings."
  :type 'boolean
  :group 'clime-ui)

(defcustom clime-kill-without-query-p t
  "If non-nil, kill CLIME processes without query when quitting Emacs."
  :type 'boolean
  :group 'clime-ui)

(defcustom clime-tooltip-hints t
  "If non-nil, mouse tooltips are activated."
  :type 'boolean
  :group 'clime-ui)

(defcustom clime-tooltip-type-hints t
  "If non-nil, type-inspecting tooltips are activated."
  :type 'boolean
  :group 'clime-ui)

(defcustom clime-graphical-tooltips t
  "If non-nil, show graphical bubbles for tooltips."
  :type 'boolean
  :group 'clime-ui)

(defgroup clime-server nil
  "Server configuration."
  :prefix "clime-"
  :group 'clime)

(defcustom clime-connected-hook nil
  "List of functions to call when CLIME connects to Lisp."
  :type 'hook
  :group 'clime-server)

(defcustom clime-default-server-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'clime-server)

(defcustom clime-default-port 9999
  "Port to use as the default for `clime-connect'."
  :type 'integer
  :group 'clime-server)

(defcustom clime-default-server-cmd "bin/server"
  "Command to launch server process."
  :type 'string
  :group 'clime-server)

(defcustom clime-default-server-root
   (file-name-directory
     (directory-file-name
    (file-name-directory
     (directory-file-name
     (file-name-directory
      (directory-file-name
       (file-name-directory
	(locate-library "clime"))))))))
  "Location of CLIME server library."
  :type 'string
  :group 'clime-server)

(defcustom clime-mode-key-prefix [?\C-c]
  "The prefix key for clime-mode commands."
  :group 'clime-mode
  :type 'sexp)

(defvar clime-protocol-version "0.0.1")

(defvar clime-prefer-noninteractive nil
  "State variable used for regression testing.")

(defvar clime-server-buffer-name "*inferior-clime-server*")

(defvar clime-popup-in-other-frame nil)

(defvar clime-ch-fix 1
  "Single character offset to convert between emacs and
 0-based character indexing.")

;;;;; clime-mode

(defgroup clime-mode nil
  "Settings for clime-mode source buffers."
  :prefix "clime-"
  :group 'clime)

(defun clime-source-file-p (&optional filename)
  "Return t if the given filename (or the currently visited file if no
argument is supplied) is source file."
  (let ((file (or filename buffer-file-name)))
    (when file
      (integerp (string-match "\\(?:\\.cc$\\|\\.h$\\|\\.c$\\)" file)))))


(defvar clime-source-buffer-saved-hook nil
  "Hook called whenever an clime source buffer is saved.")

(defun clime-run-after-save-hooks ()
  "Things to run whenever a source buffer is saved."
  (condition-case err-info
      (run-hooks 'clime-source-buffer-saved-hook)
    (error
     (message
      "Error running clime-source-buffer-saved-hook: %s"
      err-info))))

(defun clime-save-buffer-no-hooks ()
  "Just save the buffer per usual, don't type-check!"
  (let ((after-save-hook nil)
        (before-save-hook nil))
    (save-buffer)))

(defun clime-delete-buffer-and-file ()
  "Kill the current buffer and delete the corresponding file!"
  (interactive)
  (clime-assert-buffer-saved-interactive
   (let ((f buffer-file-name))
     (clime-rpc-remove-file f)
     (delete-file f)
     (kill-buffer nil)
     )))


(defun clime-write-buffer (&optional filename clear-modtime set-unmodified)
  "Write the contents of buffer to its buffer-file-name.
Do not show 'Writing..' message."
  (let ((file (or filename buffer-file-name))
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (when clear-modtime
      (clear-visited-file-modtime))
    (write-region (point-min) (point-max) file nil 'nomessage)
    (when set-unmodified
      (set-buffer-modified-p nil))
    ))


(defvar clime-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))

      (define-key prefix-map (kbd "C-v e") 'clime-show-all-errors-and-warnings)
      (define-key prefix-map (kbd "C-v a") 'clime-check-all)
      (define-key prefix-map (kbd "C-v z") 'clime-analyze-all)
      (define-key prefix-map (kbd "C-v s") 'clime-analyze-current-file)

      (define-key map clime-mode-key-prefix prefix-map)

      (define-key map (kbd "M-n") 'clime-forward-note)
      (define-key map (kbd "M-p") 'clime-backward-note)

      (define-key map [C-down-mouse-1] 'ignore)
      (define-key map [C-up-mouse-1] 'ignore)
      (define-key map [C-down-mouse-3] 'ignore)
      (define-key map [C-up-mouse-3] 'ignore)
      (define-key map [C-mouse-1] 'clime-control-mouse-1-single-click)
      (define-key map [C-mouse-3] 'clime-control-mouse-3-single-click)
      )

    map)
  "Keymap for CLIME mode."
  )

(easy-menu-define clime-mode-menu clime-mode-map
  "Menu for CLIME mode"
  '("CLIME"
    ("Build"
     ["Rebuild project" clime-builder-rebuild])
    ["Shutdown CLIME server" clime-shutdown]
    ))

(define-minor-mode clime-mode
  "CLIME: CLang Interaction Mode for Emacs (minor-mode).
\\{clime-mode-map}"
  nil
  nil
  clime-mode-map

  (if clime-mode
      (progn
        (clime-ac-enable)
        (easy-menu-add clime-mode-menu clime-mode-map)
        (add-hook 'after-save-hook 'clime-run-after-save-hooks nil t)
        (add-hook 'clime-source-buffer-saved-hook
                  'clime-check-current-file)
        (when clime-tooltip-hints
          (add-hook 'tooltip-functions 'clime-tooltip-handler)
          (make-local-variable 'track-mouse)
          (setq track-mouse t)
          (make-local-variable 'tooltip-delay)
          (setq tooltip-delay 1.0)
          (define-key clime-mode-map [mouse-movement] 'clime-mouse-motion))

        (clime-refresh-all-note-overlays))
    (progn
      (clime-ac-disable)
      (remove-hook 'after-save-hook 'clime-run-after-save-hooks t)
      (remove-hook 'clime-source-buffer-saved-hook
                   'clime-check-current-file)
      (remove-hook 'tooltip-functions 'clime-tooltip-handler)
      (make-local-variable 'track-mouse)
      (setq track-mouse nil))))

;;;;;; Mouse handlers

(defun clime-control-mouse-1-single-click (event)
  "Command handler for control+clicks of mouse button 1.
   If control is held, jump to definition of symbol under
   point."
  (interactive "e")
  (mouse-set-point event)
  (clime-edit-definition))

(defun clime-control-mouse-3-single-click (event)
  "Command handler for double clicks of mouse button 1.
   If the user clicks on a package declaration or import,
   inspect that package. Otherwise, try to inspect the type
   of the thing at point."
  (interactive "e")
  (clime-inspect-type-at-point))

(defun clime-mouse-motion (event)
  "Command handler for mouse movement events in `clime-mode-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))


;;;;;; Tooltips


(defun clime-tooltip-show-message (msg)
  "Display tooltip, respecting clime tooltip options."
  (if clime-graphical-tooltips
      (tooltip-show msg tooltip-use-echo-area)
    (message msg)))


(defun clime-tooltip-handler (event)
  "Hook function to display a help tooltip. If an error
   or warning overlay exists at point, show the description
   of that error or warning. Otherwise try to inspect the
   type of the expression under the cursor."
  (when (and (eventp event)
             clime-mode
             (clime-current-connection)
             (posn-point (event-end event)))

    (let* ((point (posn-point (event-end event)))
           (ident (tooltip-identifier-from-point point))
           (note-overlays (clime-overlays-at point)))

      (cond

       ;; If error or warning overlays exist,
       ;; show that message..
       (note-overlays (progn
                        (clime-tooltip-show-message
                         (overlay-get (car note-overlays) 'help-echo))
                        t))


       ;; Otherwise show a type hint..
       ((and ident clime-tooltip-type-hints)
        (progn
          (clime-eval-async
           `(swank:type-at-point ,buffer-file-name ,point)
           #'(lambda (type)
               (when type
                 (let ((msg (clime-type-full-name-with-args type)))
                   (clime-tooltip-show-message msg)
                   ))))
          t
          )))
      )))




;;;;;; Modeline

;; Setup the custom clime modeline handler
(add-to-list 'minor-mode-alist
             '(clime-mode (:eval (clime-modeline-string))))

(defun clime-modeline-string ()
  "Return the string to display in the modeline.
  \"CLIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
  (condition-case err
      (let ((conn (clime-current-connection)))
	;; Bail out early in case there's no connection, so we won't
	;; implicitly invoke `clime-connection' which may query the user.
	(cond ((and clime-mode (not conn))
	       " [CLIME: No Connection]")

	      ((and clime-mode (clime-connected-p conn))
	       (concat " "
		       "[CLIME: "
		       (or (plist-get (clime-config conn) :project-name)
			   "Connected")
		       " : "
		       (format "(%s/%s)" (clime-error-count conn) (clime-warning-count conn))
		       " : "
		       (let ((status (clime-modeline-state-string conn)))
			 (cond (status (concat " (" status ")"))
			       (t "")))
		       "]"))
	      (clime-mode " [CLIME: Dead Connection]")
	      ))
    (error (progn
	     (message "Error in modeline update: %s" err)
	     ""
	     ))))




(defun clime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
	 (format "%s" (process-status conn)))
	((let ((pending (length (clime-rex-continuations conn))))
	   (cond ((zerop pending) nil)
		 (t (format "%s" pending)))))))

;; Startup

(defun clime ()
  "Read config file for settings. Then start an inferior
   CLIME server and connect to its Swank server."
  (interactive)
  (when (and (clime-source-file-p) (not clime-mode))
    (clime-mode 1))
  (let* ((config (clime-config-find-and-load)))

    (when (not (null config))
      (let* ((cmd (or (plist-get config :server-cmd)
		      clime-default-server-cmd))
	     (env (plist-get config :server-env))
	     (dir (or (plist-get config :server-root)
		      clime-default-server-root))
	     (buffer clime-server-buffer-name)
	     (args (list (clime-swank-port-file))))

	(clime-delete-swank-port-file 'quiet)
	(let ((server-proc (clime-maybe-start-server cmd args env dir buffer)))
	  (clime-inferior-connect config server-proc)))
      )))


(defun clime-reload ()
  "Re-initialize the project with the current state of the config file.
Analyzer will be restarted. All source will be recompiled."
  (interactive)
  (clime-assert-connected
   (let* ((conn (clime-current-connection))
	  (current-conf (clime-config conn))
	  (config (clime-config-find-and-load
		   (plist-get current-conf :root-dir))))

     (when (not (null config))
       (clime-set-config conn config)
       (clime-init-project conn config)))))

(defun clime-maybe-start-server (program program-args env directory buffer)
  "Return a new or existing inferior server process."
  (cond ((not (comint-check-proc buffer))
	 (clime-start-server program program-args env directory buffer))
	((clime-reinitialize-inferior-server-p program program-args env buffer)
	 (when-let (conn (find (get-buffer-process buffer) clime-net-processes
			       :key #'clime-server-process))
	   (clime-net-close conn))
	 (get-buffer-process buffer))
	(t (clime-start-server program program-args env directory
				(generate-new-buffer-name buffer)))))


(defun clime-reinitialize-inferior-server-p (program program-args env buffer)
  (let ((args (clime-inferior-server-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
	 (equal (plist-get args :program-args) program-args)
	 (equal (plist-get args :env) env)
	 (not (y-or-n-p "Create an additional *inferior-server*? ")))))


(defvar clime-server-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun clime-start-server (program program-args env directory buffer)
  "Does the same as `inferior-server' but less ugly.
   Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
	  (process-connection-type nil))
      (set (make-local-variable 'comint-process-echoes) nil)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)
      (comint-exec (current-buffer) clime-server-buffer-name
		   program nil program-args))
    (let ((proc (get-buffer-process (current-buffer))))
      (clime-set-query-on-exit-flag proc)
      (run-hooks 'clime-server-process-start-hook)
      proc)))


(defun clime-shutdown()
  "Request that the current CLIME server kill itself."
  (interactive)
  (clime-quit-connection (clime-current-connection)))


(defvar clime-inferior-server-args nil
  "A buffer local variable in the inferior proccess.
See `clime-start'.")

(defun clime-inferior-server-args (process)
  "Return the initial process arguments.
   See `clime-start'."
  (with-current-buffer (process-buffer process)
    clime-inferior-server-args))

(defun clime-inferior-connect (config server-proc)
  "Start a Swank server in the inferior Server and connect."
  (clime-read-port-and-connect config server-proc nil))


(defun clime-file-in-directory-p (file-name dir-name)
  "Determine if file named by file-name is contained in the
   directory named by dir-name."
  (let* ((dir (file-name-as-directory (expand-file-name dir-name)))
	 (file (expand-file-name file-name))
	 (d file))
    (catch 'return
      (while d
	(let ((d-original d))
	  (setq d (file-name-directory
		   (directory-file-name d)))
	  (when (equal dir d)
	    (throw 'return t))
	  (when (equal d d-original)
	    (throw 'return nil))
	  )))))

(defun clime-configured-project-root ()
  "Return root path of the current project as defined in the
config file and stored in the current connection. Nil is returned
if there is no active connection, or if the project root was not
defined."
  (when (clime-connected-p)
    (let ((config (clime-config (clime-connection))))
      (plist-get config :root-dir))))

(defmacro clime-assert-connected (&rest body)
  "Surround body forms with a check to see if we're connected.
If not, message the user."
  `(if (clime-connected-p)
       (progn ,@body)
     (message "This command requires a connection to an CLIME server.")))

(defmacro clime-with-conn-interactive (conn-sym &rest body)
  "Surround body forms with a check to see if we're connected.
If not, message the user."
  `(let* ((,conn-sym (or (clime-current-connection)
			 (clime-prompt-for-connection))))
     (if conn
	 (progn ,@body)
       (message
	"This command requires a connection to an CLIME server."))))

(defun clime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (clime-temp-file-name (format "clime_port.%S" (emacs-pid))))

(defun clime-read-swank-port ()
  "Read the Swank server port number from the `clime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (clime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
	(assert (integerp port))
	port))))

(defun clime-temp-file-name (name)
  "Return the path of a temp file with filename 'name'."
  (concat (file-name-as-directory (clime-temp-directory))
	  name))

(defun clime-temp-directory ()
  "Return the directory name of the system's temporary file dump."
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	(t "/tmp/")))

(defmacro* clime-with-buffer-written-to-tmp ((file-sym) &rest body)
  "Write latest buffer state to a temp file, bind the temp filename
 to file-sym, and eval body. The idea is to not disturb the original
 file's state."
  `(let ((,file-sym (clime-temp-file-name
		     (concat ".tmp_" (file-name-nondirectory
				      buffer-file-name)))))
     (clime-write-buffer ,file-sym)
     ,@body))

(defmacro clime-with-temp-file (name)
  "Return the path of a temp file with filename 'name'."
  (concat (file-name-as-directory (clime-temp-directory))
	  name))

(defun clime-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (clime-swank-port-file))
    (error
     (ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
			 (clime-swank-port-file)))))))

(defun clime-read-port-and-connect (config server-proc retries)
  (clime-cancel-connect-retry-timer)
  (clime-attempt-connection config server-proc retries 1))


(defun clime-attempt-connection (config server-proc retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (let ((host (or (plist-get config :server-host) clime-default-server-host))
	(port-file (clime-swank-port-file)))
    (unless (active-minibuffer-window)
      (message "Polling %S.. (Abort with `M-x clime-abort-connection'.)"
	       port-file))
    (cond ((and (file-exists-p port-file)
		(> (nth 7 (file-attributes port-file)) 0)) ; file size
	   (clime-cancel-connect-retry-timer)
	   (let ((port (clime-read-swank-port))
		 (args (clime-inferior-server-args server-proc)))
	     (message "Read port %S from %S." port port-file)
	     (clime-delete-swank-port-file 'message)
	     (let ((c (clime-connect host port)))

	       ;; It may take a few secs to get the
	       ;; source roots back from the server,
	       ;; so we won't know immediately if currently
	       ;; visited source is part of the new
	       ;; project.
	       ;;
	       ;; Make an educated guess for the sake
	       ;; of UI snappiness (fast mode-line
	       ;; update).
	       (when (and (clime-source-file-p)
			  (plist-get config :root-dir)
			  (clime-file-in-directory-p
			   buffer-file-name
			   (plist-get config :root-dir))
			  (not (clime-connected-p)))
		 (setq clime-buffer-connection c))

	       (clime-set-config c config)

	       (let ((clime-dispatching-connection c))
		 (clime-eval-async
		  '(swank:connection-info)
		  (clime-curry #'clime-handle-connection-info c)))

	       (clime-set-server-process c server-proc)
	       ;; As a conveniance, we associate the client connection with
	       ;; the server buffer.
	       ;; This assumes that there's only one client connection
	       ;; per server. So far this is a safe assumption.
	       (when-let (server-buf (process-buffer server-proc))
		 (with-current-buffer server-buf
		   (setq clime-buffer-connection c)))

	       )))
	  ((and retries (zerop retries))
	   (clime-cancel-connect-retry-timer)
	   (message "Gave up connecting to Swank after %d attempts." attempt))
	  ((eq (process-status server-proc) 'exit)
	   (clime-cancel-connect-retry-timer)
	   (message "Failed to connect to Swank: server process exited."))
	  (t
	   (when (and (file-exists-p port-file)
		      (zerop (nth 7 (file-attributes port-file))))
	     (message "(Zero length port file)")
	     ;; the file may be in the filesystem but not yet written
	     (unless retries (setq retries 3)))
	   (unless clime-connect-retry-timer
	     (setq clime-connect-retry-timer
		   (run-with-timer
		    0.3 0.3
		    #'clime-timer-call #'clime-attempt-connection
		    config server-proc (and retries (1- retries))
		    (1+ attempt))))))))

(defvar clime-connect-retry-timer nil
  "Timer object while waiting for the inferior server to start.")

(defun clime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun clime-cancel-connect-retry-timer ()
  (when clime-connect-retry-timer
    (cancel-timer clime-connect-retry-timer)
    (setq clime-connect-retry-timer nil)))

(defun clime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (clime-connect-retry-timer
	 (clime-cancel-connect-retry-timer)
	 (message "Cancelled connection attempt."))
	(t (error "Not connecting"))))


;;;; Framework'ey bits
;;;
;;; This section contains some standard CLIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar


(defun clime-make-code-link (start end file-path line col &optional face)
  "Make an emacs button, from start to end in current buffer,
 linking to file-path and offset."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (find-file-other-window ,file-path)
			  (goto-line ,line)
			  (forward-char ,col)
			  )))

(defun clime-make-code-hyperlink (start end http-path &optional face)
  "Make an emacs button, from start to end in current buffer,
 hyperlinking to http-path."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (browse-url ,http-path)
			  (message "Opening documentation in browser..")
			  )))

(defun clime-http-url-p (s)
  (and (stringp s) (string-match "http://" s)))

(defun clime-insert-link (text file-path &optional offset face)
  "Insert text in current buffer and make it into an emacs
 button, linking to file-path and offset. Intelligently decide
 whether to make a source link or an http link based on the file-path."
  (let ((start (point)))
    (cond
     ((and file-path (clime-http-url-p file-path))
      (progn
	(insert text)
	(clime-make-code-hyperlink start (point) file-path face)))

     ((and file-path (integerp offset))
      (progn
	(insert text)
	(clime-make-code-link start (point) file-path offset face)))

     (t
      (progn
	(insert text))))
    ))


(defun clime-insert-action-link (text action &optional face)
  "Insert text in current buffer and make it into an emacs
 button, linking to file-path and offset."
  (let ((start (point)))
    (insert text)
    (make-button start (point) 'face
		 (or face font-lock-variable-name-face)
		 'action action)))

(defun clime-insert-with-face (text face)
  "Insert text in current buffer and color it
 with face"
  (let ((start (point)))
    (insert text)
    (set-text-properties start (point) `(face ,face))))

(defun clime-flatten-list (list)
  ;;(clime-flatten-list '((a) b c (d e (q) f g)))
  (mapcan (lambda (x)
	    (if (listp x)
		(clime-flatten-list x)
	      (list x))) list))

(defun clime-tokenize-cmd-line (str &optional delim)
  "Interpret a string as a sequence of command-line arguments.
 Break the string at space and tab boundaries, except for double-quoted
 arguments. Returns a list of string tokens.
 "
  ;;(clime-tokenize-cmd-line "")
  ;;(clime-tokenize-cmd-line "abc")
  ;;(clime-tokenize-cmd-line "abc def")
  ;;(clime-tokenize-cmd-line "abc   def")
  ;;(clime-tokenize-cmd-line "abc def -sd")
  ;;(clime-tokenize-cmd-line "abc def -sd \"apple pie\"")
  ;;(clime-tokenize-cmd-line "abc def -sd \"ap'p'le\"")
  ;;(clime-tokenize-cmd-line "abc def -sd 'ap\"pl\"e'")
  ;;(clime-tokenize-cmd-line "'ap\"pl\"e'")
  ;;(clime-tokenize-cmd-line "'ap\"pl\"e")
  ;;(clime-tokenize-cmd-line "abc \"sd")

  (let ((ch)
	(cur "")
	(tokens '()))

    (catch 'return
      (while (> (length str) 0)
	(setq ch (substring str 0 1))
	(setq str (substring str 1))

	(cond
	 ((and delim (equal ch delim))
	  (throw 'return (list tokens str)))

	 ((or (equal ch "\"")
	      (equal ch "'"))
	  (if delim
	      (setq cur (concat cur ch))
	    (let ((tmp (clime-tokenize-cmd-line str ch)))
	      (setq tokens (append tokens (car tmp)))
	      (setq str (cadr tmp)))))

	 ((and (null delim)
	       (integerp (string-match "[ \t]" ch)))
	  (when (> (length cur) 0)
	    (setq tokens (append tokens (list cur)))
	    (setq cur "")))

	 (t (setq cur (concat cur ch))))))

    (when (> (length cur) 0)
      (setq tokens (append tokens (list cur))))
    (list tokens str)
    ))

(defvar clime-qualified-type-regexp
  "^\\(?:object \\)?\\(\\(?:[a-z0-9_]+\\.\\)*\\)\\(?:\\([^\\.]+\\)\\$\\)?\\([^\\.]+\\$?\\)$"
  "Match strings of form pack.pack1.pack2.Types$Type or pack.pack1.pack2.Type")
(defmacro* clime-with-name-parts (str (path outer-type-name name) &rest body)
  "Evaluate BODY with path bound to the dot-separated path of
 this type-name, and name bound to the final type name."
  (let ((tmp (gensym)))
    `(let ((matchedp (integerp (string-match
				clime-qualified-type-regexp
				,str))))
       (let* ((,tmp (if matchedp (match-string 1 ,str) nil))
	      (,path (if (> (length ,tmp) 0)
			 (substring ,tmp 0 (- (length ,tmp) 1)) ,tmp))
	      (,outer-type-name (if matchedp (match-string 2 ,str) nil))
	      (,name (if matchedp (match-string 3 ,str) ,str)))
	 ,@body))))

(defvar clime-qualified-path-and-name-regexp
  "^\\(\\(?:[a-z0-9_]+\\.\\)*\\)\\([^\\.]*\\)$")
(defmacro* clime-with-path-and-name (str (path name) &rest body)
  "Evaluate body with path bound to all sections up to the
 last, concatenated, and name bound to the last section."
  (let ((tmp (gensym)))
    `(let ((matchedp (integerp (string-match
				clime-qualified-path-and-name-regexp
				,str))))
       (let* ((,tmp (if matchedp (match-string 1 ,str) nil))
	      (,path (if (> (length ,tmp) 0)
			 (substring ,tmp 0 (- (length ,tmp) 1)) ,tmp))
	      (,name (if matchedp (match-string 2 ,str) nil)))
	 ,@body))))

(defmacro clime-assert-buffer-saved-interactive (&rest body)
  "Offer to save buffer if buffer is modified. Execute body only if
buffer is saved."
  `(if (buffer-modified-p)
       (if (y-or-n-p "Buffer must be saved to continue. Save now? ")
	   (progn
	     (clime-save-buffer-no-hooks)
	     ,@body))
     (progn
       ,@body)))

(defun clime-assert-executable-on-path (name)
  (when (null (executable-find name))
    (error (concat name " not found on your emacs exec-path. "
		   "See Troubleshooting section of the CLIME manual."))))

(defun clime-kill-txt-props (str)
  "Remove all text-properties from str and return str."
  (set-text-properties 0 (length str) nil str)
  str)


(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))


(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
		     (if (eq (car clause) t)
			 `(t ,@(cdr clause))
		       (destructuring-bind ((op &rest rands) &rest body) clause
			 `(,op (destructuring-bind ,rands ,operands
				 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))


(defmacro clime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))


(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot))
						 ,struct-var)))))
		      slots)
	   . ,body)))))

(defun clime-in-string-or-comment (pos)
  "A helper to determine if the text at point is in a string
   or comment, and therefore should not be considered as part
   of a paren-balancing calculation.

   TODO: Currently this relies on font-lock-mode. Could be
   better."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
	 (or
	  (equal face 'font-lock-doc-face)
	  (equal face 'font-lock-string-face)
	  (equal face 'font-lock-comment-face)))))

(defun clime-replace-keywords (template proplist)
  "Replace keywords in the template list with the associated
 values in the provided proplist."
  (let* ((result '()))
    (dolist (ea template)
      (cond
       ((keywordp ea)
	(setq result (cons (plist-get proplist ea) result)))
       (t
	(setq result (cons ea result)))))
    (reverse result)))

(defun clime-line-col-to-point (file line col)
  "Convert line,column coordinates to a char offset."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-line line)
    (forward-char col)
    (point)))

(defun clime-current-line ()
  "Return the vertical position of point..."
  (1+ (count-lines 1 (point))))

(defun clime-relativise-path (path root)
  "Given a directory named root, and a path f, return f's path
relative to root. If f is not contained by root, return the
absolute path to f."
  (let* ((full-root (directory-file-name (expand-file-name root)))
	 (full-path (expand-file-name path))
	 (index (string-match (concat "^" full-root) full-path)))
    (if (equal index 0)
	(concat "." (substring full-path (length full-root)))
      path)))


(defun clime-revert-visited-files (files &optional check)
  "files is a list of buffer-file-names to revert or lists of the form
 (visited-file-name disk-file-name) where buffer visiting visited-file-name
 will be reverted to the state of disk-file-name."
  (let ((pt (point)))
    (save-excursion
      (dolist (f files)
	(let* ((dest (cond ((stringp f) f)
			   ((listp f) (car f))))
	       (src (cond ((stringp f) f)
			  ((listp f) (cadr f))))
	       (do-visit (equal dest src)))
	  (when-let (buf (find-buffer-visiting dest))
	    (with-current-buffer buf
	      (insert-file-contents src do-visit nil nil t)
	      (when check
		(clime-check-current-file)))
	    ))))
    (goto-char pt)
    ))

(defvar clime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar clime-net-process-close-hooks '()
  "List of functions called when a clime network connection closes.
The functions are called with the process as their argument.")


(defun clime-net-connect (host port)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
	 (proc (open-network-stream "CLIME Server" nil host port))
	 (buffer (clime-make-net-buffer " *clime-connection*")))
    (push proc clime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'clime-net-filter)
    (set-process-sentinel proc 'clime-net-sentinel)
    (clime-set-query-on-exit-flag proc)

    ;; TODO make this smart like slime?
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)

    proc))

(defun clime-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `clime-kill-without-query-p'."
  (when clime-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
		   'set-process-query-on-exit-flag
		 'process-kill-without-query)))
      (funcall fun process nil))))

(defun clime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

(defun clime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC. This is the lowest
 level of communication. The sexp will be read and interpreted
 by the Clime Server."
  (let* ((msg (concat (clime-prin1-to-string sexp) "\n"))
	 (string (concat (clime-net-encode-length (length msg)) msg))
	 (coding-system (cdr (process-coding-system proc))))
    (clime-log-event sexp)
    (process-send-string proc string)))

(defun clime-net-close (process &optional debug)
  (setq clime-net-processes (remove process clime-net-processes))
  (set-process-sentinel process 'ignore)
  (set-process-filter process 'ignore)
  (delete-process process)
  (run-hook-with-args 'clime-net-process-close-hooks process)
  ;; killing the buffer also closes the socket
  (kill-buffer (process-buffer process)))

(defun clime-net-sentinel (process message)
  (message "Server connection closed unexpectedly: %s" message)
  (clime-net-close process))

;;; Socket input is handled by `clime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun clime-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (clime-process-available-input process))

(defun clime-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (and
	    (buffer-live-p (process-buffer process))
	    (clime-net-have-input-p))
      (let ((event (clime-net-read-or-lose process))
	    (ok nil))
	(clime-log-event event)
	(unwind-protect
	    (save-current-buffer
	      (clime-dispatch-event event process)
	      (setq ok t))
	  (unless ok
	    (clime-run-when-idle
	     'clime-process-available-input process)))))))

(defun clime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (clime-net-decode-length))))

(defun clime-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time
	 (if (featurep 'xemacs) itimer-short-interval 0)
	 nil function args))

(defun clime-net-read-or-lose (process)
  (condition-case error
      (clime-net-read)
    (error
     (debug 'error error)
     (clime-net-close process)
     (error "net-read error: %S" error))))

(defun clime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (clime-net-decode-length))
	 (start (+ 6 (point)))
	 (end (+ start length)))
    (assert (plusp length))
    (prog1 (save-restriction
	     (narrow-to-region start end)
	     (read (current-buffer)))
      (delete-region (point-min) end))))

(defun clime-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun clime-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun clime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
	  print-escape-newlines
	  print-length
	  print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))



;;;;; Event logging to *clime-events*
;;;
;;; The *clime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar clime-log-events t
  "*Log protocol events to the *clime-events* buffer.")

(defvar clime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *clime-events*.")

(defvar clime-event-buffer-name "*clime-events*"
  "The name of the clime event buffer.")

(defun clime-log-event (event)
  "Record the fact that EVENT occurred."
  (when clime-log-events
    (with-current-buffer (clime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
	(goto-char (/ (buffer-size) 2))
	(re-search-forward "^(" nil t)
	(delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
	(clime-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
		 outline-minor-mode)
	(hide-entry))
      (goto-char (point-max)))))

(defun clime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun clime-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer clime-event-buffer-name)
      (let ((buffer (get-buffer-create clime-event-buffer-name)))
	(with-current-buffer buffer
	  (buffer-disable-undo)
	  (set (make-local-variable 'outline-regexp) "^(")
	  (set (make-local-variable 'comment-start) ";")
	  (set (make-local-variable 'comment-end) "")
	  (when clime-outline-mode-in-events-buffer
	    (outline-minor-mode)))
	buffer)))



;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->CLIME-Server networking concept.
;;;
;;; Emacs has a connection to each CLIME server process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Servers simultaneously.
;;;
;;; A connection consists of a control socket and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Server process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `clime-dispatching-connection' if dynamically bound, or
;;;   `clime-buffer-connection' if this is set buffer-local,
;;;   or the value of `(clime-owning-connection-for-source-file buffer-file-name)'
;;;   otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `(clime-owning-connection-for-source-file)'.
;;;
;;; When a command creates a new buffer it will set
;;; `clime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `clime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Server handles commands in
;;; clime-mode source buffers, and clime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.


(defmacro clime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `clime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
	(defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
	 (clime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
	 `(clime-with-connection-buffer
	   (,process)
	   (setq (\, (quote (\, real-var))) (\, store))
	   (\, store)))
       '(\, varname))))

(put 'clime-def-connection-var 'lisp-indent-function 2)
(put 'clime-indulge-pretty-colors 'clime-def-connection-var t)

(clime-def-connection-var clime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(clime-def-connection-var clime-server-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(clime-def-connection-var clime-pid nil
  "The process id of the Lisp process.")

(clime-def-connection-var clime-server-implementation-type nil
  "The implementation type of the Lisp process.")

(clime-def-connection-var clime-server-implementation-version nil
  "The implementation type of the Lisp process.")

(clime-def-connection-var clime-server-implementation-name nil
  "The short name for the Lisp implementation.")

(clime-def-connection-var clime-server-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(clime-def-connection-var clime-connection-name nil
  "The short name for connection.")

(clime-def-connection-var clime-server-process nil
  "The inferior process for the connection if any.")

(clime-def-connection-var clime-config nil
  "The project configuration corresponding to this connection.")

(clime-def-connection-var clime-communication-style nil
  "The communication style.")

(clime-def-connection-var clime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

(clime-def-connection-var clime-analyzer-ready nil
  "Whether the analyzer has finished its initial run.")

(clime-def-connection-var clime-compiler-notes nil
  "Warnings, Errors, and other notes produced by the analyzer.")

(clime-def-connection-var clime-error-count 0
  "Current number of errors.")

(clime-def-connection-var clime-warning-count 0
  "Current number of warnings.")

(clime-def-connection-var clime-awaiting-full-check nil
  "Should we show the errors and warnings report on next full-check event?")

(defvar clime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `clime-buffer-connection'.")

(make-variable-buffer-local
 (defvar clime-buffer-connection nil
   "Network connection to use in the current buffer."))


(defvar clime-connection-counter 0
  "The number of CLIME connections made. For generating serial numbers.")

(defun clime-current-connection ()
  "Return the connection to use for Lisp interaction.
 Return nil if there's no connection. Note, there is some loss of
 precision here, as clime-connections-for-source-file might return
 more than one connection. "
  (or clime-dispatching-connection
      clime-buffer-connection
      (clime-owning-connection-for-source-file buffer-file-name)))

(defun clime-connected-p (&optional conn)
  "Return t if clime-current-connection would return non-nil.
 Return nil otherwise."
  (let ((conn (or conn (clime-current-connection))))
    (and conn
	 (buffer-live-p (process-buffer conn)))))


(defun clime-connection ()
  "Return the connection to use for Lisp interaction.
 Signal an error if there's no connection."
  (let ((conn (clime-current-connection)))
    (cond ((not conn)
	   (or (clime-auto-connect)
	       (error "Not connected. M-x clime to connect")))
	  ((not (eq (process-status conn) 'open))
	   (error "Connection closed."))
	  (t conn))))


(defun clime-connections-for-source-file (file)
  "Return the connections corresponding to projects that contain
   the given file in their source trees."
  (when file
    (let ((result '()))
      (dolist (p clime-net-processes)
	(let* ((config (clime-config p))
	       (source-roots (plist-get config :source-roots)))
	  (dolist (dir source-roots)
	    (when (clime-file-in-directory-p file dir)
	      (setq result (cons p result))))))
      result)))


(defun clime-owning-connection-for-source-file (file)
  "Return the connection corresponding to the single
 that owns the given file. "
  (when file
    (catch 'return
      ;; First check individual source-roots
      (dolist (p clime-net-processes)
	(let* ((config (clime-config p))
	       (source-roots (plist-get config :source-roots)))
	  (dolist (dir source-roots)
	    (when (clime-file-in-directory-p file dir)
	      (throw 'return p)))))

      )))


(defun clime-prompt-for-connection ()
  "Prompt the user to select a server connection. Used in situations where
the active connection is ambiguous."
  (let* ((options
	  (mapcar
	   (lambda (p)
	     (let* ((conf (clime-config p))
		    (root (plist-get conf :root-dir))
		    (num (clime-connection-number p)))
	       `(,(format "%s#%s" root num) . ,p)))
	   clime-net-processes))
	 (keys (mapcar (lambda (opt) (car opt)) options)))
    (let ((key (when keys
		 (completing-read
		  (concat "Which project to use? ("
			  (mapconcat #'identity keys ", ")
			  "): ")
		  keys nil t (car keys)))))
      (cdr (assoc key options)))))


;; FIXME: should be called auto-start
(defcustom clime-auto-connect 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after Clime is loaded."
  :group 'clime-mode
  :type '(choice (const never)
		 (const always)
		 (const ask)))

(defun clime-auto-connect ()
  (cond ((or (eq clime-auto-connect 'always)
	     (and (eq clime-auto-connect 'ask)
		  (y-or-n-p "No connection.  Start Clime? ")))
	 (save-window-excursion
	   (clime)
	   (while (not (clime-current-connection))
	     (sleep-for 1))
	   (clime-connection)))
	(t nil)))

(defun clime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((clime-dispatching-connection process))

    ;; Initialize connection state in the process-buffer of PROC."

    ;; To make life simpler for the user: if this is the only open
    ;; connection then reset the connection counter.
    (when (equal clime-net-processes (list process))
      (setq clime-connection-counter 0))

    (clime-with-connection-buffer
     () (setq clime-buffer-connection process))

    (setf (clime-connection-number process)
	  (incf clime-connection-counter))

    process))

(defmacro* clime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `clime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (clime-connection)
			   (error "No connection")))
     ,@body))

(defun clime-connect (host port)
  "Connect to a running Swank server. Return the connection."
  (interactive (list
		(read-from-minibuffer "Host: " clime-default-server-host)
		(read-from-minibuffer "Port: " (format "%d" clime-default-port)
				      nil t)))
  (when (and (interactive-p) clime-net-processes
	     (y-or-n-p "Close old connections first? "))
    (clime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ()
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (clime-net-connect host port))
	   (clime-dispatching-connection process))
      (clime-setup-connection process))))




(defun clime-handle-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (clime-event-sig :connected info)
  (let ((clime-dispatching-connection connection))
    (destructuring-bind (&key pid style server-implementation machine
			      features package version modules
			      &allow-other-keys) info
      (clime-check-version version connection)
      (setf (clime-pid) pid
	    (clime-communication-style) style
	    (clime-server-features) features)
      (destructuring-bind (&key type name version program) server-implementation
	(setf (clime-server-implementation-type) type
	      (clime-server-implementation-version) version
	      (clime-server-implementation-name) name
	      (clime-server-implementation-program) program
	      (clime-connection-name) (clime-generate-connection-name name)))
      (destructuring-bind (&key instance type version) machine
	(setf (clime-machine-instance) instance)))
    (let ((args (when-let (p (clime-server-process))
		  (clime-inferior-server-args p))))
      (when-let (name (plist-get args :name))
	(unless (string= (clime-server-implementation-name) name)
	  (setf (clime-connection-name)
		(clime-generate-connection-name (symbol-name name)))))
      ;; TODO
      ;;(clime-load-contribs)
      (run-hooks 'clime-connected-hook)
      (when-let (fun (plist-get args ':init-function))
	(funcall fun)))

    (message "Connected.")

    ;; Send the project initialization..
    (let ((config (clime-config connection)))
      (clime-init-project connection config))
    ))


(defun clime-init-project (conn config)
  "Send configuration to the server process. Setup handler for
 project info that the server will return."
  (clime-eval-async `(swank:init-project ,config)
		     (clime-curry #'clime-handle-project-info
				   conn)))


(defun clime-handle-project-info (conn info)
  "Handle result of init-project rpc call. Install project information
computed on server into the local config structure."
  (let* ((config (clime-config conn)))
    (setf config (plist-put config :project-name
			    (or
			     (plist-get config :project-name)
			     (plist-get info :project-name)
			     )))
    (setf config (plist-put config :source-roots
			    (plist-get info :source-roots)))
    (clime-set-config conn config)
    (force-mode-line-update t)))


(defun clime-check-version (version conn)
  (or (equal version clime-protocol-version)
      (equal clime-protocol-version 'ignore)
      (y-or-n-p
       (format "Versions differ: %s (clime) vs. %s (swank). Continue? "
	       clime-protocol-version version))
      (clime-net-close conn)
      (top-level)))

(defun clime-generate-connection-name (server-name)
  (loop for i from 1
	for name = server-name then (format "%s<%d>" server-name i)
	while (find name clime-net-processes
		    :key #'clime-connection-name :test #'equal)
	finally (return name)))

(defun clime-connection-close-hook (process)

  ;; TODO should this be per-connection?
  (clime-clear-note-overlays))

(add-hook 'clime-net-process-close-hooks 'clime-connection-close-hook)



;;; `clime-rex' is the RPC primitive which is used to implement both
;;; `clime-eval' and `clime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* clime-rex ((&rest saved-vars)
		       sexp
		       &rest continuations)
  "(clime-rex (VAR ...) SEXP CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort REASON).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
			 collect (etypecase var
				   (symbol (list var var))
				   (cons var)))
       (clime-dispatch-event
	(list :swank-rpc ,sexp
	      (lambda (,result)
		(destructure-case ,result
		  ,@continuations)))))))

(put 'clime-rex 'lisp-indent-function 2)


;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar clime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun clime-eval (sexp)
  "Evaluate EXPR on the superior Lisp and return the result."
  (let* ((tag (gensym (format "clime-result-%d-sym"
			      (1+ (clime-continuation-counter)))))
	 (clime-stack-eval-tags (cons tag clime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (clime-rex (tag sexp)
	   sexp
	 ((:ok value)
	  (unless (member tag clime-stack-eval-tags)
	    (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
		   tag sexp))
	  (throw tag (list #'identity value)))
	 ((:abort code reason)
	  (throw tag (list #'error
			   (format
			    "Synchronous RPC Aborted: %s" reason)))))
       (let ((debug-on-quit t)
	     (inhibit-quit nil)
	     (conn (clime-connection)))
	 (while t
	   (unless (eq (process-status conn) 'open)
	     (error "Lisp connection closed unexpectedly"))
	   (accept-process-output nil 1 0)))))))


(defun clime-eval-async (sexp &optional cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (clime-rex (cont (buffer (current-buffer)))
      sexp
    ((:ok result)
     (when cont
       (if (buffer-live-p buffer)
	   (progn
	     (set-buffer buffer)
	     (funcall cont result))
	 (message
	  "CLIME: Asynchronous return could not find originating buffer.")
	 )))
    ((:abort code reason)
     (message "Asynchronous RPC Aborted: %s" reason)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; clime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :clime-eval-async)

;;;;; Commands on connections

(defun clime-disconnect ()
  "Close the current connection."
  (interactive)
  (clime-net-close (clime-connection)))

(defun clime-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'clime-net-close clime-net-processes))

(defun clime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun clime-process (&optional connection)
  "Return the CLIME server process for CONNECTION
 (default `clime-connection'). Return nil if there's no process
 object for the connection."
  (let ((proc (clime-server-process connection)))
    (if (and proc
	     (memq (process-status proc) '(run stop)))
	proc)))

;; Non-macro version to keep the file byte-compilable.
(defun clime-set-server-process (connection process)
  (setf (clime-server-process connection) process))

(defun clime-set-config (connection config)
  (setf (clime-config connection) config))


;; Commonly used functions

(defun clime-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun clime-rcurry (fun &rest args)
  "Like `clime-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from the CLIME server.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from the CLIME server don't.

(clime-def-connection-var clime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(clime-def-connection-var clime-continuation-counter 0
  "Continuation serial number counter.")

(defvar clime-event-hooks)

(defun clime-dispatch-event (event &optional process)
  (let ((clime-dispatching-connection (or process (clime-connection))))
    (or (run-hook-with-args-until-success 'clime-event-hooks event)
	(destructure-case event
	  ((:swank-rpc form continuation)
	   (let ((id (incf (clime-continuation-counter))))
	     (clime-send `(:swank-rpc ,form ,id))
	     (push (cons id continuation) (clime-rex-continuations))
	     ))
	  ((:return value id)
	   (let ((rec (assq id (clime-rex-continuations))))

	     (cond (rec (setf (clime-rex-continuations)
			      (remove rec (clime-rex-continuations)))
			(funcall (cdr rec) value)
			(force-mode-line-update t)
			(clime-event-sig :return-value value))
		   (t
		    (error "Unexpected reply: %S %S" id value)))))

	  ((:full-check-finished val)
	   (when (clime-awaiting-full-check (clime-connection))
	     (message "Check finished.")
	     (setf (clime-awaiting-full-check
		    (clime-connection)) nil)
	     (clime-show-all-errors-and-warnings))
	   (clime-event-sig :full-check-finished val))

	  ((:compiler-ready status)
	   (message "CLIME ready. %s" (clime-random-words-of-encouragement))
	   (setf (clime-analyzer-ready process) t)
	   (clime-event-sig :compiler-ready status))

	  ((:indexer-ready status)
	   (clime-event-sig :indexer-ready status))

	  ((:notes result)
	   (clime-add-notes result))

	  ((:clear-all-notes result)
	   (clime-clear-notes))

	  ((:clear-file-notes files)
	   (let ((file-set (make-hash-table :test 'equal)))
	     (dolist (file files)
	       (puthash (file-truename file) t file-set))
	     (clime-clear-file-notes file-set)))
	   

	  ((:channel-send id msg)
	   (clime-channel-send (or (clime-find-channel id)
				    (error "Invalid channel id: %S %S" id msg))
				msg))
	  ((:emacs-channel-send id msg)
	   (clime-send `(:emacs-channel-send ,id ,msg)))
	  ((:read-from-minibuffer thread tag prompt initial-value)
	   (clime-read-from-minibuffer-for-swank
	    thread tag prompt initial-value))
	  ((:y-or-n-p thread tag question)
	   (clime-y-or-n-p thread tag question))
	  ((:emacs-return-string thread tag string)
	   (clime-send `(:emacs-return-string ,thread ,tag ,string)))
	  ((:new-features features)
	   (setf (clime-server-features) features))
	  ((:eval-no-wait fun args)
	   (apply (intern fun) args))
	  ((:eval thread tag form-string)
	   (clime-check-eval-in-emacs-enabled)
	   (clime-eval-for-lisp thread tag form-string))
	  ((:emacs-return thread tag value)
	   (clime-send `(:emacs-return ,thread ,tag ,value)))
	  ((:ed what)
	   (clime-ed what))
	  ((:background-message code detail)
	   (clime-background-message "%s" detail))
	  ((:reader-error code detail)
	   (clime-with-popup-buffer
	    ("*Clime Error*")
	    (princ (format "Invalid protocol message:\n%s\n\n%S"
			   condition packet))
	    (goto-char (point-min)))
	   (error "Invalid protocol message"))
	  ))))

(defun clime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (clime-net-send sexp (clime-connection)))


;;; Words of encouragement

(defun clime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
		  (user-login-name)
		(user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar clime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "May the source be with you!"
    "M-x be_cool"
    ,(format "%s, this could be the start of a beautiful program."
	     (clime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun clime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length clime-words-of-encouragement))
	     clime-words-of-encouragement)))


;; Compiler Notes (Error/Warning overlays)

;; Note: This might better be a connection-local variable, but
;; afraid that might lead to hanging overlays..

(defvar clime-note-overlays '()
  "The overlay structures created to highlight notes.")

(defun clime-all-notes ()
  (clime-compiler-notes (clime-connection)))

(defun clime-add-notes (result)
  (let ((is-full (plist-get result :is-full))
	(notes (plist-get result :notes)))
      (setf (clime-compiler-notes (clime-connection))
	    (append (clime-compiler-notes (clime-connection))
	     notes))
    (clime-make-note-overlays notes)
    (clime-update-note-counts)
    ))


(defun clime-update-note-counts ()
  (let* ((con (clime-connection))
	 (notes (clime-compiler-notes con))
	 (error-count 0)
	 (warn-count 0))
    (dolist (note notes)
      (let ((severity (plist-get note :severity)))
	(case severity
	  (error (incf error-count))
	  (warn (incf warn-count))
	  (otherwise t)
	  )))
    (setf (clime-error-count con) error-count)  
    (setf (clime-warning-count con) warn-count)))


(defun clime-clear-notes ()
  (setf (clime-compiler-notes (clime-connection)) nil)
  (clime-clear-note-overlays)
  (clime-update-note-counts))


(defun clime-clear-file-notes (file-set)
    (let* ((con (clime-connection))
	   (notes (clime-compiler-notes con))
	   (error-count 0)
	   (warn-count 0)
	   (revised '()))
      (dolist (note notes)
	(let ((f (plist-get note :file)))
	  (when (not (gethash (file-truename f) file-set))
	    (setq revised (cons note revised)))))
      (setf (clime-compiler-notes con) (reverse revised)))
    (clime-clear-note-overlays file-set)
    (clime-update-note-counts))


(defun clime-make-overlay-at (file line col msg face)
  "Create an overlay highlighting the given line in any
 buffer visiting the given file."
    (when-let (buf (find-buffer-visiting file))
      ;; If line provided, use line to define region
      (when (integerp line)
	(with-current-buffer buf
	  (save-excursion
	    (goto-line line)
	    (clime-make-overlay (point-at-bol) (point-at-eol) msg face nil buf))))
      ))


(defun clime-make-note-overlays (notes)
  (dolist (note notes)
    (destructuring-bind
	(&key severity msg beg end line col file &allow-other-keys) note

      (let ((face
	     (cond
	      ((equal severity 'error)
	       'clime-errline-highlight)
	      (t
	       'clime-warnline-highlight))))

	(when-let (ov (clime-make-overlay-at
		       file line col
		       msg face))

	  (push ov clime-note-overlays))

	))))


(defun clime-refresh-all-note-overlays ()
  (let ((notes (if (clime-connected-p)
		    (clime-compiler-notes (clime-current-connection)))))
    (clime-clear-note-overlays)
    (clime-make-note-overlays notes)
    ))


(defface clime-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking the line on which an error occurs."
  :group 'clime-ui)

(defface clime-errline-highlight
  '((((class color) (background dark)) (:background "Firebrick3"))
    (((class color) (background light)) (:background "HotPink"))
    (t (:bold t)))
  "Face used for marking the specific region of an error, if available."
  :group 'clime-ui)

(defface clime-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking the line on which an warning occurs."
  :group 'clime-ui)

(defface clime-warnline-highlight
  '((((class color) (background dark)) (:background "dark slate blue"))
    (((class color) (background light)) (:background "DeepSkyBlue1"))
    (t (:bold t)))
  "Face used for marking the specific region of an warning, if available."
  :group 'clime-ui)


(defun clime-make-overlay (beg end tooltip-text face &optional mouse-face buf)
  "Allocate a clime overlay in range BEG and END."
  (let ((ov (make-overlay beg end buf t t)))
    (overlay-put ov 'face           face)
    (overlay-put ov 'mouse-face     mouse-face)
    (overlay-put ov 'help-echo      tooltip-text)
    (overlay-put ov 'clime-overlay  t)
    (overlay-put ov 'priority 100)
    ov)
  )

(defun clime-overlays-at (point)
  "Return list of overlays of type 'clime-overlay at point."
  (let ((ovs (overlays-at point)))
    (remove-if-not
     (lambda (ov) (overlay-get ov 'clime-overlay))
     ovs)
    ))

(defun clime-clear-note-overlays (&optional file-set)
  "Delete note overlays language. If lang is nil, delete all
 overlays."
  (let ((revised '()))
    (dolist (ov clime-note-overlays)
      (if (or (null file-set)
	      (gethash
	       (file-truename 
		(buffer-file-name (overlay-buffer ov)))
	       file-set
	       ))
	  (delete-overlay ov)
	(setq revised (cons ov revised))))
    (setq clime-note-overlays revised)))

(defun clime-next-note-in-current-buffer (notes forward)
  (let ((best-note nil)
	(best-dist most-positive-fixnum)
	(cur-line (line-number-at-pos (point)))
	(max-line (line-number-at-pos (point-max))))
    (dolist (note notes)
      (if (and (clime-files-equal-p (clime-note-file note)
				     buffer-file-name)
	       (/= (clime-note-line note) cur-line))
	  (let ((dist (cond
		       (forward
			(if (< (clime-note-line note) cur-line)
			    (+ (clime-note-line note)
			       (- max-line cur-line))
			  (- (clime-note-line note) cur-line)))

		       (t (if (> (clime-note-line note) cur-line)
			      (+ cur-line (- max-line
					    (clime-note-line note)))
			    (- cur-line (clime-note-line note)))))))

	    (when (< dist best-dist)
	      (setq best-dist dist)
	      (setq best-note note))
	    )))
    best-note))

(defun clime-goto-next-note (forward)
  "Helper to move point to next note. Go forward if forward is non-nil."
  (let* ((conn (clime-current-connection))
	 (notes (clime-compiler-notes conn))
	 (next-note (clime-next-note-in-current-buffer notes forward)))
    (if next-note
	(progn
	  (goto-line (clime-note-line next-note))
	  (message (clime-note-message next-note)))
      (message (concat
		"No more compilation issues in this buffer. "
		"Use clime-check-all [C-c C-v a] to find"
		" all issues, project-wide.")))))

(defun clime-forward-note ()
  "Goto the next compilation note in this buffer"
  (interactive)
  (clime-goto-next-note t))

(defun clime-backward-note ()
  "Goto the prev compilation note in this buffer"
  (interactive)
  (clime-goto-next-note nil))

(defun clime-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))

(defun clime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(defun clime-edit-definition-other-window ()
  (interactive)
  (clime-edit-definition 'window))

(defun clime-edit-definition-other-frame ()
  (interactive)
  (clime-edit-definition 'frame))

(defun clime-edit-definition (&optional where)
  "Lookup the definition of the name at point."
  (interactive)

  (let* ((info (clime-rpc-symbol-at-point))
	 (pos (clime-symbol-decl-pos info))
	 (offset (clime-pos-offset pos))
	 (type (clime-symbol-type info)))
    (cond
     ((clime-pos-valid-local-p pos)
      (progn
	(clime-push-definition-stack)
	(clime-goto-source-location pos where)))

     (type
      (let ((info (clime-rpc-inspect-type-by-id (clime-type-id type))))
	(if info
	    (progn
	      (clime-push-definition-stack)
	      (clime-type-inspector-show info))
	  (message "Sorry, no definition found."))))

     (t
      (message "Sorry, no definition found.")))))


(defun clime-files-equal-p (f1 f2)
  "Return t if file-names refer to same file."
  (equal (file-truename f1) (file-truename f2)))


(defun clime-goto-source-location (pos &optional where)
  "Move to the source location POS. Don't open
 a new window or buffer if file is open and visible already."
  (let* ((file (clime-pos-file pos))
	 (file-visible-buf
	  (catch 'result
	    (dolist (w (window-list))
	      (let* ((buf (window-buffer w))
		     (window-file (buffer-file-name buf)))
		(when (and window-file
			   (clime-files-equal-p file window-file))
		  (throw 'result buf)))))))

    (when (not file-visible-buf)
      (ecase where
	((nil)
	 (find-file file))
	(window
	 (find-file-other-window file)))
      (setq file-visible-buf (current-buffer)))

    (with-current-buffer file-visible-buf
      (if (> (clime-pos-line pos) 0)
	  (goto-line (clime-pos-line pos))
	(if (> (clime-pos-offset pos) 0)
	    (goto-char (+ (clime-pos-offset pos) clime-ch-fix)))))))


;; Compilation result interface

(defvar clime-compile-result-buffer-name "*CLIME-Compilation-Result*")

(defvar clime-compile-result-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda()(interactive)
				(clime-popup-buffer-quit-function)
				))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    map)
  "Key bindings for the build result popup.")

(defface clime-compile-errline
  '((((class color) (background dark)) (:foreground "#ff5555"))
    (((class color) (background light)) (:foreground "Firebrick4"))
    (t (:bold t)))
  "Face used for marking the line on which an error occurs."
  :group 'clime-ui)

(defface clime-compile-warnline
  '((((class color) (background dark)) (:foreground "LightBlue2"))
    (((class color) (background light)) (:foreground "DarkBlue"))
    (t (:bold t)))
  "Face used for marking the line on which an warning occurs."
  :group 'clime-ui)

(defun clime-show-compile-result-buffer (notes-in)
  "Show a popup listing the results of the last build."

  (clime-with-popup-buffer
   (clime-compile-result-buffer-name t t)
   (use-local-map clime-compile-result-map)
   (clime-insert-with-face
    "Latest Compilation Results (q to quit, TAB to jump to next error)"
    'font-lock-constant-face)
   (clime-insert-with-face
    "\n----------------------------------------\n\n"
    'font-lock-comment-face)
   (if (null notes-in)
       (insert "0 errors, 0 warnings.")
     (save-excursion
       ;; Output the notes
       (dolist (note notes)
	 (destructuring-bind
	     (&key severity msg beg
		   end line col file &allow-other-keys) note


	   (let ((face (case severity
			 (error 'clime-compile-errline)
			 (warn 'clime-compile-warnline)
			 (info 'clime-compile-warnline)
			 (otherwise font-lock-comment-face)
			 ))
		 (header (case severity
			   (error "ERROR")
			   (warn "WARNING")
			   (info "INFO")
			   (otherwise "MISC")
			   ))
		 )

	      (clime-insert-with-face
			      (concat "\n" file "\n")
			      face)
	      (let ((p (point)))
		(insert (format "%s: %s : line %s"
				header msg line))
		(clime-make-code-link p (point)
				      file
				      line
				      col
				      face))
	     (insert "\n\n")
	     )))
       (insert "\n\n"))
     (forward-button 1)
     )
   ))


;; Compilation on request

(defun clime-check-current-file ()
  "Send a request for re-check of current file to all CLIME servers
 managing projects that contains the current file. File is saved
 first if it has unwritten modifications."
  (interactive)
  (when (buffer-modified-p) (clime-write-buffer nil t))
  (clime-rpc-async-check-file
   buffer-file-name 'identity))

(defun clime-analyze-current-file ()
  "Send a request for re-check of current file to all CLIME servers
 managing projects that contains the current file. File is saved
 first if it has unwritten modifications."
  (interactive)
  (when (buffer-modified-p) (clime-write-buffer nil t))
  (clime-rpc-async-analyze-file
   buffer-file-name 'identity))

(defun clime-check-all ()
  "Send a request for re-check of whole project to the CLIME server.
   Current file is saved if it has unwritten modifications."
  (interactive)
  (message "Checking entire project...")
  (if (buffer-modified-p) (clime-write-buffer nil t))
  (setf (clime-awaiting-full-check (clime-connection)) t)
  (clime-rpc-async-check-all 'identity))

(defun clime-analyze-all ()
  "Send a request for re-check of whole project to the CLIME server.
   Current file is saved if it has unwritten modifications."
  (interactive)
  (message "Analyzing entire project...")
  (if (buffer-modified-p) (clime-write-buffer nil t))
  (setf (clime-awaiting-full-check (clime-connection)) t)
  (clime-rpc-async-analyze-all 'identity))


(defun clime-show-all-errors-and-warnings ()
  "Show a summary of all compilation notes."
  (interactive)
  (let ((notes (clime-compiler-notes (clime-connection))))
    (clime-show-compile-result-buffer
     notes)))


(defun clime-sym-at-point ()
  "Return information about the symbol at point. If not looking at a
 symbol, return nil."
  (let ((start nil)
	(end nil))

    (when (thing-at-point 'symbol)

      (save-excursion
	(search-backward-regexp "\\W" nil t)
	(setq start (+ (point) 1)))
      (save-excursion
	(search-forward-regexp "\\W" nil t)
	(setq end (- (point) 1)))
      (list :start start
	    :end end
	    :name (buffer-substring-no-properties start end)))))

(defun clime-completions-at-point (&optional prefix)
  (if (buffer-modified-p) (clime-write-buffer nil t))
  (let ((file (buffer-file-name))
	(col (1+ (- (point) (point-at-bol))))
	(line (count-lines (point) (point-min))))
    (clime-rpc-completions
     file line col (or prefix ""))))

;; Source Formatting

(defun clime-format-source ()
  "Format the source in the current buffer using the Scalariform
 formatting library."
  (interactive)
  (clime-with-buffer-written-to-tmp
   (file)
   (message "Formatting...")
   (clime-rpc-async-format-files
    (list file)
    `(lambda (result)
       (clime-revert-visited-files (list (list ,buffer-file-name ,file)) t)
       ))))


;; RPC Helpers

(defun clime-debug-unit-info-at-point ()
  (interactive)
  (clime-rpc-debug-unit-info (file-name-nondirectory buffer-file-name)
			      (line-number-at-pos (point))
			      ""))

;; Basic RPC calls


(defun clime-rpc-symbol-at-point ()
  (clime-eval
   `(swank:symbol-at-point ,buffer-file-name ,(clime-computed-point))))

(defun clime-rpc-repl-config ()
  "Get the configuration information needed to launch the scala interpreter
with the current project's dependencies loaded. Returns a property list."
  (clime-eval
   `(swank:repl-config)))

(defun clime-rpc-debug-config ()
  "Get the configuration information needed to launch the debugger
with the current project's dependencies loaded. Returns a property list."
  (clime-eval
   `(swank:debug-config)))

(defun clime-rpc-debug-unit-info (file-name-no-path
				   line-number &optional package-prefix)
  "Get descriptive info for the compilation unit defined at
 file-name/line-number."
  (clime-eval
   `(swank:debug-unit-info ,file-name-no-path
			   ,line-number
			   ,(or package-prefix ""))))

(defun clime-rpc-debug-class-locs-to-source-locs (locs)
  "Get source locations corresponding to class,line pairs."
  (clime-eval
   `(swank:debug-class-locs-to-source-locs ,locs)))

(defun clime-rpc-remove-file (file-name)
  (clime-eval `(swank:remove-file ,file-name)))

(defun clime-rpc-async-check-file (file-name continue)
  (clime-eval-async `(swank:check-file ,file-name) continue))

(defun clime-rpc-async-check-all (continue)
  (clime-eval-async `(swank:check-all) continue))

(defun clime-rpc-async-analyze-all (continue)
  (clime-eval-async `(swank:analyze-all) continue))

(defun clime-rpc-async-analyze-file (file-name continue)
  (clime-eval-async `(swank:analyze-file ,file-name) continue))

(defun clime-rpc-async-completions (file-name line col prefix continue)
  (clime-eval-async `(swank:completions ,file-name ,line ,col ,prefix) continue))

(defun clime-rpc-completions (file-name line col prefix)
  (clime-eval `(swank:completions ,file-name ,line ,col ,prefix)))

(defun clime-rpc-include-completions (file-name prefix)
  (clime-eval `(swank:include-completions ,file-name ,prefix)))

(defun clime-rpc-async-format-files (file-names continue)
  (clime-eval-async `(swank:format-source ,file-names) continue))


(defun clime-rpc-name-completions-at-point (&optional prefix is-constructor)
  (clime-eval
   `(swank:scope-completion
     ,buffer-file-name
     ,(clime-computed-point)
     ,(or prefix "")
     ,is-constructor)))

(defun clime-rpc-import-suggestions-at-point (names max-results)
  (clime-eval
   `(swank:import-suggestions
     ,buffer-file-name
     ,(clime-computed-point)
     ,names
     ,max-results
     )))

(defun clime-rpc-async-public-symbol-search
  (names max-results continue)
  (clime-eval-async
   `(swank:public-symbol-search
     ,names
     ,max-results
     ) continue))

(defun clime-rpc-uses-of-symbol-at-point ()
  (clime-eval
   `(swank:uses-of-symbol-at-point
     ,buffer-file-name
     ,(clime-computed-point)
     )))

(defun clime-rpc-members-for-type-at-point (&optional prefix)
  (clime-eval
   `(swank:type-completion
     ,buffer-file-name
     ,(clime-computed-point)
     ,(or prefix ""))))

(defun clime-rpc-package-member-completions (path &optional prefix)
  (clime-eval
   `(swank:package-member-completion ,path ,(or prefix ""))))

(defun clime-rpc-get-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (clime-eval
       `(swank:type-by-id ,id))))

(defun clime-rpc-get-type-by-name (name)
  (clime-eval
   `(swank:type-by-name ,name)))

(defun clime-rpc-get-type-by-name-at-point (name)
  (clime-eval
   `(swank:type-by-name-at-point
     ,name ,buffer-file-name ,(clime-computed-point))))

(defun clime-rpc-get-type-at-point ()
  (clime-eval
   `(swank:type-at-point ,buffer-file-name ,(clime-computed-point))))

(defun clime-rpc-inspect-type-at-point ()
  (clime-eval
   `(swank:inspect-type-at-point ,buffer-file-name ,(clime-computed-point))))

(defun clime-rpc-inspect-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (clime-eval
       `(swank:inspect-type-by-id ,id))))

(defun clime-rpc-inspect-package-by-path (path)
  (clime-eval
   `(swank:inspect-package-by-path ,path)))

(defun clime-rpc-get-call-completion (id)
  (if (and (integerp id) (> id -1))
      (clime-eval
       `(swank:call-completion ,id))))

(defun clime-rpc-peek-undo ()
  (clime-eval
   `(swank:peek-undo)))

(defun clime-rpc-exec-undo (id)
  (clime-eval
   `(swank:exec-undo ,id)))

(defun clime-rpc-refactor-perform
  (proc-id refactor-type params non-interactive continue blocking)
  (if blocking
      (clime-eval
       `(swank:perform-refactor
	 ,proc-id ,refactor-type ,params ,(not non-interactive)))
    (clime-eval-async
     `(swank:perform-refactor
       ,proc-id ,refactor-type ,params ,(not non-interactive)) continue)))

(defun clime-rpc-refactor-exec (proc-id refactor-type continue)
  (clime-eval-async `(swank:exec-refactor ,proc-id , refactor-type) continue))

(defun clime-rpc-refactor-cancel (proc-id)
  (clime-eval-async `(swank:cancel-refactor ,proc-id) #'identity))

(defun clime-rpc-shutdown-server ()
  (clime-eval `(swank:shutdown-server)))



;; Uses UI

(defvar clime-uses-buffer-name "*Uses*")

(defvar clime-uses-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "q") 'clime-popup-buffer-quit-function)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    map)
  "Key bindings for the uses popup.")

(defun clime-show-uses-of-symbol-at-point ()
  "Display a hyperlinked list of the source locations
 where the symbol under point is referenced."
  (interactive)
  (let ((uses (clime-rpc-uses-of-symbol-at-point)))
    (clime-with-popup-buffer
     (clime-uses-buffer-name t t)
     (use-local-map clime-uses-buffer-map)


     (clime-insert-with-face
      "TAB to advance to next use, q to quit"
      'font-lock-constant-face)
     (insert "\n\n\n")

     (dolist (pos uses)
       (let* ((file (clime-pos-file pos))

	      (range-start (- (clime-pos-offset pos) 80))
	      (range-end (+ (clime-pos-offset pos) 80))
	      (result (clime-extract-file-chunk
		       file range-start range-end))
	      (chunk-text (plist-get result :text))
	      (chunk-start (plist-get result :chunk-start))
	      (chunk-start-line (plist-get result :chunk-start-line)))

	 (clime-insert-with-face file 'font-lock-comment-face)
	 (clime-insert-with-face
	  (format "\n------------------- @line %s -----------------------\n"
		  chunk-start-line)
	  'font-lock-comment-face)

	 (let ((p (point)))

	   ;; Insert the summary chunk
	   (insert chunk-text)

	   ;; Highlight the occurances
	   (let* ((from (+ (plist-get pos :start) clime-ch-fix))
		  (to (+ (plist-get pos :end) clime-ch-fix))
		  (len (- to from))
		  (buffer-from (+ p (- from chunk-start)))
		  (buffer-to (+ p (- to chunk-start))))
	     (clime-make-code-link
	      buffer-from buffer-to file from)))

	 (insert "\n\n\n")
	 ))
     (goto-char (point-min))
     (when uses (forward-button 1))
     )
    (clime-event-sig :references-buffer-shown)
    ))

;; Type Inspector UI

(defvar clime-inspector-buffer-name "*Inspector*")

(defvar clime-indent-level 0
  "In inspector UI, how much to indent.")

(defun clime-inspector-buffer-p (buffer)
  "Is this an clime inspector buffer?"
  (eq (get-buffer clime-inspector-buffer-name) buffer))

(defun clime-popup-buffer-p (buffer)
  "Is this an clime popup buffer?"
  (with-current-buffer buffer
    clime-is-popup-buffer))

(defun clime-inspector-insert-linked-package-path (path &optional face)
  "For each component of the package path, insert a link to inspect
   that package."
  (let ((pieces (split-string path "\\."))
	(accum ""))
    (dolist (piece pieces)
      (setq accum (concat accum piece))
      (clime-insert-action-link
       piece
       `(lambda (x)
	  (clime-inspect-package-by-path ,accum))
       (or face font-lock-type-face))
      (insert ".")
      (setq accum (concat accum "."))
      )))


(defun clime-inspector-insert-link-to-type-id (text type-id &optional is-obj)
  "A helper for type link insertion. See usage in
 clime-inspector-insert-linked-type. If is-obj is
 non-nil, use an alternative color for the link."
  (clime-insert-action-link
   text
   `(lambda (x)
      (clime-type-inspector-show
       (clime-rpc-inspect-type-by-id ,type-id)
       ))
   (if is-obj
       font-lock-constant-face
     font-lock-type-face)
   ))

(defun clime-inspector-insert-linked-type
  (type &optional with-doc-link qualified)
  "Helper utility to output a link to a type.
 Should only be invoked by clime-inspect-type-at-point"
  (if (clime-type-is-arrow-p type)
      (clime-inspector-insert-linked-arrow-type type with-doc-link qualified)

    (let* ((type-args (clime-type-type-args type))
	   (last-type-arg (car (last type-args)))
	   (is-obj (clime-type-is-object-p type)))

      (insert (make-string clime-indent-level ?\s))

      (if qualified
	  (clime-with-name-parts
	   (clime-type-full-name type)
	   (path outer-type-name name)
	   (when path
	     (clime-inspector-insert-linked-package-path path))
	   (if (and outer-type-name (integerp (clime-outer-type-id type)))
	       (progn
		 (clime-inspector-insert-link-to-type-id
		  outer-type-name (clime-outer-type-id type))
		 (insert "$")
		 (clime-inspector-insert-link-to-type-id
		  name (clime-type-id type) is-obj))
	     (progn
	       (clime-inspector-insert-link-to-type-id
		name (clime-type-id type) is-obj))))

	;; Otherwise, insert short name..
	(clime-inspector-insert-link-to-type-id
	 (clime-type-name type) (clime-type-id type) is-obj))

      (when type-args
	(let ((clime-indent-level 0))
	  (insert "[")
	  (dolist (tpe type-args)
	    (clime-inspector-insert-linked-type tpe nil nil)
	    (if (not (eq tpe last-type-arg))
		(insert ", ")))
	  (insert "]")))

      (when with-doc-link
	(let* ((pos (plist-get type :pos))
	       (url (or (clime-pos-file pos)
			(clime-make-doc-url type)
			)))
	  (clime-insert-link " doc" url
			      (+ (clime-pos-offset pos)
				 clime-ch-fix))))

      )))

(defun clime-inspector-insert-linked-arrow-type
  (type  &optional with-doc-link qualified)
  "Helper utility to output a link to a type.
   Should only be invoked by clime-inspect-type-at-point"
  (let*  ((param-sections (clime-type-param-sections type))
	  (result-type (clime-type-result-type type)))
    (dolist (sect param-sections)
      (let ((params (plist-get sect :params)))
	(insert "(")
	(let ((last-param (car (last params))))
	  (dolist (p params)
	    (let ((tpe (cadr p)))
	      (clime-inspector-insert-linked-type tpe nil qualified)
	      (if (not (eq p last-param))
		  (insert ", "))))
	  (insert ") => "))))
    (clime-inspector-insert-linked-type result-type nil qualified)
    ))


(defun clime-inspector-insert-linked-member (owner-type m)
  "Helper utility to output a link to a type member.
   Should only be invoked by clime-inspect-type-at-point"
  (let* ((type (clime-member-type m))
	 (pos (clime-member-pos m))
	 (member-name (clime-member-name m))
	 (url (or (clime-pos-file pos)
		  (clime-make-doc-url owner-type m)
		  )))

    (if (or (equal 'method (clime-declared-as m))
	    (equal 'field (clime-declared-as m)))
	(progn
	  (clime-insert-link
	   (format "%s" member-name) url
	   (+ (clime-pos-offset pos) clime-ch-fix)
	   font-lock-function-name-face)
	  (tab-to-tab-stop)
	  (clime-inspector-insert-linked-type type nil nil))

      ;; otherwise, assume it's a nested type
      (progn
	(clime-insert-with-face
	 (clime-declared-as-str m)
	 'font-lock-comment-face)
	(tab-to-tab-stop)
	(clime-inspector-insert-linked-type type nil nil)
	))
    ))


(defun clime-inspect-type-at-point-other-frame ()
  "See clime-inspect-type-at-point, but in other frame."
  (interactive)
  (let ((clime-popup-in-other-frame t))
    (clime-inspect-type-at-point)))

(defun clime-type-inspect-info-at-point ()
  "Helper to pull the inspect info for object at point."
  (let* ((imported-type-path (clime-imported-type-path-at-point))
	 (imported-type (when imported-type-path
			  (clime-rpc-get-type-by-name-at-point
			   imported-type-path))))
    (if imported-type
	;; if imported type under point
	(clime-rpc-inspect-type-by-id
	 (clime-type-id imported-type))
      ;; otherwise do normal type inspection
      (clime-rpc-inspect-type-at-point))))

(defun clime-inspect-java-type-at-point ()
  "Use the global index to search for type at point.
 Inspect the type selected by user."
  (let* ((sym (clime-sym-at-point))
	 (name (plist-get sym :name))
	 (name-start (plist-get sym :start))
	 (name-end (plist-get sym :end))
	 (suggestions (clime-rpc-import-suggestions-at-point (list name) 10)))
    (when suggestions
      (let* ((names (mapcar
		     (lambda (s)
		       (propertize (plist-get s :name)
				   'local-name
				   (plist-get s :local-name)))
		     (apply 'append suggestions)))
	     (selected-name
	      (popup-menu*
	       names :point (point))))
	(when selected-name
	  (clime-inspect-by-path
	   (clime-kill-txt-props selected-name))
	  )))))

(defun clime-inspect-type-at-point ()
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (interactive)
  (let ((pack-path (clime-package-path-at-point)))

    (cond ((clime-visiting-java-file-p)
	   (clime-inspect-java-type-at-point))

	  (t ;; inspect package if package under point
	   (if pack-path (clime-inspect-package-by-path pack-path)
	     ;; otherwise, inspect type
	     (let* ((inspect-info (clime-type-inspect-info-at-point)))
	       (clime-type-inspector-show inspect-info)))))))

(defun clime-type-inspector-show (info &optional focus-on-member)
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (if (null info)
      (message "Cannot inspect nil type.")
    (let* ((interfaces (plist-get info :interfaces))
	   (type (plist-get info :type))
	   (companion-id (plist-get info :companion-id))
	   (buffer-name clime-inspector-buffer-name)
	   (clime-indent-level 0)
	   (focus-point nil))
      (clime-with-inspector-buffer
       (buffer-name info t)

       ;; We want two main columns. The first, 20 chars wide.
       (let ((tab-stop-list '(20)))
	 (setq wrap-prefix (make-string 21 ?\s))

	 ;; Display main type
	 (let* ((full-type-name (plist-get type :name)))
	   (clime-insert-with-face (format "%s\n"
					    (clime-declared-as-str type))
				    font-lock-comment-face)
	   (clime-inspector-insert-linked-type type t t)
	   (insert "\n")

	   ;; Insert a link to the companion object or class, if extant
	   (when-let (id companion-id)
	     (clime-inspector-insert-link-to-type-id
	      "(companion)" id
	      (not (clime-type-is-object-p type))))

	   ;; Display each member, arranged by owner type
	   (dolist (interface interfaces)
	     (let* ((owner-type (plist-get interface :type))
		    (implicit (plist-get interface :via-view))
		    (members (plist-get owner-type :members)))
	       (clime-insert-with-face
		(format "\n\n%s%s\n"
			(clime-declared-as-str owner-type)
			(if implicit
			    (concat " (via implicit, " implicit ")") ""))
		font-lock-comment-face)
	       (clime-inspector-insert-linked-type owner-type t t)
	       (insert "\n")
	       (insert "---------------------------\n")
	       (dolist (m members)
		 (when (and focus-on-member
			    (equal (clime-member-name m)
				   focus-on-member))
		   (setq focus-point (point)))
		 (clime-inspector-insert-linked-member owner-type m)
		 (insert "\n")
		 )
	       ))

	   (if (integerp focus-point)
	       (progn (goto-char focus-point)
		      (recenter-top-bottom))
	     (goto-char (point-min)))
	   ))
       ))))



;; Inspector

(defun clime-path-completions (path predicate flag)
  "Return a list of valid completions of the given qualified path.
See: http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_280.html for the
interface we are implementing."
  ;; Note: when this function is invoked from completing-read, current
  ;; buffer will be mini-buffer. We need to setup connection manually.
  ;; See clime-completing-read-path...
  (clime-with-path-and-name
   path (pack name)
   (let* ((members (clime-rpc-package-member-completions pack name))
	  (candidates (mapcar (lambda (ea)
				(let ((name (plist-get ea :name)))
				  (if (and pack (> (length pack) 0))
				      (concat pack "." name) name)))
			      members)))
     (cond
      ((null flag) (try-completion path candidates predicate))
      ((eq flag t) (all-completions path candidates predicate))
      ((eq 'lambda flag) (member candidates path))
      (t nil)
      ))))

(defun clime-completing-read-path (prompt &optional initial)
  ;; Note: First thing we do is bind buffer connection so
  ;; completion function will have access.
  (let ((clime-dispatching-connection
	 (clime-current-connection)))
    (completing-read prompt #'clime-path-completions
		     nil nil (or initial (clime-package-containing-point)))))

(defun clime-inspect-package-by-path (path)
  (clime-package-inspector-show
   (clime-rpc-inspect-package-by-path path)))

(defun clime-inspect-by-path (&optional path focus-on-member)
  "Open the Inspector on the type or package denoted by path. If path is nil,
read a fully qualified path from the minibuffer."
  (interactive)
  (let* ((case-fold-search nil))
    (let ((p (or path
		 (clime-completing-read-path
		  "Qualified type or package name: "))))
      (clime-with-path-and-name
       p (pack name)
       (if (and name (integerp (string-match "^[a-z_0-9]+$" name)))
	   (clime-inspect-package-by-path p)
	 (let ((type (clime-rpc-get-type-by-name p)))
	   (if type
	       (let ((info (clime-rpc-inspect-type-by-id
			    (clime-type-id type))))
		 (clime-type-inspector-show info focus-on-member))
	     (message "Could not locate type named '%s'." p))
	   ))))))


(defun clime-package-path-at-point ()
  "Return the package path at point, or nil if point is not in a package path."
  (let* ((case-fold-search nil)
	 (re "\\(?:package\\|import\\)[ ]+\\(\\(?:[a-z][a-z0-9_]+\\.\\)+[a-z][a-z0-9]+\\)"))
    (save-excursion
      (catch 'return
	(let ((init-point (point))
	      (limit (point-at-eol)))
	  (goto-char (point-at-bol))
	  (while (search-forward-regexp re limit t)
	    (if (and (>= init-point (match-beginning 1))
		     (<= init-point (match-end 1)))
		(throw 'return
		       (clime-kill-txt-props
			(match-string 1))))))))))


(defun clime-package-containing-point ()
  "Return the package point is in."
  (save-excursion
    (when (search-backward-regexp
	   "^package \\(\\(?:[a-z0-9_]+\\.\\)*[a-z0-9_]+\\)"
	   (point-min) t)
      (let ((path (match-string 1)))
	(clime-kill-txt-props path)))))

(defun clime-imported-type-path-at-point ()
  "Return the qualified name of the type being imported at point."
  (when-let (sym (symbol-at-point))
    (let ((sym-name (clime-kill-txt-props
		     (symbol-name sym))))
      (when (and (integerp (string-match "^[A-ZA-z_]+$" sym-name))
		 (save-excursion
		   (beginning-of-line)
		   (search-forward-regexp
		    (concat
		     "^\\s-*import \\(\\(?:[a-z0-9_]+\\.\\)*\\)"
		     "\\(?:[A-Z][A-z0-9_\\.]+\\|{[A-z0-9_\\., \n]+}\\)$")
		    (point-at-eol) t)))
	(let ((path (clime-kill-txt-props (match-string 1))))
	  (concat path sym-name))))))

(defun clime-inspect-package-at-point ()
  "If cursor is over a package path, inspect that path. Otherwise,
inspect the package of the current source file."
  (interactive)
  (let ((pack (or (clime-package-path-at-point)
		  (clime-package-containing-point))))
    (if pack
	(clime-inspect-by-path pack)
      (message "No package declaration found."))))


(defun clime-inspect-project-package ()
  "Inspect the package declared as the project package in the config file."
  (interactive)
  (let* ((config (clime-config))
	 (given (plist-get config :project-package)))
    (clime-inspect-by-path given)))

(defun clime-inspector-insert-package (pack)
  "Helper to insert a hyper-linked package name."
  (let ((name (clime-package-full-name pack))
	(members (clime-package-members pack)))
    (insert (make-string clime-indent-level ?\s))
    (clime-inspector-insert-linked-package-path name font-lock-variable-name-face)
    (insert "\n")
    (let ((clime-indent-level (+ clime-indent-level 5)))
      (dolist (ea members)
	(when (not (clime-package-p ea))
	  (clime-inspector-insert-linked-type ea nil nil)
	  (clime-insert-with-face
	   (format " %s" (clime-declared-as-str ea))
	   font-lock-comment-face)
	  (insert "\n")))
      (dolist (ea members)
	(when (clime-package-p ea)
	  (clime-inspector-insert-package ea)
	  ))
      )))

(defun clime-package-inspector-show (info)
  "Display a list of all the members of the provided package."
  (if (null info)
      (message "Cannot inspect nil package.")
    (let* ((buffer-name clime-inspector-buffer-name)
	   (clime-indent-level 0))
      (clime-with-inspector-buffer
       (buffer-name info t)
       (clime-inspector-insert-package info)
       (goto-char (point-min))
       ))))

(defvar clime-inspector-history '()
  "Maintain a history of the info objects viewed in the inspector buffer.")

(defvar clime-inspector-history-cursor 0
  "Where are we in the history?")

(defvar clime-inspector-paging-in-progress nil
  "A dynamic variable to inform dynamic extant of user's intent.
   Are we moving in history, or inspecting a new info?")

(defun clime-inspector-backward-page ()
  "Inspect the info object preceding current in history."
  (interactive)
  (setq clime-inspector-history-cursor
	(min (- (length clime-inspector-history) 1)
	     (+ clime-inspector-history-cursor 1)))
  (clime-inspector-goto-cursor))

(defun clime-inspector-forward-page ()
  "Inspect the info object following current in history."
  (interactive)
  (setq clime-inspector-history-cursor
	(max 0 (- clime-inspector-history-cursor 1)))
  (clime-inspector-goto-cursor))


(defun clime-inspector-goto-cursor ()
  "Helper to jump to a specific point in history."
  (let ((info (nth clime-inspector-history-cursor
		   clime-inspector-history))
	(clime-inspector-paging-in-progress t))

    (cond ((clime-package-p info)
	   (clime-package-inspector-show info))

	  ((clime-type-inspection-p info)
	   (clime-type-inspector-show info))

	  (t (error
	      (format "Cannot inspect unknown structure: %s"
		      info))))
    ))


(defvar clime-popup-inspector-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "q") 'clime-popup-buffer-quit-function)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    (define-key map (kbd ".") 'clime-inspector-forward-page)
    (define-key map (kbd ",") 'clime-inspector-backward-page)
    map)
  "Type and package inspector key bindings.")


(defmacro* clime-with-inspector-buffer ((name object &optional select)
					 &body body)
  "Extend the standard popup buffer with inspector-specific bindings."
  `(clime-with-popup-buffer
    (,name t ,select)
    (use-local-map clime-popup-inspector-map)
    (when (not clime-inspector-paging-in-progress)

      ;; Clamp the history cursor
      (setq clime-inspector-history-cursor
	    (max 0 clime-inspector-history-cursor))
      (setq clime-inspector-history-cursor
	    (min (- (length clime-inspector-history) 1)
		 clime-inspector-history-cursor))

      ;; Remove all elements preceding the cursor (the 'redo' history)
      (setq clime-inspector-history
	    (subseq clime-inspector-history
		    clime-inspector-history-cursor))

      ;; Add the new history item
      (push ,object clime-inspector-history)

      ;; Set cursor to point to the new item
      (setq clime-inspector-history-cursor 0)

      )
    ,@body
    ))


;; Interface

(defvar clime-message-function 'message)

(defun clime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
	(if (fboundp 'temp-minibuffer-message) ;; XEmacs
	    (temp-minibuffer-message text)
	  (minibuffer-message text))
      (message "%s" text))))

(defun clime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply clime-message-function format args))

(defun clime-display-warning (message &rest args)
  (display-warning '(clime warning) (apply #'format message args)))

(defvar clime-background-message-function 'clime-display-oneliner)


(defun clime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `clime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply clime-background-message-function format-string format-args))

(defun clime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (clime-oneliner msg)))))

(defun clime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
			   (or (position ?\n string) most-positive-fixnum)
			   (1- (frame-width)))))



;; Data-structure accessors

(defun clime-search-sym-name (sym)
  (plist-get sym :name))

(defun clime-search-sym-local-name (sym)
  (plist-get sym :local-name))

(defun clime-search-sym-pos (sym)
  (plist-get sym :pos))

(defun clime-search-sym-owner-name (sym)
  (plist-get sym :owner-name))

(defun clime-search-sym-decl-as (sym)
  (plist-get sym :decl-as))

(defun clime-symbol-name (sym)
  (plist-get sym :name))

(defun clime-symbol-decl-pos (sym)
  (plist-get sym :decl-pos))

(defun clime-symbol-type (sym)
  (plist-get sym :type))

(defun clime-package-name (info)
  (plist-get info :name))

(defun clime-package-full-name (info)
  (plist-get info :full-name))

(defun clime-package-members (info)
  (plist-get info :members))

(defun clime-package-p (info)
  (equal 'package (plist-get info :info-type)))

(defun clime-type-inspection-p (info)
  (equal 'typeInspect (plist-get info :info-type)))

(defun clime-type-name (type)
  (plist-get type :name))

(defun clime-type-name-with-args (type)
  (concat (plist-get type :name)
	  (clime-type-type-args-postfix type)))

(defun clime-type-id (type)
  (plist-get type :type-id))

(defun clime-type-is-object-p (type)
  (equal (plist-get type :decl-as) 'object))

(defun clime-outer-type-id (type)
  (plist-get type :outer-type-id))

(defun clime-type-full-name (type)
  (if (plist-get type :arrow-type)
      (plist-get type :name)
    (plist-get type :full-name)))

(defun clime-type-full-name-with-args (type)
  (if (plist-get type :arrow-type)
      (plist-get type :name)
    (concat
     (plist-get type :full-name)
     (clime-type-type-args-postfix type))))

(defun clime-type-type-args-postfix (type)
  (let ((args (clime-type-type-args type)))
    (if args
	(concat "["
		(mapconcat
		 (lambda(tpe)
		   (clime-type-name-with-args tpe)) args ", ")
		"]")
      "")))

(defun clime-declared-as (obj)
  (plist-get obj :decl-as))

(defun clime-declared-as-str (obj)
  (case (plist-get obj :decl-as)
    (method "method")
    (trait "trait")
    (interface "interface")
    (class "class")
    (object "object")
    (otherwise "type")
    ))

(defun clime-type-is-arrow-p (type)
  (plist-get type :arrow-type))

(defun clime-type-param-sections (type)
  (plist-get type :param-sections))

(defun clime-type-param-types (type)
  "Return types of params in first section."
  (let ((section (car (plist-get type :param-sections))))
    (mapcar
     (lambda (p)
       (cadr p))
     (plist-get section :params)
     )))

(defun clime-type-result-type (type)
  (plist-get type :result-type))

(defun clime-type-type-args (type)
  (plist-get type :type-args))

(defun clime-member-name (member)
  (plist-get member :name))

(defun clime-member-type (member)
  (plist-get member :type))

(defun clime-member-pos (member)
  (plist-get member :pos))

(defun clime-pos-file (pos)
  (plist-get pos :file))

(defun clime-pos-offset (pos)
  (or (plist-get pos :offset) -1))

(defun clime-pos-line (pos)
  (or (plist-get pos :line) -1))

(defun clime-pos-valid-local-p (pos)
  (and (stringp (clime-pos-file pos))
       (file-exists-p (clime-pos-file pos))
       (integerp (clime-pos-offset pos))
       (> (clime-pos-offset pos) 0)))

(defun clime-note-file (note)
  (plist-get note :file))

(defun clime-note-beg (note)
  (plist-get note :beg))

(defun clime-note-end (note)
  (plist-get note :end))

(defun clime-note-line (note)
  (plist-get note :line))

(defun clime-note-message (note)
  (plist-get note :msg))

;; Portability

(defun clime-computed-point ()
  "Subtract one to convert to 0-indexed buffer offsets.
 Additionally, in buffers with windows-encoded line-endings,
 add the appropriate number of CRs to compensate for characters
 that are hidden by Emacs."
  (clime-externalize-offset (point)))

(defun clime-externalize-offset (offset)
  (+ offset (- clime-ch-fix)
     (if (eq 1 (coding-system-eol-type buffer-file-coding-system))
	 (- (line-number-at-pos offset) 1)
       0)
     ))

(defun clime-internalize-offset (offset)
  (+ offset clime-ch-fix))

(defun clime-internalize-offset-fields (plist &rest keys)
  (dolist (key keys)
    (setq plist (plist-put
		 plist key
		 (clime-internalize-offset
		  (plist-get plist key)))))
  plist)

;; Popup Buffer

;;;;; Temporary popup buffers

(defvar clime-popup-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'clime-popup-buffer-quit-function)
    (define-key map [mouse-1] 'push-button)
    map)
  "Keymap for `clime-popup-buffer-mode'.")

(define-minor-mode clime-popup-buffer-mode
  "Mode for displaying read only stuff"
  nil
  nil
  (make-sparse-keymap))

(add-to-list 'minor-mode-alist
	     '(clime-popup-buffer-mode (:eval (clime-modeline-string))))

(defvar clime-popup-restore-data nil
  "Data needed when closing popup windows.
This is used as buffer local variable.
The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
POPUP-WINDOW is the window used to display the temp buffer.
That window may have been reused or freshly created.
SELECTED-WINDOW is the window that was selected before displaying
the popup buffer.
OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
OLD-BUFFER is nil if POPUP-WINDOW was newly created.

See `view-return-to-alist' for a similar idea.")

(make-variable-buffer-local
 (defvar clime-is-popup-buffer nil
   "So we can query later whether this is a popup buffer."))

;; Interface
(defmacro* clime-with-popup-buffer ((name &optional connection select)
				     &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
CONNECTION is the value for `clime-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.
"
  `(let* ((vars% (list ,(if (eq connection t) '(clime-connection) connection)))
	  (standard-output (clime-make-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1
	   (progn
	     ,@body)
	 (assert (eq (current-buffer) standard-output))
	 (setq buffer-read-only t)
	 (set-window-point (clime-display-popup-buffer ,(or select 'nil))
			   (point))))))


(defun clime-make-popup-buffer (name buffer-vars)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `clime-popup-buffer-mode'."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table lisp-mode-syntax-table)
    (clime-init-popup-buffer buffer-vars)
    (use-local-map clime-popup-buffer-map)
    (setq clime-is-popup-buffer t)
    (current-buffer)))

(defun clime-init-popup-buffer (buffer-vars)
  (clime-popup-buffer-mode 1)
  (multiple-value-setq (clime-buffer-connection)
    buffer-vars))

(defun clime-display-popup-buffer (select)
  "Display the current buffer.
   Save the selected-window in a buffer-local variable, so that we
   can restore it later."
  (let ((selected-window (selected-window))
	(old-windows))
    (walk-windows (lambda (w)
		    (if (not (clime-popup-buffer-p (window-buffer w)))
			(push (cons w (window-buffer w)) old-windows)))
		  nil t)
    (let ((new-window
	   (cond
	    (clime-popup-in-other-frame
	     (display-buffer-other-frame (current-buffer)))
	    (t (display-buffer (current-buffer))))))
      (unless clime-popup-restore-data
	(set (make-local-variable 'clime-popup-restore-data)
	     (list new-window
		   selected-window
		   (cdr (find new-window old-windows :key #'car)))))
      (when select
	(select-window new-window))
      new-window)))

(defun clime-close-popup-window ()
  (when clime-popup-restore-data
    (destructuring-bind (popup-window selected-window old-buffer)
	clime-popup-restore-data
      (kill-local-variable 'clime-popup-restore-data)
      (bury-buffer)
      (when (eq popup-window (selected-window))
	(cond ((and (not old-buffer) (not (one-window-p)))
	       (delete-window popup-window))
	      ((and old-buffer (buffer-live-p old-buffer))
	       (set-window-buffer popup-window old-buffer))
	      ))
      (when (window-live-p selected-window)
	(select-window selected-window)))
    ))


(defmacro clime-save-local-variables (vars &rest body)
  (let ((vals (make-symbol "vals")))
    `(let ((,vals (mapcar (lambda (var)
			    (if (clime-local-variable-p var)
				(cons var (eval var))))
			  ',vars)))
       (prog1 (progn . ,body)
	 (mapc (lambda (var+val)
		 (when (consp var+val)
		   (set (make-local-variable (car var+val)) (cdr var+val))))
	       ,vals)))))


(make-variable-buffer-local
 (defvar clime-popup-buffer-quit-function 'clime-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun clime-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `clime-popup-buffer-quit-function'."
  (interactive)
  (funcall clime-popup-buffer-quit-function kill-buffer-p))

(defun clime-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
  Restore the window configuration unless it was changed since we
  last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (clime-close-popup-window)
    (when kill-buffer-p
      (kill-buffer buffer))))


;;;;; Connection listing

(define-derived-mode clime-connection-list-mode fundamental-mode
  "Clime-Connections"
  "CLIME Connection List Mode.

\\{clime-connection-list-mode-map}
\\{clime-popup-buffer-mode-map}"
  (when clime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(clime-define-keys clime-connection-list-mode-map
		    ("g"         'clime-update-connection-list)
		    ((kbd "C-k") 'clime-quit-connection-at-point)
		    ("R"         'clime-restart-connection-at-point))

(defun clime-connection-at-point ()
  (or (get-text-property (point) 'clime-connection)
      (error "No connection at point")))

(defun clime-quit-connection-at-point (connection)
  (interactive (list (clime-connection-at-point)))
  (clime-quit-connection connection)
  (clime-update-connection-list))

(defun clime-quit-connection (connection)
  (clime-rpc-shutdown-server)
  (let ((end (time-add (current-time) (seconds-to-time 3))))
    (while (memq connection clime-net-processes)
      (when (time-less-p end (current-time))
	(message "Quit timeout expired.  Disconnecting.")
	(delete-process connection))
      (sit-for 0 100))
    ))

(defun clime-restart-connection-at-point (connection)
  (interactive (list (clime-connection-at-point)))
  (let ((clime-dispatching-connection connection))
    (clime-restart-inferior-lisp)))


(defvar clime-connections-buffer-name "*CLIME Connections*")

(defun clime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (clime-with-popup-buffer (clime-connections-buffer-name)
			    (clime-connection-list-mode)
			    (clime-draw-connection-list)))

(defun clime-update-connection-list ()
  "Display a list of all connections."
  (interactive)
  (let ((pos (point))
	(inhibit-read-only t))
    (erase-buffer)
    (clime-draw-connection-list)
    (goto-char pos)))

(defun clime-draw-connection-list ()
  (let ((default-pos nil)
	(fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
	    (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse clime-net-processes))
      (clime-insert-propertized
       (list 'clime-connection p)
       (format fstring
	       " "
	       (clime-connection-number p)
	       (clime-connection-name p)
	       (or (process-id p) (process-contact p))
	       (clime-pid p)
	       (clime-server-implementation-type p))))
    ))




;; Interface Helpers

(defmacro clime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun clime-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

(defsubst clime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (clime-propertize-region props (apply #'insert args)))

(defmacro clime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)) (l (gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
	 (clime-indent-rigidly ,start (point) ,l)))))

(put 'clime-with-rigid-indentation 'lisp-indent-function 1)

(defun clime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
		  (progn
		    (insert-before-markers indent)
		    (zerop (forward-line -1))))))))

(defun clime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (clime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun clime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))


;; Testing helpers

(defun clime-event-sig (event &optional value)
  "Signal an event. Send to testing harness if it exists.
   Used to drive asynchronous regression tests."
  (if (fboundp 'clime-test-sig)
      (clime-test-sig event value)))



(provide 'clime)
