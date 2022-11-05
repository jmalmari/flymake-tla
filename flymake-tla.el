;;; flymake-tla.el --- A TLA+ Flymake backend  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2022

;; Author:  Jarno Malmari
;; Keywords: extensions, tools, languages

;;; Commentary:

;; This TLA+ Flymake backend is based on the Ruby example in the
;; Flymake info manual.
;;
;; The TLA+ parser SANY is used. Install either the standalone tools
;; or the TLA+ Toolbox. Specify the tla2tools.jar file location
;; `flymake-tla-tla2tools-jar'.
;;
;; You may use `flymake-tla-setup-backend-current-buffer' to register
;; the backend in buffer local `flymake-diagnostic-functions'. The
;; backend only works for TLA+ buffers. The actual backend function is
;; `flymake-tla'.
;;
;; Configuration example assuming `tla-mode':
;;
;;   (require 'flymake-tla)
;;   (add-hook 'tla-mode-hook #'flymake-tla-setup-backend-current-buffer)
;;   (add-hook 'tla-mode-hook #'flymake-mode)
;;

;;; Code:

(require 'cl-lib)

(defvar-local flymake-tla--proc nil)

(defvar flymake-tla-tla2tools-jar
  "/usr/share/java/tla-toolbox/tla2tools.jar"
  "Path to the file \”tla2tools.jar\" required to run SANY.")

(defun flymake-tla (report-fn &rest _args)
  (unless (executable-find
           "java") (error "Cannot find a suitable java"))

  ;; Kill any earlier process.
  (when (process-live-p flymake-tla--proc)
    (kill-process flymake-tla--proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `flymake-tla--proc' process to a new process
      ;; calling the tlc tool.
      ;;
      (setq
       flymake-tla--proc
       (make-process
        :name "flymake-tla" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        ;;
        :buffer (with-current-buffer (get-buffer-create "*flymake-tla*")
				  (erase-buffer)
				  (current-buffer))
		:command (list "java" "-cp" flymake-tla-tla2tools-jar "tla2sany.SANY" (buffer-file-name))
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          ;;
		  ;; The exit code is somewhat irrelevant. For instance,
		  ;; parsing error leads to exit code 255 but semantic errors
		  ;; have the code 0.
          (when (eq 'exit (process-status proc))
            (unwind-protect
                ;; Only proceed if `proc' is the same as the buffer
                ;; local variable `flymake-tla--proc', which indicates
                ;; that `proc' is not an obsolete process.
                ;;
                (if (with-current-buffer source (eq proc flymake-tla--proc))
                    (with-current-buffer (process-buffer proc)
                      (apply report-fn (flymake-tla--search-sany-buffer source)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
			  ))))))))

(defun flymake-tla--strip-text-properties (str)
  (set-text-properties 0 (length str) nil str)
  str)

(defun flymake-tla--search-modules ()
  "Search current buffer for modules and their files. They'll be
returned as a list with entries like so:

    (\"Module1\" ((file . \"/path/to/Module1.tla\")))"
  (cl-loop
   while (search-forward-regexp
		  "^Parsing file \\(.*\\)$"
		  nil t)
   for fname = (flymake-tla--strip-text-properties (match-string 1))
   for module = (file-name-sans-extension
				 (file-name-nondirectory fname))
   collect (cons module
				 (list
				  (cons 'file fname))) into modules
   finally return modules))

(defun flymake-tla--match-extractor-indices (extractor)
  (append
   (and (plist-member extractor :module)
		(list :module (match-string (plist-get extractor :module))))
   (and (plist-member extractor :text)
		(list :text (match-string (plist-get extractor :text))))
   (and (plist-member extractor :line)
		(plist-member extractor :column)
		(list :beg (cons
					 (string-to-number (match-string (plist-get extractor :line)))
					 (string-to-number (match-string (plist-get extractor :column))))))
   (and (plist-member extractor :endline)
		(plist-member extractor :endcolumn)
		(list :end (cons
					(string-to-number (match-string (plist-get extractor :endline)))
					(string-to-number (match-string (plist-get extractor :endcolumn))))))))

(defun flymake-tla--default-module (modules)
  "Return the default module name, choosing from MODULES. Useful
  when SANY forgets to tell us which module the message is
  targeting."
  (caar modules))

(defun flymake-tla--fix-end-column (end)
  "Increment cdr of the cons cell END by one and return the new
cell.  The cons cell has the form (line . column)."
  (and end
	   (cons (car end)
			 (1+ (cdr end)))))

(defun flymake-tla--search-semantic-error-warning-boundaries ()
  "Search error and warning boundaries. Return a list
of ((ERROR_POS, ERROR_COUNT) (WARNING_POS, WARNING_COUNT))."
  (seq-map
   (lambda (re)
	 (let ((pos (search-forward-regexp re nil t)))
	   (when pos
		 (cons pos (string-to-number (match-string 1))))))
   (list
	"^*** Errors: \\([[:digit:]]+\\)"
	"^*** Warnings: \\([[:digit:]]+\\)")))

(defun flymake-tla--semantic-error-or-warning-by-pos (pos boundaries)
  (cond
   ((nth 1 boundaries) ; Has a warning boundary?
	(if (< pos (car (nth 1 boundaries))) :error :warning))
   (:error)))

(defun flymake-tla--search-module-diagnostics (source)
  (let ((modules (flymake-tla--search-modules))
		(error-warning-boundaries (flymake-tla--search-semantic-error-warning-boundaries))
		(diags '())
		(hit-pos))
	(dolist (extractor flymake-tla--diag-extractors (list diags))
	  (goto-char (point-min))
	  (while (setq hit-pos (search-forward-regexp (plist-get extractor :re) nil t))
		(let* ((match (flymake-tla--match-extractor-indices extractor))
			   (module (or (plist-get match :module) (flymake-tla--default-module modules)))
			   (file (flymake-tla--module-get-file module modules))
			   (error_or_warning (flymake-tla--semantic-error-or-warning-by-pos hit-pos error-warning-boundaries)))
		  (if (file-readable-p file)
			  (add-to-list
			   'diags
			   ;; If this diagnostic targets the source buffer use the
			   ;; `flymake-make-diagnostic' with buffer argument.
			   ;; Flymake will then treat the diagnostic as domestic,
			   ;; whatever that means. This was added after noticing
			   ;; Flymake dropped all diagnostics when used with
			   ;; "emacs -q Module.tla".
			   (if (string= file (buffer-file-name source))
				   (let ((beg-region (flymake-diag-region
									  source
									  (car (plist-get match :beg))
									  (cdr (plist-get match :beg))))
						 (end-region (flymake-diag-region
									  source
									  (car (plist-get match :end))
									  (cdr (plist-get match :end)))))
					 (flymake-make-diagnostic
					  source
					  (car beg-region)
					  (or (cdr end-region) (cdr beg-region))
					  error_or_warning
					  (plist-get match :text)))
				 ;; Make a foreign diagnostic.
				 (flymake-make-diagnostic
				  file
				  (plist-get match :beg)
				  (flymake-tla--fix-end-column (plist-get match :end))
				  error_or_warning
				  (plist-get match :text)))
			   t)
			(flymake-log :warning "Where might module %s be at (%s)?" module file)))))))

(defun flymake-tla--search-sany-buffer (source)
  "Search the current buffer for SANY diagnostic messages
belonging to buffer SOURCE and return the result as a list of
arguments suitable for Flymake's REPORT-FN callback. See
documentation for variable `flymake-diagnostic-functions'."
  (goto-char (point-min))
  (if (search-forward-regexp "^****** SANY2 Version 2\\..*$" 1000 t)
	  (flymake-tla--search-module-diagnostics source)
	(message "Returning Panic")
	'(:panic
	  :explanation "SANY header line was not detected.")))

(defun flymake-tla--module-get (name modules)
  (cdr (assoc name modules)))

(defun flymake-tla--module-get-file (name modules)
  (cdr (assoc 'file (flymake-tla--module-get name modules))))

(defvar flymake-tla--diag-extractors nil
  "List of instructions for extracting diagnostics from Sany output.")

(setq
 flymake-tla--diag-extractors
 '((:re "^line \\([[:digit:]]+\\), col \\([[:digit:]]+\\) to line \\([[:digit:]]+\\), col \\([[:digit:]]+\\) of module \\([[:alnum:]]+\\).*\n\n\\(.+[\n].+\\|.+\\)"
		:module 5 :line 1 :column 2 :endline 3 :endcolumn 4 :text 6)
   (:re "\\(Was expecting [^\n]*\nEncountered \"[[:alnum:]]+\"\\) at line \\([[:digit:]]+\\), column \\([[:digit:]]+\\) and token .*$"
		:line 2 :column 3 :text 1)))

(defun flymake-tla-setup-backend-current-buffer ()
  "Register Flymake backend for current buffer."
  (interactive)
  (if (string-suffix-p ".tla" (buffer-file-name) t)
	  (add-hook 'flymake-diagnostic-functions 'flymake-tla nil t)
	(flymake-log :warning "Not setting up Flymake for TLA+ for file %s." (buffer-file-name))))

(provide 'flymake-tla)
;;; flymake-tla.el ends here
