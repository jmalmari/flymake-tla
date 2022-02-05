;;; flymake-tla.el --- A TLA+ Flymake backend  -*- lexical-binding: t; coding: utf-8 -*-
(defvar-local flymake-tla--proc nil)

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
        :buffer (generate-new-buffer "*flymake-tla*")
		:command (list "java" "-cp" "/usr/share/java/tla-toolbox/tla2tools.jar" "tla2sany.SANY" (buffer-file-name))
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          ;;
          (when (eq 'exit (process-status proc))
            (unwind-protect
                ;; Only proceed if `proc' is the same as the buffer
                ;; local variable `flymake-tla--proc', which indicates
                ;; that `proc' is not an obsolete process.
                ;;
                (if (with-current-buffer source (eq proc flymake-tla--proc))
                    (with-current-buffer (process-buffer proc)
                      (funcall report-fn (flymake-tla--gather-from-current-buffer)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              ;;
              (kill-buffer (process-buffer proc))))))))))

(defun flymake-tla-setup-backend ()
  (add-hook 'flymake-diagnostic-functions 'flymake-tla nil t))

;;(add-hook 'tla-mode-hook 'flymake-tla-setup-backend)

(defun flymake-tla--strip-text-properties (str)
  (set-text-properties 0 (length str) nil str)
  str)

(defun flymake-tla--parse-modules ()
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

(defun flymake-tla--make-diagnostic (source location type msg)
  (flymake-make-diagnostic
   source
   (car (flymake-diag-region source
							 (plist-get location :line)
							 (plist-get location :column)))
   (cdr (flymake-diag-region source
							 (plist-get location :endline)
							 (plist-get location :endcolumn)))
   type
   msg))

(defun flymake-tla--sany-location-from-extractor-match (extractor)
  (append
   (and (plist-member extractor :module)
		(list :module (match-string (plist-get extractor :module))))
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
  (caar modules))

(defun flymake-tla--gather-from-current-buffer ()
  (goto-char (point-min))
  (search-forward-regexp "^****** SANY2 Version 2.1 .*$")
  (let ((modules (flymake-tla--parse-modules))
		(issues '()))
	(dolist (extractor flymake-tla--issue-extractors issues)
	  (goto-char (point-min))
	  (while (search-forward-regexp (plist-get extractor :re) nil t)
		(let* ((location (flymake-tla--sany-location-from-extractor-match extractor))
			   (module (or (plist-get location :module) (flymake-tla--default-module modules)))
			   (file (flymake-tla--module-get-file module modules)))
		  (if (file-readable-p file)
			  (add-to-list
			   'issues
			   (flymake-make-diagnostic
				file
				(plist-get location :beg)
				(plist-get location :end)
				:error
				"Noniin tosi pahalta näyttää tässä."))
			(flymake-log :warning "Where might module %s be at (%s)?" module file)))))))

(defun flymake-tla--module-get (name modules)
  (cdr (assoc name modules)))

(defun flymake-tla--module-get-file (name modules)
  (cdr (assoc 'file (flymake-tla--module-get name modules))))

(defvar flymake-tla--issue-extractors nil
  "List of instructions for extracting issues from Sany output.")

(setq
 flymake-tla--issue-extractors
 '((:re "line \\([[:digit:]]+\\), col \\([[:digit:]]+\\) to line \\([[:digit:]]+\\), col \\([[:digit:]]+\\) of module \\([[:alnum:]]+\\)"
		:module 5 :line 1 :column 2 :endline 3 :endcolumn 4)
   (:re "Was expecting [^\n]*\nEncountered \"[[:alnum:]]+\" at line \\([[:digit:]]+\\), column \\([[:digit:]]+\\) and token .*$"
		:line 1 :column 2)))

(provide 'flymake-tla)
