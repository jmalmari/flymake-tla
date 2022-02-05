;;; flymake-tla-test.el --- A TLA+ Flymake backend tests  -*- lexical-binding: t; coding: utf-8 -*-

;; See ERT manual to see how to run.
;; One example is
;;     emacs -Q -batch -l ert -l flymake-tla.el -l flymake-tla-test.el -f ert-run-tests-batch-and-exit
;;

(require 'ert)
(require 'flymake-tla)
(require 'cl-lib)

(defun flymake-tla-test--gather-from-sany-file (tla sany)
  (with-temp-buffer
	(insert-file-contents-literally sany)
	(let ((diags (flymake-tla--gather-from-current-buffer)))
	  diags)))

(ert-deftest flymake-tla-test-count ()
  (dolist (example flymake-tla-test--examples)
	(message "Testing %s" example)
	(let ((diags (flymake-tla-test--gather-from-sany-file
				  (plist-get example :tla)
				  (plist-get example :sany))))
	  (should
	   (=
		(length (plist-get example :errors))
		(length diags)))
	  (cl-mapcar
	   (lambda (actual expected)
		 (should (string= actual expected)))
	   (seq-map (lambda (diag) (flymake-diagnostic-text diag)) diags)
	   (plist-get example :errors)))))

(defvar flymake-tla-test--examples nil
  "List of example output from SANY.")

(setq
 flymake-tla-test--examples
 '((:sany
	"./test_data/SeqAlreadyDefined.sany"
	:tla
	"./test_data/SeqAlreadyDefined.tla"
	:errors ("Operator Seq already defined or declared."
			 "Multiply-defined symbol 'Seq': this definition or declaration conflicts \nwith the one at line 20, col 1 to line 20, col 41 of module Sequences."))
   (:sany
	"./test_data/ModuleParsingError.sany"
	:tla
	"./test_data/ModuleParsingError.tla"
	:errors ("Was expecting \"==== or more Module body\"\nEncountered \"Token1\""))
   (:sany
	"./test_data/MultipleDeclarations.sany"
	:tla
	"./test_data/MultipleDeclarations.tla"
	:errors ("Multiple declarations or definitions for symbol Token1.  \nThis duplicates the one at line 2, col 10 to line 2, col 15 of module MultipleDeclarations."))
   ))

(provide 'flymake-tla-test)
;;; flymake-tla-test.el ends here
