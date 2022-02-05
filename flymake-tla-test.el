;;; flymake-tla-test.el --- A TLA+ Flymake backend tests  -*- lexical-binding: t; coding: utf-8 -*-

(require 'ert)
(require 'flymake-tla)

(defun flymake-tla-test--gather-from-sany-file (tla sany)
  (with-temp-buffer
	(insert-file-contents-literally sany)
	(let ((diags (flymake-tla--gather-from-current-buffer)))
	  diags)))

(ert-deftest flymake-tla-test-count ()
  (dolist (example flymake-tla-test--examples)
	(message "Testing %s" example)
	(should
	 (=
	  (length (plist-get example :errors))
	  (length (flymake-tla-test--gather-from-sany-file
			   (plist-get example :tla)
			   (plist-get example :sany)))))))

(defvar flymake-tla-test--examples nil
  "List of example output from SANY.")

(setq
 flymake-tla-test--examples
 '((:sany
	"./test_data/SeqAlreadyDefined.sany"
	:tla
	"./test_data/SeqAlreadyDefined.tla"
	:errors (1))
   (:sany
	"./test_data/ModuleParsingError.sany"
	:tla
	"./test_data/ModuleParsingError.tla"
	:errors (1))))

(provide 'flymake-tla-test)
;;; flymake-tla-test.el ends here
