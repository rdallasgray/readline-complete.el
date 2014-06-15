;; -*- lexical-binding: t -*-

(defmacro context (description body)
  `(progn (ert-runner-message "%s\n" ,description)
          ,body))

(defmacro with-entry (entry body)
  `(with-temp-buffer
     (insert prompt)
     (insert ,entry)
     (let ((prompt-end (length prompt)))
       (noflet ((comint-bol () (move-to-column prompt-end)))
               ,body))))

(defmacro with-readline-output (op)
  `(with-temp-buffer
     (let ((proc (get-buffer-process (current-buffer))))
       (process-send-string ,op))))

(context "with a shell prompt"
         (let ((prompt "bash$ "))
           (context "given an entry"
                    (let ((entries
                           '(("~/.em" . ".em")
                             ("/usr/lo" . "lo")
                             ("origin/ma" . "ma")
                             ("rebase --ab" . "--ab")
                             ("git che" . "che")
                             ("--arg=val" . "val"))))
                      (ert-deftest shell-prefix-test ()
                        "each test entry returns the given prefix"
                        (mapc
                         (lambda (entry)
                           (with-entry (car entry)
                                       (should (string= (rlc-prefix-chars)
                                                        (cdr entry)))))
                         entries))))))
