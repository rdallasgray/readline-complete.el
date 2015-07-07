;; -*- lexical-binding: t -*-

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

(describe "rlc-prefix-chars"
          (let ((prompt "bash$ "))
            (describe "given some entries"
                      (let ((entries
                             '(("~/.em" . ".em")
                               ("/usr/lo" . "lo")
                               ("origin/ma" . "ma")
                               ("rebase --ab" . "--ab")
                               ("git che" . "che")
                               ("--arg=val" . "val"))))
                        (it "returns the correct prefix for the entry"
                            (mapc (lambda (entry)
                                    (with-entry (car entry)
                                                (expect (rlc-prefix-chars)
                                                        :to-equal
                                                        (cdr entry))))
                                  entries))))))
