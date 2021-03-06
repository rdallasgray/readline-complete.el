;;; readline-complete.el --- offers completions in shell mode

;; Copyright (C) 2012 Christopher Monsanto
;; Copyright (C) 2014 Dmitry Gutov

;; Author: Christopher Monsanto <chris@monsan.to>
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I find it particularly annoying that Emacs *shell* buffers do not
;; offer satisfactory completions, but xterm does. I even used
;; ansi-term for a while because of this (oh god). What I really
;; wanted was for Emacs to show completions via auto-complete.el, like
;; every other mode I use often. This package hopes to deliver on that
;; promise.

;; xterm and friends get their completions from readline, a GNU
;; library that is aware of your context and monitors your keystrokes
;; for TABs and what not so you can be shown a list of
;; completions. readline-complete.el gives you a process filter that
;; tries to parse readline completion menus and give Emacs a list of
;; strings that are valid for completion in the current context.

;;; Installation:

;; There is no one "right" way to install this package, because it
;; does not provide any user level commands. I find that it works
;; particularly well with shell-mode and auto-complete.el, so I'll
;; describe how my setup works.

;; First, let us setup shell-mode. shell-mode, by default, does not
;; create a tty (terminal) and does not echo input, both of which are
;; required for proper operation. To fix this, adjust the arguments to
;; your shell:

;; (setq explicit-shell-file-name "bash")
;; (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;; (setq comint-process-echoes t)

;; ASIDE: if you call ssh from shell directly, add "-t" to explicit-ssh-args to enable terminal.

;; The simplest way is to install from melpa, and add to your init file:
;; (require 'readline-complete)
;; This package also supports lazy loading via a loaddefs file.

;; Two completion frameworks are supported, you only need to pick one.

;; Auto-Complete setup:

;; Go ahead and get auto-complete.el from
;; http://cx4a.org/software/auto-complete/, and follow the
;; instructions for setup.

;; Then enable it in your init file.

;; (add-to-list 'ac-modes 'shell-mode)
;; (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)

;; Company setup:

;; See installation instructions at http://company-mode.github.io/.

;; Set up completion back-end:

;; (push 'company-readline company-backends)
;; (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))

;; Finally, M-x shell, and start typing!

;; For customization, see the docstrings of `rlc-timeout`,
;; `rlc-attempts`, and `ac-rlc-prompts`.

;;; Gotchas:

;; GNU readline, unfortunately, was not made for this kind of
;; operation. That is not to say this package isn't stable (how could
;; you think such a thing!), but it is sensitive to some readline
;; customizations. Consider putting some of the following lines in
;; your bashrc (you can test if we are using Emacs for the session by
;; testing [[ $INSIDE_EMACS ]]. Note that readline-complete should
;; work with the default readline settings.

;; - bell-style: it is preferable to disable the bell.
;;   bind 'set bell-style none'
;;
;; - completion-display-width: saves bandwidth to set this to zero:
;;   bind 'set completion-display-width 0'
;;
;; - completion-prefix-display-length: ***MUST*** be 0, otherwise
;;   readline truncates the completions. This is the default.
;;
;; - completion-query-items: Controls the "Display ... possible
;;   completions?" menu. Adjust this to keep readline-complete from
;;   downloading tons and tons of completions. The default of 100 is
;;   pretty good.
;;
;; - disable-completion: ***MUST*** be off! Default.
;;
;; - expand-tilde: ***MUST*** be off! Default.
;;
;; - horizontal-scroll-mode: Default is off, but on will save
;;   bandwidth. bind 'set horizontal-scroll-mode on'.
;;
;; - page-completions: Default is on (off on Ubuntu), but causes
;;   readline-complete to disregard completions with the --More--
;;   menu. Best to disable  this with bind 'set page-completions off'.
;;
;; - print-completions-horizontally: Default is off, but saves
;;   bandwidth to turn it on.
;;   bind 'set print-completions-horizontally on'.
;;
;; - bindings: When a command is executed, readline processes the text
;;   as input according to its bindings. Best to make tab characters not
;;   replaced by the default binding "\C-i": complete. Instead:
;;   bind '"\C-i": self-insert'
;;

;; *** Note that this package works with READLINE. Maybe editline (BSD
;; programs), NOT haskeline (ghci, unfortunately). Shells that don't use
;; readline (like zsh?) won't work. Use bash instead. (come on, with
;; shopt you can enable most of zsh's cool features, and the user
;; experience is given by Emacs!!).

;; I don't use a Mac (Linux all the way), but I hear Apple uses
;; editline instead of readline. Consider recompiling bash, python,
;; etc with readline support instead.

;; Patches welcome! https://github.com/monsanto/readline-complete.el

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'comint)
(require 'shell)

(defvar rlc-idle-time 0.01
  "How much idle time must elapse before trying to retrieve candidates
(company only).")

(defvar rlc-attempts 5
  "How many times should we wait for readline output, before
aborting and running `rlc-no-readline-hook'?")

(defvar rlc-timeout 0.25
  "Time, in seconds, to wait for readline output. If readline is
Not enabled on your terminal, you will wait a total of
rlc-attempts * rlc-timeout seconds.")

(defvar rlc-no-readline-hook nil
  "Hook that is run when readline-complete cannot parse the
  output. Useful for disabling autocompletion.")

(defvar rlc-accumulated-output nil
  "The input obtained so far by `rlc-filter'.")

(defun rlc-filter (proc string)
  "Process filter which accumulates text in `rlc-accumulated-output'."
    (setq rlc-accumulated-output (concat rlc-accumulated-output string)))

(defun rlc-regexp-more (term-re chars-to-delete)
  "Match a 'More' dialog given TERM-RE and CHARS-TO-DELETE."
  (concat "\C-m?\n"
          "\\(?:\\(?:.*\n\\)+\\)" ; match a bunch of lines (ignored)
          "--More--\C-m\C-m"
          ".*?" ; prompt
          term-re
          "\\*" ; terminator
          (format "\\(?:\C-h \C-h\\)\\{%s\\}" (1- chars-to-delete))
          ;; maybe bell at the end (too many deletions)
          "\C-g?"))

(defun rlc-regexp-too-many (term-re chars-to-delete)
  "Match a 'Display all' dialog given TERM-RE and CHARS-TO-DELETE."
  (concat "\C-m?\n"
          "Display all [0-9]+ possibilities\\? (y or n)"
          "\C-m?\n.*?" ; prompt
          term-re
          "\\*" ; A terminator
          ;; Once for position backwards, once for space, once for reposition...
          (format "\\(?:\C-h \C-h\\)\\{%s\\}" (1- chars-to-delete))
          ;; maybe bell at the end (too many deletions)
          "\C-g?"))

(defun rlc-regexp-enough (term-re chars-to-delete)
  "Match readline completions given TERM-RE and CHARS-TO-DELETE."
  (concat
   "\\(?:"
   "\C-m?\n"
   "\\(\\(?:.*\n\\)+\\)" ; success; match a bunch of lines
   ".*?" ; then match the prompt followed by our old input
   ;; stack overflow without non-greedy
   term-re
   "\\|"
   "\C-g?" ; maybe bell otherwise
   "\\)"
   "n\\*" ; Terminator
   ;; Once for position backwards, once for space, once for reposition...
   (format "\\(?:\C-h \C-h\\)\\{%s\\}" chars-to-delete)))

(defun rlc-regexp (term)
  "Match readline output given TERM."
  (let ((term-re (regexp-quote term))
        (chars-to-delete (+ (length term) (length "n*"))))
    (concat
     term-re
     "\\(?:"
     (rlc-regexp-more term-re chars-to-delete)
     "\\|"
     (rlc-regexp-too-many term-re chars-to-delete)
     "\\|"
     (rlc-regexp-enough term-re chars-to-delete)
     "\\)"))
  )

(defun rlc-send-input (input proc)
  "Send INPUT to the shell process PROC, show the completion menu, \
dismiss any prompts, then delete the input."
  (let* ((chars-to-delete (+ (length input) (length "n*")))
         (str-to-send (concat input
                              "\e?" ; show menu
                              "n*"  ; dismiss prompts
                              (make-string chars-to-delete ?\C-h))))
    (process-send-string proc str-to-send)))

(defun rlc-find-candidates (output regexp)
  "Find match candidates in OUTPUT using REGEXP."
  (when (and output
             (string-match regexp output)
             (match-string 1 output))
    (rlc-unpath (split-string (match-string 1 output)))))

(defvar rlc-last-prefix-chars "")

(defvar rlc-last-candidates nil)

(defun rlc-candidates ()
  "Return the list of completions that readline would have given via \
completion-menu."
  (let* ((proc (get-buffer-process (current-buffer)))
         ;; Get the current process filter so we can restore it afterwards
         (filt (process-filter proc))
         (term (buffer-substring-no-properties
                (save-excursion (comint-bol) (point))
                (point)))
         (matches '()))
    (unwind-protect
        (progn
          (setq rlc-accumulated-output "")
          ;; Set our filter, which captures all output
          (set-process-filter proc 'rlc-filter)
          (rlc-send-input term proc)
          (setq matches (rlc-attempt-match
                         term
                         (lambda () rlc-accumulated-output)))
          (when (null matches)
            (progn
              (run-hooks 'rlc-no-readline-hook))))
      (progn
        (set-process-filter proc filt)))
    matches))

(defun rlc-attempt-match (term get-output)
  "Repeatedly attempt to match TERM in the result of GET-OUTPUT."
  (let* ((regexp (rlc-regexp term))
         (matched (catch 'matched
                    (dotimes (done rlc-attempts)
                      (let* ((output (funcall get-output))
                             (matches (rlc-find-candidates output regexp)))
                        (if matches
                            (throw 'matched matches)
                          (when (< done rlc-attempts)
                            (progn
                              (sleep-for rlc-timeout)))))))))
    matched))

(defconst rlc-interpath-separator-regex "/[^ /]+")
(defconst rlc-path-separator-regex "/")

(defun rlc-unpath (candidates)
  "If the full prefix is pathed, unpath CANDIDATES based on the length of path."
  (let ((n (- (length (split-string (rlc-full-prefix-chars) rlc-interpath-separator-regex)) 1)))
    (if (> n 0)
        (mapcar (lambda (candidate) (rlc-unpath-nth candidate n))
                candidates)
      candidates)))

(defun rlc-unpath-nth (candidate n)
  "Unpath CANDIDATE at depth N."
  (let* ((split-candidate (split-string candidate rlc-path-separator-regex))
         (nth-split (nth n split-candidate)))
    (if (and nth-split (not (string= "" nth-split)))
        nth-split
      candidate)))

;; Auto-Complete
;;

(defvar ac-rlc-prompts
  `(("^>>> " ac-prefix-default ac-prefix-c-dot) ; python, you have to
                                        ; setup python to work with
                                        ; readline though. use ipython instead.
    ("^In \\[[0-9]+\\]: " ac-prefix-rlc-dot) ; ipython
    ("^sftp> " ac-prefix-rlc-disable) ; sftp has no readline
    ("^lftp [^>]+> " ac-prefix-rlc-shell) ; lftp
    ("^\\[[0-9]+\\] pry([^>]> " ac-prefix-rlc-dot) ; pry
    ("^irb([^>]> " ac-prefix-rlc-dot) ; irb, need irb -r irb/completion
    (,shell-prompt-pattern ac-prefix-rlc-shell) ; Shell
    )
  "ac-rlc works by checking the current prompt. This list holds
  all of ac-rlc's known prompts, along with an auto-complete
  prefix to recognize contexts appropriate to the application.

To disable ac-rlc for an application, add '(prompt ac-prefix-rlc-disable).")

(defun ac-prefix-rlc-disable ()
  "Disable autocomplete on readline."
  nil)

(defun ac-prefix-rlc-shell ()
  (if (re-search-backward "[ /=]\\([^ /=]+\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-prefix-rlc-dot ()
  (if (re-search-backward "[^a-zA-Z0-9_.]\\([a-zA-Z0-9_.]+\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-rlc-setup-sources ()
  "Add me to shell-mode-hook!"
  (add-to-list 'ac-sources 'ac-source-shell)
  (add-hook 'rlc-no-readline-hook '(lambda () (auto-complete-mode -1))))

(defun rlc-full-prefix ()
  "Prefix back to the most recent break character."
  (if (re-search-backward "[ =]\\([^ =]+\\)" nil t)
      (match-beginning 1)))

(defun rlc-full-prefix-chars ()
  "Full prefix as characters."
  (rlc-prefix-chars-for-prefix (lambda () (rlc-full-prefix))))

(defun rlc-prefix-chars-for-prefix (prefix-fun)
  "Prefix as characters for function PREFIX-FUN."
  (save-excursion
    (let ((pt (point))
          (beg (funcall prefix-fun)))
      (and beg (buffer-substring-no-properties beg pt)))))

(defun rlc-prefix-chars ()
  "Prefix as characters."
  (rlc-prefix-chars-for-prefix (lambda () (ac-rlc-prefix-shell-dispatcher))))

;;;###autoload
(defun ac-rlc-prefix-shell-dispatcher ()
  (let ((prefix (and (not isearch-mode)
                     (save-excursion
                       (comint-bol)
                       (loop for (regexp prefix) in ac-rlc-prompts
                             when (looking-back regexp)
                             return prefix)))))
    (when prefix
      (funcall prefix))))

;;;###autoload
(eval-after-load 'auto-complete
  ;; eval to avoid compiling. compiler needs ac-define-source macro
  ;; definition loaded or else we get runtime error, but we want to keep
  ;; installation of auto-complete optional
  '(eval '(ac-define-source shell
                            '((candidates . rlc-candidates)
                              (prefix . ac-rlc-prefix-shell-dispatcher)
                              (requires . 0)))))

;; Company
;;

;;;###autoload
(defun company-readline (command &optional arg &rest _ignore)
  "`company-mode' back-end using `readline-complete'."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-readline))
    (prefix (rlc-prefix-chars))
    (candidates (rlc-candidates))))

(provide 'readline-complete)

;;; readline-complete.el ends here
