;;; setup --- setup this repo with commands for hacking it -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)

(defvar advent-of-code-2022-in-common-lisp-repo-current-year nil
  "The year that I'm working on at the moment.")

(defvar advent-of-code-2022-in-common-lisp-repo-current-day nil
  "The day of the problem that I'm working on at the moment.")

(defvar advent-of-code-2022-in-common-lisp-repo-session-cookie nil
  "The session cookie for my session on adventofcode.")

(defun advent-of-code-2022-in-common-lisp-repo-reset-problem ()
  "Reset the day and year that I'm working on."
  (interactive)
  (setq advent-of-code-2022-in-common-lisp-repo-current-year nil
        advent-of-code-2022-in-common-lisp-repo-current-day nil))

(defun advent-of-code-2022-in-common-lisp-repo-reset-session ()
  "Reset the session cookie that I'm using to auth against adventofcode."
  (interactive)
  (setq advent-of-code-2022-in-common-lisp-repo-session-cookie nil))

(defun advent-of-code-2022-in-common-lisp-repo-ensure-current-session-cookie ()
  "Ensure that there's a current session cookie."
  (when (null advent-of-code-2022-in-common-lisp-repo-session-cookie)
    (setq advent-of-code-2022-in-common-lisp-repo-session-cookie
          (call-interactively #'advent-of-code-2022-in-common-lisp-repo-get-session-cookie))))

(defun advent-of-code-2022-in-common-lisp-repo-get-session-cookie (cookie)
  "Prompt for the current session COOKIE to connect to adventofcode."
  (interactive "sSession cookie: ")
  cookie)

(defun advent-of-code-2022-in-common-lisp-repo-get-year-day (year day)
  "Prompt the user for the YEAR and DAY that we're working on."
  (interactive "nYear: \nnDay: ")
  (list year day))

(require 'cl-lib)

(defun advent-of-code-2022-in-common-lisp-repo-ensure-current-problem ()
  "Ensure that the year and day are set."
  (when (or (null advent-of-code-2022-in-common-lisp-repo-current-year)
            (null advent-of-code-2022-in-common-lisp-repo-current-day))
    (cl-destructuring-bind (year day) (call-interactively #'advent-of-code-2022-in-common-lisp-repo-get-year-day)
      (setq advent-of-code-2022-in-common-lisp-repo-current-year year
            advent-of-code-2022-in-common-lisp-repo-current-day day))))

(defun advent-of-code-2022-in-common-lisp-repo-new-problem ()
  "Create a new problem for the current problem and switch buffer to it."
  (interactive)
  (progn
    (advent-of-code-2022-in-common-lisp-repo-ensure-current-problem)
    (let ((default-directory (project-root (project-current))))
      (shell-command (format "./add-problem.sh %s %s"
                             advent-of-code-2022-in-common-lisp-repo-current-year
                             advent-of-code-2022-in-common-lisp-repo-current-day))
      (find-file (format "%ssrc/%s-day-%s.lisp"
                         (project-root (project-current))
                         advent-of-code-2022-in-common-lisp-repo-current-year
                         advent-of-code-2022-in-common-lisp-repo-current-day)))))

(require 'url)

(defun advent-of-code-2022-in-common-lisp-repo-get-problem-input ()
  "Get the problem input for the current problem.

Puts it into src/YEAR-day-DAY.in."
  (interactive)
  (progn
    (advent-of-code-2022-in-common-lisp-repo-ensure-current-problem)
    (advent-of-code-2022-in-common-lisp-repo-ensure-current-session-cookie)
    (let ((url-request-method (encode-coding-string "GET" 'us-ascii))
          (url-request-extra-headers '())
          (url-mime-charset-string (url-mime-charset-string))
          (url-mime-language-string nil)
          (url-mime-encoding-string nil)
          (url-mime-accept-string nil)
          (url-personal-mail-address nil))

      (let* ((hkey (encode-coding-string "Cookie" 'us-ascii))
             (hvalue (encode-coding-string (format "session=%s"
                                                   advent-of-code-2022-in-common-lisp-repo-session-cookie)
                                           'us-ascii)))
        (setq url-request-extra-headers (cons (cons hkey hvalue) url-request-extra-headers)))
      (message "Fetching: %s" (format "https://adventofcode.com/%s/day/%s/input"
                                      advent-of-code-2022-in-common-lisp-repo-current-year
                                      advent-of-code-2022-in-common-lisp-repo-current-day))
      (url-retrieve (format "https://adventofcode.com/%s/day/%s/input"
                            advent-of-code-2022-in-common-lisp-repo-current-year
                            advent-of-code-2022-in-common-lisp-repo-current-day)
                    (lambda (_results) (let ((problem-input (buffer-substring (point-min) (point-max))))
                                         (find-file-other-window
                                          (format "%ssrc/%s-day-%s.in"
                                                  (project-root (project-current))
                                                  advent-of-code-2022-in-common-lisp-repo-current-year
                                                  advent-of-code-2022-in-common-lisp-repo-current-day))
                                         (delete-region (point-min) (point-max))
                                         (insert problem-input)
                                         (save-excursion
                                           (goto-char (point-min))
                                           (let ((begin (point)))
                                             (search-forward "\n\n")
                                             (delete-region begin (point))))
                                         (save-buffer)))))))

(provide 'setup)
;;; setup.el ends here
