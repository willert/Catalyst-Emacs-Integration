;;; plackup-server.el --- Major mode for running plackup dev servers

;; Copyright (C) 2009
;;     Sebastian Willert

;; Author: Sebastian Willert
;; Keywords: Perl, Plackup

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a pre-release version, nothing is stable at the moment,
;; although I am quite happy with using comint.el after a lot of
;; experimentation with other inferior interpreter or shell modes.
;; Most likely, I'll try to add a new major mode to allow for better
;; customization and add support for eproject.el by jrockway. Patches
;; and pull requests are always welcome :)

;;; Disclaimer:

;; I suck at elisp, so I can just hope the code is not too brain-damaged

(require 'ffap)
(require 'button)
(require 'ansi-color)
(require 'cl)

(defgroup plackup-server nil
  "Run a plackup server process for this project via comint, parse output."
  :group 'perl)

;; ; cargo-culted from compile.el
;; (defconst plackup-server/filename-regexp-alist-alist
;;   '((log4perl
;;      " +\\(\
;; /[a-zA-Z0-9._/-]*\.pm\
;; \\) +\\(\\+\\|line +\\)\\([0-9]+\\)" 1 3)
;;     (log4perl-relative
;;      " +\\(\
;; lib/[a-zA-Z0-9._/-]*\\.pm\\|\
;; t/[a-zA-Z0-9._/-]*\\.t\\|\
;; t/[a-zA-Z0-9._/-]*\\.pm\\|\
;; script/[a-zA-Z0-9._/-]*\\.pl\\|\
;; root/[a-zA-Z0-9._/-]*\\|\
;; bin/[a-zA-Z0-9._/-]*\\|\
;; sbin/[a-zA-Z0-9._/-]*\
;; \\) +\\(\\+\\|line +\\)\\([0-9]+\\)" 1 3)
;;     (stack-trace
;;      "\\[\\(/[a-zA-Z0-9._/-]*\\):\\([0-9]+\\)\\]" 1 2)
;;     (perl
;;      " at \\([^ \n]+\\) line \\([0-9]+\\)\
;; \\(?:[,.]\\|$\\| during global destruction\\.$\\)" 1 2))
;;   "Alist of values for `plackup-server/filename-regexp-alist'.")
;;

; cargo-culted from compile.el
(defconst plackup-server/filename-regexp-alist-alist
  '((log4perl
     "\
\\( +\\|\\[\\)\
\\(\
[a-zA-Z0-9._/-]*\\.pm\\|\
[a-zA-Z0-9._/-]*\\.pl\\|\
[a-zA-Z0-9._/-]*\\.m.\\|\
[a-zA-Z0-9._/-]*\\.t\\|\
[a-zA-Z0-9._/-][a-zA-Z0-9._/-]*\
\\)\
\\(\
 \\+\\|\
 line \\|\
:\
\\)\
\\([0-9][0-9]*\\)\
.*" 2 4))
  "Alist of values for `plackup-server/filename-regexp-alist'.")

(defcustom plackup-server/filename-regexp-alist
  (mapcar 'car plackup-server/filename-regexp-alist-alist)
  "Alist that specifies how to match filenames in plackup output."
  :group 'plackup-server)

;; taken almost literally from dirvars.el
;; Location: http://www.lickey.com/env/elisp/dirvars.el
(defcustom plackup-server/chase-remote t
  "Whether plackup-server looks upward if in a remote filesystem.
Most of the process interaction will fail if this is turned off."
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'plackup-server)

(defvar plackup-server/last-server-buffer nil
  "Contains the buffer of the last server buffer that was started or visited
and is used to figure out if we have to start a new server or just switch to
the existing buffer")

(defvar plackup-server/plackup-args ""
  "Additional parameter for plackup"
)

(make-variable-buffer-local 'plackup-server/plackup-args)

(defun plackup-server/find-upwards (file-name)
  "Find a file in the current directory or one of its parents.
Returns the fully qualified file name, or nil if it isn't found.
The `file-name' specifies the file name to search for."

  (if (and (not plackup-server/chase-remote)
           (file-remote-p default-directory))
      nil
    ;; Chase links in the source file and search in the dir where it
    ;; points.

    (setq dir-name (or (and buffer-file-name
                            (file-name-directory (file-chase-links
                                                  buffer-file-name)))
                       default-directory))
    ;; Chase links before visiting the file.  This makes it easier to
    ;; use a single file for several related directories.
    (setq dir-name (file-chase-links dir-name))
    (setq dir-name (expand-file-name dir-name))
    ;; Move up in the dir hierarchy till we find a change log file.
    (let ((file1 (concat dir-name file-name))
          parent-dir)
      (while (and (not (file-exists-p file1))
                  (progn (setq parent-dir
                               (file-name-directory
                                (directory-file-name
                                 (file-name-directory file1))))
                         ;; Give up if we are already at the root dir.
                         (not (string= (file-name-directory file1)
                                       parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file1 (expand-file-name file-name parent-dir)))
      ;; If we found the file in a parent dir, use that.  Otherwise,
      ;; return nil
      (if (or (get-file-buffer file1) (file-exists-p file1))
          file1
        nil))))

(defun plackup-server/find-root ()
  "Tries to find the perl project root for the current dirctory"
  (let
      ((server-root (or
                     (plackup-server/find-upwards "Makefile.PL")
                     (plackup-server/find-upwards "dist.ini")
                    )))

    ; try to find server root last
    (if (and (not server-root) plackup-server/last-server-buffer
             (buffer-live-p plackup-server/last-server-buffer))
        (setq server-root (with-current-buffer plackup-server/last-server-buffer
              (plackup-server/find-root))))

    (if server-root (file-name-directory server-root))))

(defun plackup-server/guess-server-module ()
  "Tries to guess the projects module name (like 'My::App')"
  (let
      ((plackup-server/root (plackup-server/find-root)))
    (assert plackup-server/root nil
            "Can't find plackup server root for %s" default-directory)
    (or
     (let*
        ((makefile (expand-file-name "Makefile.PL" plackup-server/root))
         (distfile (expand-file-name "dist.ini" plackup-server/root))
         (meta (if (file-exists-p makefile) makefile distfile)))
      (with-temp-buffer
        (insert-file-contents-literally meta)
        (goto-char (point-min))
        (if (string-match
             "\\(^ *\\|; *\\)name *(? *\\('\\|\"\\)\\([a-zA-Z0-9:-]+\\)\\2"
             (buffer-string))
            (replace-regexp-in-string
             "-" "::" (match-string 3 (buffer-string)))
          ))
      )

     (let
        ((makefile (expand-file-name "dist.ini" plackup-server/root)))
      (with-temp-buffer
        (insert-file-contents-literally makefile)
        (goto-char (point-min))
        (if (string-match
             "name *= *\\([a-zA-Z0-9:-]+\\)"
             (buffer-string))
            (replace-regexp-in-string
             "-" "::" (match-string 1 (buffer-string)))
          ))
      )

    ))
  )

(defun plackup-server/guess-psgi-app ()
  "Tries to guess the projects server script (like 'myapp.psgi')"
  (let
      ((server-module (plackup-server/guess-server-module))
       (server-root (plackup-server/find-root)))

    (assert server-root nil
            "Can't find plackup server root for %s" default-directory)
    (assert server-module nil
            "Can't guess server module for %s" server-root)

    (let*
       ((script-prefix
         (downcase (replace-regexp-in-string "::" "_" server-module)))
       (script-name (concat script-prefix ".psgi")))

      (car
       (remove-if
        'null
        (mapcar
         '(lambda (fn)
            (if (file-exists-p fn) fn))
         (mapcar
          '(lambda (psgi-app) (expand-file-name psgi-app server-root) )
          (list script-name "app.psgi")
          ))
        )))))

(defun plackup-server/clear-comint-buffer ()
  "Clears plackup buffer"
  (let*
      ((module-name (plackup-server/guess-server-module))
       (process-name (concat module-name " Plackup Server"))
       (commands-buffer-name (concat "* " process-name " *"))
       (buf (get-buffer commands-buffer-name))
       (comint-buffer-maximum-size 0))
    (if (not buf)
        (message "No server buffer found")
      (switch-to-buffer buf t)
      (let ((comint-buffer-maximum-size 0))
        (ignore-errors (comint-truncate-buffer))))))

(defun plackup-server/kill-process (&optional force)
  "Tries to kill a running plackup server process.
If the optional argument `force' is non-nil, GNU tools (ps, grep, awk,
xargs, kill) are used to to find and kill processes matching
`plackup-server/guess-psgi-app'"
  (interactive "P")

  (let*
      ((module-name (plackup-server/guess-server-module))
       (process-name (concat module-name " Plackup Server"))
       (commands-buffer-name (concat "* " process-name " *"))
       (buf (get-buffer commands-buffer-name))
       (psgi-app
        (file-name-nondirectory (plackup-server/guess-psgi-app))))

    (if (and buf (get-buffer-process buf))
        (with-current-buffer buf (comint-kill-subjob)))

    (if (and force
             (assert psgi-app  nil "Failed to find plackup server script"))
        (shell-command
         (concat "ps --no-headers -o pid= -o cmd -C grep -N | "
                 "grep " psgi-app " | awk '{print $1}' | xargs -r kill")))
      ))

(defun plackup-server/start-or-show-process (&optional restart)
  "Switches to or starts a new plackup server process that is managed
by `comint-mode'. If the optional argument `restart' is non-nil,
`plackup-server/start' calls `plackup-server/kill-process' to
get rid of any existing processes"
  (interactive "P")

  (let ((server-root (plackup-server/find-root)))

    (assert server-root nil
            "Can't find psgi app for %s" default-directory)

    (let*
        ((module-name (plackup-server/guess-server-module))
         (process-name (concat module-name " Plackup Server"))
         (commands-buffer-name (concat "* " process-name " *"))
         (buf (get-buffer commands-buffer-name))
         (psgi-app (plackup-server/guess-psgi-app))
         (commands-window nil)
         (local-args plackup-server/plackup-args))

      (assert module-name nil "Failed to guess plackup module name")
      (assert psgi-app  nil "Failed to find plackup server script")

      (if buf

          (progn
            (walk-windows
             (lambda (win)
               (if (string=
                    (buffer-name (window-buffer win)) commands-buffer-name)
                   (setq commands-window win))))
            (if commands-window
                (select-window commands-window)
              (switch-to-buffer commands-buffer-name t)))

        (setq buf (switch-to-buffer commands-buffer-name t)))

      ;; I guess most of the rest could only be called after
      ;; buffer creation, but we'd need a way to handle
      ;; successful window walks together with restart commands
      ;; somehow. I told you, I suck at elisp

      (setq default-directory server-root)

      (if restart (progn
                    (plackup-server/kill-process)
                    (sleep-for 1)
                    (plackup-server/clear-comint-buffer)
                    ))

      (if (file-executable-p (concat server-root "/perl5/bin/wecare-plack"))
          (make-comint-in-buffer
           process-name buf (concat server-root "/perl5/bin/wecare-plack") nil )
        (if (file-executable-p (concat server-root "/perl5/bin/mist-run"))
            (make-comint-in-buffer
             process-name buf (concat server-root "/perl5/bin/mist-run") nil
             "plackup" "-r" "-R" (concat server-root "etc")
             "-E" "development"
             "--access-log" "/dev/null"
             psgi-app)

          (make-comint-in-buffer
           process-name buf "perl" nil "-Mlocal::lib=perl5"
           "plackup" "-r" "-R" "etc"
           "-E" "development"
           "--access-log" "/dev/null"
           psgi-app)))

      (compilation-minor-mode)

      ;; let C-c C-c pass through for comint-mode's abort function
      (define-key compilation-minor-mode-map (kbd "C-c C-c" ) nil)

      (make-local-variable 'comint-output-filter-functions)
      (setq comint-output-filter-functions nil )

      (make-local-variable 'comint-buffer-maximum-size)
      (setq comint-buffer-maximum-size 4096)

      (add-hook 'comint-output-filter-functions
                'plackup-server/clear-on-restart) ;comint gets really slow otherwise

      (add-hook 'comint-output-filter-functions
                'comint-truncate-buffer t) ;comint gets really slow otherwise

      (add-hook 'comint-output-filter-functions
                'ansi-color-process-output t)

      (add-hook 'comint-output-filter-functions
                'comint-postoutput-scroll-to-bottom t)

      (ansi-color-for-comint-mode-on)

      (setq plackup-server/last-server-buffer buf)
)))

(defun plackup-server/clear-on-restart (original-output)
  (let*
      ((regex "Successfully killed! Restarting the new server process")
       (last-match nil))

    (while (string-match regex (buffer-string) (if last-match (+ last-match 1) 10))
      (setq last-match (match-beginning 0)))

    (if last-match (delete-region 1 (+ last-match 1)))))

(defun plackup-server/compilation-scan-buffer (original-output)
  "A `comint-output-filter-functions' hook to creates buttons
from file paths in the plackup output (e.g. log messages)
according to `plackup-server/filename-regexp-alist'"
  (let*
      ((start (marker-position comint-last-output-start))
       (end (marker-position
             (process-mark (get-buffer-process (current-buffer)))))
       (last-match start))

    (mapcar (lambda (item)
  (if (symbolp item)
      (setq item (cdr (assq item
          plackup-server/filename-regexp-alist-alist))))
    (let
        ((regex    (nth 0 item))
         (file-pos (nth 1 item))
         (line-pos (nth 2 item)))

      (while (and regex (< last-match (point-max))
                  (string-match regex (buffer-string) last-match))

        (let
            ((fn-start (+ 2 (match-beginning 0)))
             (fn-stop  (+ 1 (match-end 0))))
          (setq last-match (+ 1 fn-stop))
          (make-button
           fn-start fn-stop
           'action (defun visit-file
                     (btn) (interactive)
                     (find-file (button-get btn 'file-path-to-open))
                     (goto-line
                      (string-to-number (button-get btn 'line-numer-to-show)))
                     )
           'follow-link t
           'file-path-to-open (substring-no-properties
                               (buffer-string)
                               (match-beginning file-pos)
                               (match-end file-pos))
           'line-numer-to-show (substring-no-properties
                                (buffer-string)
                                (match-beginning line-pos)
                                (match-end line-pos)))
          ))) ) plackup-server/filename-regexp-alist )))

(provide 'plackup-server)
