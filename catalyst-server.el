;;; catalyst-server.el --- Major mode for running catalyst dev servers

;; Copyright (C) 2009
;;     Sebastian Willert

;; Author: Sebastian Willert
;; Keywords: Perl, Catalyst

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

(defgroup catalyst-server nil
  "Run a catalyst server process for this project via comint, parse output."
  :group 'perl)

;; ; cargo-culted from compile.el
;; (defconst catalyst-server-filename-regexp-alist-alist
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
;;   "Alist of values for `catalyst-server-filename-regexp-alist'.")
;;

; cargo-culted from compile.el
(defconst catalyst-server-filename-regexp-alist-alist
  '((log4perl
     "\\( +\\|\\[\\)\
\\([a-zA-Z0-9._/-]*\\.pm\\|[a-zA-Z0-9._/-]*\\.pl\
\\|[a-zA-Z0-9._/-]*\\.t\\|[a-zA-Z0-9._/-]+\\)\
\\( \\+\\| line \\|:\\)\
\\([0-9]+\\)\
\\( +\\|\\]\\)" 2 4))
  "Alist of values for `catalyst-server-filename-regexp-alist'.")

(defcustom catalyst-server-filename-regexp-alist
  (mapcar 'car catalyst-server-filename-regexp-alist-alist)
  "Alist that specifies how to match filenames in catalyst output."
  :group 'catalyst-server)

;; taken almost literally from dirvars.el
;; Location: http://www.lickey.com/env/elisp/dirvars.el
(defcustom catalyst-server-chase-remote t
  "Whether catalyst-server looks upward if in a remote filesystem.
Most of the process interaction will fail if this is turned off."
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'catalyst-server)

(defvar catalyst-server-last-server-buffer nil
  "Contains the buffer of the last server buffer that was started or visited
and is used to figure out if we have to start a new server or just switch to
the existing buffer")

(defun catalyst-server-find-upwards (file-name)
  "Find a file in the current directory or one of its parents.
Returns the fully qualified file name, or nil if it isn't found.
The `file-name' specifies the file name to search for."

  (if (and (not catalyst-server-chase-remote)
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

(defun catalyst-server-find-root ()
  "Tries to find the perl project root for the current dirctory"
  (let
      ((server-root (or
                     (catalyst-server-find-upwards "Makefile.PL")
                     (catalyst-server-find-upwards "dist.ini")
                    )))

    ; try to find server root last
    (if (and (not server-root) catalyst-server-last-server-buffer
             (buffer-live-p catalyst-server-last-server-buffer))
        (setq server-root (with-buffer catalyst-server-last-server-buffer
              (catalyst-server-find-root))))

    (if server-root (file-name-directory server-root))))

(defun catalyst-server-guess-server-module ()
  "Tries to guess the projects module name (like 'My::App')"
  (let
      ((catalyst-server-root (catalyst-server-find-root)))
    (assert catalyst-server-root nil
            "Can't find catalyst server root for %s" default-directory)
    (or
     (let*
        ((makefile (expand-file-name "Makefile.PL" catalyst-server-root))
         (distfile (expand-file-name "dist.ini" catalyst-server-root))
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
        ((makefile (expand-file-name "dist.ini" catalyst-server-root)))
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

(defun catalyst-server-guess-server-script ()
  "Tries to guess the projects server script (like 'myapp_server.pl')"
  (let
      ((server-module (catalyst-server-guess-server-module))
       (server-root (catalyst-server-find-root)))

    (assert server-root nil
            "Can't find catalyst server root for %s" default-directory)
    (assert server-module nil
            "Can't guess server module for %s" server-root)

    (let*
       ((script-prefix
         (downcase (replace-regexp-in-string "::" "_" server-module)))
       (script-name (concat script-prefix "_" "server.pl")))

      (car
       (remove-if
        'null
        (mapcar
         '(lambda (fn)
            (if (file-executable-p fn) fn))
         (mapcar
          '(lambda (dir) (expand-file-name (concat dir "/" script-name) server-root) )
          (cons "bin" (cons "sbin" (cons "script" nil)))
          ))
        )))))

(defun catalyst-server-kill-process (&optional force)
  "Tries to kill a running catalyst server process.
If the optional argument `force' is non-nil, GNU tools (ps, grep, awk,
xargs, kill) are used to to find and kill processes matching
`catalyst-server-guess-server-script'"
  (interactive "P")

  (let*
      ((module-name (catalyst-server-guess-server-module))
       (process-name (concat module-name " Server"))
       (commands-buffer-name (concat "* " process-name " *"))
       (buf (get-buffer commands-buffer-name))
       (server-cmd
        (file-name-nondirectory (catalyst-server-guess-server-script))))

    (if (and buf (get-buffer-process buf))
        (with-buffer buf (comint-kill-subjob)))

    (if (and force
             (assert server-cmd  nil "Failed to find catalyst server script"))
        (shell-command
         (concat "ps --no-headers -o pid= -o cmd -C grep -N | "
                 "grep " server-cmd " | awk '{print $1}' | xargs -r kill")))
      ))

(defun catalyst-server-start-or-show-process (&optional restart)
  "Switches to or starts a new catalyst server process that is managed
by `comint-mode'. If the optional argument `restart' is non-nil,
`catalyst-server-start' calls `catalyst-server-kill-process' to
get rid of any existing processes"
  (interactive "P")

  (let ((server-root (catalyst-server-find-root)))

    (assert server-root nil
            "Can't find catalyst server root for %s" default-directory)

    (let*
        ((module-name (catalyst-server-guess-server-module))
         (process-name (concat module-name " Server"))
         (commands-buffer-name (concat "* " process-name " *"))
         (buf (get-buffer commands-buffer-name))
         (server-cmd (catalyst-server-guess-server-script))
         (commands-window nil))

      (assert module-name nil "Failed to guess catalyst module name")
      (assert server-cmd  nil "Failed to find catalyst server script")

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

      (if restart (progn (catalyst-server-kill-process) (sleep-for 1)))

      (if (file-executable-p (concat server-root "/perl5/bin/mist-run"))
          (make-comint-in-buffer
           process-name buf (concat server-root "/perl5/bin/mist-run")
           nil "perl" server-cmd "-r" "-d")

        (make-comint-in-buffer
         process-name buf "perl"
         nil "-Mlocal::lib=perl5" server-cmd "-r" "-d"))

      (make-local-variable 'comint-output-filter-functions)

      (setq comint-output-filter-functions nil )


      (add-hook 'comint-output-filter-functions
                'comint-truncate-buffer) ;comint gets really slow otherwise

      (add-hook 'comint-output-filter-functions
                'comint-postoutput-scroll-to-bottom)

      (add-hook 'comint-output-filter-functions
                'catalyst-server-compilation-scan-buffer)

      (add-hook 'comint-output-filter-functions
                'ansi-color-process-output)

      (ansi-color-for-comint-mode-on)

      (setq catalyst-server-last-server-buffer buf)
)))

(defun catalyst-server-compilation-scan-buffer (original-output)
  "A `comint-output-filter-functions' hook to creates buttons
from file paths in the catalyst output (e.g. log messages)
according to `catalyst-server-filename-regexp-alist'"
  (let*
      ((start (marker-position comint-last-output-start))
       (end (marker-position
             (process-mark (get-buffer-process (current-buffer)))))
       (last-match start))

    (mapcar (lambda (item)
  (if (symbolp item)
      (setq item (cdr (assq item
          catalyst-server-filename-regexp-alist-alist))))
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
          ))) ) catalyst-server-filename-regexp-alist )))

(provide 'catalyst-server)
