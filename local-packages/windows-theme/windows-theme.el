;;; windows-theme.el --- sync theme with windows
;;; -*- lexical-binding: t; -*-

;;; Author: Ketan Kanishka

;;; Commentary:
;;; This package syncs your emacs theme (under wsl) with the host windows theme.
;;; It relies on AutoDarkMode updating a file with the windows theme whenever it changes.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 's)
(require 'inotify)

(defgroup windows-theme nil
  "Sync theme with windows"
  :group 'theme)

(defcustom windows-theme/themes
  '((light . modus-operandi)
    (dark  . modus-vivendi)
    (error . adwaita))
  "Mapping of windows themes to emacs themes.
Should have a entries for 'light, 'dark,
and 'error (for unexpected erroneous cases)."
  :group 'windows-theme)

(defcustom windows-theme/theme-file
  "~/.windows_theme"
  "File to watch for windows theme changes."
  :type 'string
  :group 'windows-theme)

(defun windows-theme/file-contents (file)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents file)
          (buffer-string))
      (file-error
       (message "Unable to read file %s" filename)
       nil))))

(cl-defun windows-theme/event-handler ((_watch-hdl _aspects file _cookie))
  (windows-theme/update file))

(defun windows-theme/update (file)
  "Check file to see if a theme change is required."
  (let* ((win-theme
          (pcase (s-trim (windows-theme/file-contents file))
            ("light" 'light)
            ("dark" 'dark)
            (_ 'error)))
         (emacs-theme
          (or (alist-get win-theme windows-theme/themes)
              (progn
                (warn "No entry for %s in `windows-theme/themes'. Defaulting to the default theme." win-theme)
                'default))))
    (unless (memq emacs-theme custom-enabled-themes)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme emacs-theme 'no-confirm))))

(defvar windows-theme/inotify-handle nil
  "Watch handle received from inotify.")

(defun windows-theme/cancel-handle ()
  (when windows-theme/inotify-handle
    (inotify-rm-watch windows-theme/inotify-handle)
    (setq windows-theme/inotify-handle nil)))

(defun windows-theme/turn-on ()
  (windows-theme/cancel-handle)
  (let ((file (expand-file-name windows-theme/theme-file)))
    (unless file
      (user-error "`windows-theme/theme-file' needs to be set before starting `windows-theme-minor-mode'."))
    (unless (file-exists-p file)
      ;; ensure file exists so we can watch it
      ;; NOTE: we maybe should provide a default here,
      ;; but let's just go with the 'error case for now by keeping the file empty
      (make-empty-file file))
    (unless (file-readable-p file)
      (user-error "`windows-theme/theme-file' must be readable."))
    (setq windows-theme/inotify-handle
          (inotify-add-watch file '(modify) #'windows-theme/event-handler))
    ;; update once since the callback won't run until the file is modified
    (windows-theme/update file)))

(defun windows-theme/turn-off ()
  (windows-theme/cancel-handle))

(define-minor-mode windows-theme-minor-mode
  "Minor mode to sync theme with windows."
  :global t
  :group 'windows-theme
  :lighter " WinTheme"
  (if windows-theme-minor-mode
      (windows-theme/turn-on)
    (windows-theme/turn-off)))

;; (define-minor-mode windows-theme-minor-mode
;;   "Minor mode to periodically change theme."
;;   :global t
;;   :group 'windows-theme
;;   :lighter " WindowsTheme"
;;   (when windows-theme/change-timer (cancel-timer windows-theme/change-timer))
;;   (setq windows-theme/change-timer nil)
;;   (when windows-theme-minor-mode
;;     (windows-theme/load-theme-for-time t)
;;     (setq windows-theme/change-timer
;;           (run-with-timer nil windows-theme/change-time #'windows-theme/load-theme-for-time))))


(provide 'windows-theme)
;;; windows-theme.el ends here
