;;; timed-themes.el --- change themes according to time of day
;;; -*- lexical-binding: t; -*-

;;; Author: Ketan Kanishka

;;; Commentary:
;;; This package changes your emacs theme during the day. The need arose from
;;; trying to auto-switch day and night themes, but I found that it's quite nice
;;; to set particular themes for the afternoon, early evening etc to match the
;;; ambient light.

;;; Code:

(require 'cl-lib)

(defgroup timed-themes nil
  "Change themes automatically by time."
  :group 'themes)

(defcustom timed-themes/theme-timings
  `((modus-operandi . 6)
    (wombat . 4)
    (modus-vivendi . 21))
  "Alist mapping themes to their suitable timings.
Entries should be of the form (THEME . START-HOUR).
START-HOUR is inclusive."
  :type '(alist :key-type symbol :value-type integer)
  :group 'timed-themes)

(defcustom timed-themes/change-theme-if-manually-set t
  "Should the theme be changed if a different theme has been set manually?"
  :type '(choice (const :tag "Don't change" nil)
                 (const :tag "Prompt yes or no" 'ask)
                 (const :tag "Change without asking" t))
  :group 'timed-themes)

(defcustom timed-themes/change-theme-default-on-ask-timeout nil
  "Should the theme be changed if the user prompt to change manually set theme times out?"
  :type 'boolean
  :group 'timed-themes)

(defun timed-themes/between (start item end)
  (and (<= start item)
       (<= item end)))

(defun timed-themes/theme-for-time (&optional hour-diff)
  "Get appropriate theme for current time (offset by HOUR-DIFF hours) from `timed-theme/theme-timings'."
  (when (seq-empty-p timed-themes/theme-timings) (user-error "Theme timings are empty!"))
  (require 'dash)
  (let* ((curr-hour (mod (+ (or hour-diff 0)
                       (decoded-time-hour (decode-time (current-time))))
                    24)))
    (car (or (--last (<= (cdr it) curr-hour) timed-themes/theme-timings)
             (last timed-themes/theme-timings)))))

(defun timed-themes/load-theme-for-time (&optional force)
  "Load appropriate theme for time if the current theme hasn't been changed."
  (interactive)
  (let* ((prev-theme-for-time (timed-themes/theme-for-time -1))
         (curr-theme-for-time (timed-themes/theme-for-time))
         (loaded-theme (car-safe custom-enabled-themes))
         (change-theme
          (and (not (equal loaded-theme curr-theme-for-time))
               (or
                force
                (not loaded-theme)
                (called-interactively-p 'any) ;; force if called interactively
                (equal loaded-theme prev-theme-for-time)
                (pcase timed-themes/change-theme-if-manually-set
                  (`ask (y-or-n-p-with-timeout
                         (format "Current theme '%s' has been set manually. Do you want to set it to the timed theme %s"
                                 loaded-theme curr-theme-for-time)
                         5 timed-themes/change-theme-default-on-ask-timeout))
                  (`nil nil)
                  (`t t))))))
    (when change-theme
      (load-theme curr-theme-for-time t nil))))

(defvar timed-themes/change-timer nil
  "Timer to change themes periodically.")

(defvar timed-themes/change-time 3600
  "Number of seconds per which to check for theme changes.")

(define-minor-mode timed-themes-minor-mode
  "Minor mode to periodically change themes."
  :global t
  :group 'timed-themes
  :lighter " TimedThemes"
  (when timed-themes/change-timer (cancel-timer timed-themes/change-timer))
  (setq timed-themes/change-timer nil)
  (when timed-themes-minor-mode
    (timed-themes/load-theme-for-time t)
    (setq timed-themes/change-timer
          (run-with-timer nil timed-themes/change-time #'timed-themes/load-theme-for-time))))


(provide 'timed-themes)
;;; timed-themes.el ends here
