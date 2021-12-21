;;; timed-themes.el --- change themes according to time of day
;;; -*- lexical-binding: t; -*-

;;; Author: Ketan Kanishka

;;; Commentary:
;;; This package changes your emacs theme during the day. The need arose from
;;; trying to auto-switch day and night themes, but I found that it's quite nice
;;; to set particular themes for the afternoon, early evening etc to match the
;;; ambient light.

;;; Code:
;;;; Requirements

(require 'cl-lib)

(defcustom timed-themes/theme-timings
  `((modus-operandi . (6  . 15))
    (wombat         . (4  . 20))
    (modus-vivendi  . (21 . 5)))
  "Alist mapping themes to their suitable timings.
Entries should be of the form (THEME . (START-HOUR . END-HOUR)).
Both START-HOUR and END-HOUR are inclusive."
  :type '(alist :key-type symbol :value-type (alist :key-type integer :value-type integer))
  :group 'timed-themes)

(defcustom timed-themes/default-theme 'wombat
  "Fallback theme in case no suitable timed theme is found."
  :type 'symbol
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
  (let* ((hour (mod (+ (or 0 hour-diff)
                       (decoded-time-hour (decode-time (current-time))))
                    24))
         (check-theme
          (lambda (item)
            (cl-destructuring-bind (_theme . (start-hour . end-hour)) item
              (if (<= start-hour end-hour)
                  (timed-themes/between start-hour hour end-hour)
                (or (timed-themes/between end-hour hour 23)
                    (timed-themes/between 0 hour end-hour))))))
         (found-item (seq-find check-theme timed-themes/theme-timings)))
    (if (not found-item)
        (progn
          (warn "No suitable theme found in `timed-themes/theme-timings' for hour = %s. Defaulting to %s theme" hour timed-themes/default-theme)
          timed-themes/default-theme)
      (car found-item))))

(defun timed-themes/load-theme-for-time ()
  "Load appropriate theme for time if the current theme hasn't been changed."
  (interactive)
  (let* ((prev-theme-for-time (timed-themes/theme-for-time -1))
         (curr-theme-for-time (timed-themes/theme-for-time))
         (loaded-theme (and custom-enabled-themes (car custom-enabled-themes)))
         (change-to-curr-theme-for-time
          (and (not (equal loaded-theme curr-theme-for-time))
               (or
                (not loaded-theme)
                (equal loaded-theme prev-theme-for-time)
                (cl-case timed-themes/change-theme-if-manually-set
                  (nil nil)
                  (t t)
                  ('ask (y-or-n-p-with-timeout
                         (format "Current theme '%s' has been set manually. Do you want to set it to the timed theme %s"
                                 loaded-theme curr-theme-for-time)
                         5 timed-themes/change-theme-default-on-ask-timeout)))))))
    (if change-to-curr-theme-for-time
        (load-theme curr-theme-for-time t nil)
      (message "Not changing theme"))))

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
    (setq timed-themes/change-timer
          (run-with-timer nil timed-themes/change-time #'timed-themes/load-theme-for-time))))


(provide 'timed-themes)
;;; timed-themes.el ends here
