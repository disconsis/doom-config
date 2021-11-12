;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Ketan Kanishka"
      user-mail-address "ketan.kanishka@nyu.edu")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq doom-leader-key "SPC"
      doom-leader-alt-key "M-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(setq org-hide-emphasis-markers t)

(map! :leader "SPC" #'counsel-M-x)

;;; $ UI

;;; $$ Font
(setq doom-font (font-spec :family "Iosevka" :size 12))

;;; $$ Theme

(defun pm (hour) (mod (+ hour 12) 24))
(defun am (hour) hour)
(defun between (start item end)
  (and (<= start item)
       (<= item end)))

(setq kk/theme-timings `((doom-flatwhite         . (,(am 6) . ,(pm 3)))
                         (doom-monokai-ristretto . (,(pm 4) . ,(pm 8)))
                         (doom-homage-black      . (,(pm 9) . ,(pm 12)))
                         (doom-ayu-mirage        . (,(am 1) . ,(am 5)))))

(defun kk/theme-for-time ()
  "Get appropriate theme for current time from `kk/theme-timings'."
  (let* ((curr-hour (decoded-time-hour (decode-time (current-time))))
         (check-theme
            (lambda (item)
              (cl-destructuring-bind (_theme . (start-hour . end-hour)) item
                  (if (<= start-hour end-hour)
                      (between start-hour curr-hour end-hour)
                    (or (between end-hour curr-hour 23)
                        (between 0 curr-hour end-hour))))))
         (found-item (seq-find check-theme kk/theme-timings))
         (default-theme 'default))
    (if (not found-item)
        (progn
          (warn "No suitable theme found in `kk/theme-timings' for hour = %s. Defaulting to %s theme" curr-hour default-theme)
          default-theme)
      (car found-item))))

(setq doom-theme (kk/theme-for-time))

(defun kk/reload-theme-for-time ()
  (setq doom-theme (kk/theme-for-time))
  (doom/reload-theme))

;; check every hour
(run-at-time t 3600 #'kk/reload-theme-for-time)
