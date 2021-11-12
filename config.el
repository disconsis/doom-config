;;; -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Ketan Kanishka"
      user-mail-address "ketan.kanishka@nyu.edu")

;;; $ Org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq org-hide-emphasis-markers t)


;;; $ Keybindings

(setq doom-leader-key "SPC"
      doom-leader-alt-key "M-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; learned habit from IntelliJ and VS Code
(map! :nv "C-/" #'evilnc-comment-or-uncomment-lines)

(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

;; easier window movement
(map!
 :n "C-h" #'evil-window-left
 :n "C-j" #'evil-window-down
 :n "C-k" #'evil-window-up
 :n "C-l" #'evil-window-right)

;; take back ~s~
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; vim-vinegar
(map! :m "-" #'dired-jump)

;; keep keybinds consistent even in emacs' different terminals.
;; for this, unmap C-k and C-j from moving between prompts.
;; these are still available through ~g k~ and ~g j~
(map! :map term-mode-map
 :n "C-k" nil
 :n "C-j" nil)

;;; $ Filesystem
(add-hook! dired-mode #'dired-hide-details-mode)

;;; $ Evil
(use-package! evil
  :config
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t))

;;; $ UI

(setq display-line-numbers-type t)

(use-package! highlight-indent-guides
  :config
  ;; stop doom from autoloading this
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

;;; $$ Font
(setq doom-font (font-spec :family "Iosevka" :size 12))

;;; $$ Theme

(defun pm (hour) (mod (+ hour 12) 24))
(defun am (hour) hour)
(defun between (start item end)
  (and (<= start item)
       (<= item end)))
(defun kk/load-doom-theme ()
  "Load `doom-theme'"
  (interactive)
  (doom--load-theme-a #'load-theme doom-theme t nil))

(setq kk/theme-timings `((doom-flatwhite         . (,(am 6) . ,(pm 3)))
                         (doom-monokai-ristretto . (,(pm 4) . ,(pm 8)))
                         (doom-miramare          . (,(pm 9) . ,(am 5)))))

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

(defun kk/load-theme-for-time ()
  "Load appropriate theme for current time."
  (interactive)
  (let ((new-theme (kk/theme-for-time)))
    (when (or (not (equal new-theme doom-theme))
              (not (custom-theme-enabled-p doom-theme)))
      (setq doom-theme new-theme)
      (kk/load-doom-theme))))

;; check every hour
(run-at-time t 3600 #'kk/load-theme-for-time)

;;; Assorted

(setq confirm-kill-emacs nil)
