;;; -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Ketan Kanishka"
      user-mail-address "ketan.kanishka@nyu.edu")

;;; Utility functions
;; Due to name visibility issues, this section needs to be at the top.

(require 'cl)

;; 12-hour time -> 24-hour time
(defun pm (hour) (mod (+ hour 12) 24))
(defun am (hour) hour)

(defun between (start item end)
  (and (<= start item)
       (<= item end)))

(defun kk/load-doom-theme (&optional theme)
  "Load the currently set `doom-theme'. If THEME is provided, set it to `doom-theme' first."
  (setq doom-theme (or theme doom-theme))
  (load-theme doom-theme t nil))

(defun nshuffle (sequence)
  "Shuffle SEQUENCE in place. Picked up from somewhere on the internet."
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

;;; Org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq org-hide-emphasis-markers t)


;;; LSP
;; TODO this does not isolate this to prog-mode-map
(map! :map prog-mode-map
      (:leader
       (:prefix ("l" . "lsp")
        :desc "start lsp server"   :n "l" #'lsp!
        :desc "restart lsp server" :n "r" #'lsp-workspace-restart
        :desc "stop lsp server"    :n "k" #'lsp-workspace-shutdown)))

;;; Filesystem
(add-hook! dired-mode #'dired-hide-details-mode)

;;; Evil
(use-package! evil
  :config
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t))

;;; LSP

(setq lsp-ui-doc-enable nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable nil
      lsp-eldoc-enable-hover t
      lsp-signature-render-documentation t)

;;; UI

(setq display-line-numbers-type t)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(use-package! highlight-indent-guides
  :config
  ;; stop doom from autoloading this
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))


;; put the window on the second monitor if present

(setq kk/single-monitor-width 1920)

(when (display-graphic-p)
  (let* ((second-monitor-present? (> (x-display-pixel-width) kk/single-monitor-width))
         (left-pos (* kk/single-monitor-width (if second-monitor-present? 1 0))))
    (setq initial-frame-alist `((top . 23) (left . ,left-pos) (height . 56) (width . 272)))))

(message "initial-frame-alist: %s" initial-frame-alist)

;;;; Font

(setq doom-font (font-spec :family "Iosevka" :size 14))

;; fix the weird fixed-pitch font in Info manuals
(doom-themes-set-faces nil
  '(fixed-pitch-serif :inherit 'default))

(setq doom-font-increment 1)

;;;; Theme
;;;;; Timed changes

(defcustom timed-themes/theme-timings
  `((doom-flatwhite         . (,(am 6) . ,(pm 3)))
    (doom-monokai-ristretto . (,(pm 4) . ,(pm 8)))
    (doom-sourcerer         . (,(pm 9) . ,(am 5))))
  "Alist mapping themes to their suitable timings.
Entries should be of the form (THEME . (START-TIME . END-TIME))."
  :type '(alist :key-type symbol :value-type (alist :key-type integer :value-type integer))
  :group 'timed-themes)

(defcustom timed-themes/default-theme 'wombat
  "Fallback theme in case no suitable timed theme is found."
  :type 'symbol
  :group 'timed-themes)

(defcustom timed-themes/change-if-timeout nil
  "Should the theme be changed if a different theme has been loaded manually and the user prompt times out?"
  :type 'boolean
  :group 'timed-themes)

(defun timed-themes/theme-for-time (&optional hour-diff)
  "Get appropriate theme for current time (offset by HOUR-DIFF hours) from `timed-theme/theme-timings'."
  (let* ((hour (mod (+ (or 0 hour-diff)
                       (decoded-time-hour (decode-time (current-time))))
                    24))
         (check-theme
          (lambda (item)
            (cl-destructuring-bind (_theme . (start-hour . end-hour)) item
              (if (<= start-hour end-hour)
                  (between start-hour hour end-hour)
                (or (between end-hour hour 23)
                    (between 0 hour end-hour))))))
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
         (change-to-curr-theme-for-time
          (and (not (equal doom-theme curr-theme-for-time))
               (or (equal doom-theme prev-theme-for-time)
                   (y-or-n-p-with-timeout
                    (format "Current theme '%s' has been set manually. Do you want to set it to the timed theme %s"
                            doom-theme curr-theme-for-time)
                    5 timed-themes/change-if-timeout)))))
    (if change-to-curr-theme-for-time
        (progn (setq doom-theme curr-theme-for-time)
               (kk/load-doom-theme))
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

(setq doom-theme (timed-themes/theme-for-time))
(timed-themes-minor-mode)

;;;;; Random theme switching

(defvar kk/random-themes-list nil)
(defvar kk/random-theme-idx nil)

(defun kk/set-random-theme-idx (idx)
  (interactive)
  (let ((idx (mod idx (length kk/random-themes-list))))
    (setq kk/random-theme-idx idx)
    (load-theme (elt kk/random-themes-list kk/random-theme-idx) t nil)))

(defun kk/random-themes-reshuffle ()
  (interactive)
  (setq kk/random-themes-list (or kk/random-themes-list (apply #'vector (custom-available-themes))))
  (nshuffle kk/random-themes-list)
  (kk/set-random-theme-idx 0))

(defvar kk/random-theme-at-start 'default
  "The theme that `hydra-random-themes' was entered with, in case the user wants to go back to it.")

(defun hydra-random-themes-gen-docstring (num-surrounding)
  "Generate docstring for `hydra-random-themes' with NUM-SURROUNDING themes before and after (each) the current one."

  (let* ((num-themes (length kk/random-themes-list))
         (-frame-width- (frame-width))
         (rows (--reduce-r-from
                (cl-destructuring-bind (theme-row num-row) acc
                  (let* ((theme-idx (mod (+ it kk/random-theme-idx) num-themes))
                         (theme-name (symbol-name (elt kk/random-themes-list theme-idx)))
                         (is-current (zerop it))
                         (theme-name-decorated (propertize theme-name
                                                           'face
                                                           (if is-current
                                                               'font-lock-preprocessor-face
                                                             'font-lock-comment-face)))
                         (theme-num (s-center (string-width theme-name-decorated)
                                              (if is-current
                                                  (format "[%d/%d]" (1+ theme-idx) num-themes)
                                                ""))))
                    (list (cons theme-name-decorated theme-row) (cons theme-num num-row))))

                (list nil nil)
                (number-sequence (- num-surrounding) num-surrounding))))

    (cl-destructuring-bind (theme-row num-row) rows
      (apply #'s-concat
             (--map
              (s-concat (s-center -frame-width- (s-join "   " it)) "\n")
              (list (-concat '("...") theme-row '("..."))
                    (-concat '("   ")  num-row  '("   "))))))))

(defun kk/search-random-theme ()
  (interactive)
  (-> (completing-read "Find theme: "
                       (mapcar #'symbol-name kk/random-themes-list)
                       nil t)
      (intern)
      (cl-position kk/random-themes-list)
      (kk/set-random-theme-idx)))

(defhydra hydra-random-themes
  (:color red
   :body-pre
   (progn
     (setq kk/random-theme-at-start doom-theme)
     (if kk/random-themes-list
         (load-theme (elt kk/random-themes-list kk/random-theme-idx) t nil)
       (kk/random-themes-reshuffle))))
  "
%s(hydra-random-themes-gen-docstring 3)
"
  ("k" (kk/set-random-theme-idx (1- kk/random-theme-idx)) "prev-theme")
  ("j" (kk/set-random-theme-idx (1+ kk/random-theme-idx)) "next-theme")
  ("r" kk/random-themes-reshuffle "randomize")
  ("/" kk/search-random-theme "search")
  ("q" (load-theme kk/random-theme-at-start t nil) "reset to initial" :color blue)
  ("." nil "confirm and quit" :color blue))

;;;; Modeline

(after! doom-modeline
  (setq doom-modeline-hud t
        doom-modeline-major-mode-icon t
        ;; reduce the size of icons in the modeline so that it doesn't get cut off at the end
        all-the-icons-scale-factor 1.1)

  (remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-themes-set-faces nil
    ;; Modeline icons use this face, which has :inherit `success' and `bold'.
    ;; I don't know any way of removing the :inherit `bold' without just specifying everything else it inherits from.
    '(doom-modeline-info :inherit 'success)))

;;; Language-specific configs
;;;; OCaml

(defun kk/find-dune-file ()
  "Find the corresponding dune file for the current file."
  (interactive)
  (let* ((curr-dir (f-dirname (ff-buffer-file-name (current-buffer))))
         (dune-file (f-join curr-dir "dune")))
    (if (f-exists? dune-file)
        (find-file dune-file)
      (message "No dune file found in %s" curr-dir))))

(after! tuareg
  (map! :map tuareg-mode-map
        :desc "format region" :v "gq" #'ocp-indent-region
        :localleader
        ;; switch back-and-forth b/w ocaml and dune file
        :desc "visit corresp. dune file" "d" #'kk/find-dune-file)

  (add-hook! 'tuareg-mode-hook
    (setq-local
     +default-want-RET-continue-comments nil
     +evil-want-o/O-to-continue-comments nil)))

(after! dune
  ;; switch back-and-forth b/w ocaml and dune file
  (map! :map dune-mode-map
        :localleader
        :desc "visit previous file" "d" #'evil-switch-to-windows-last-buffer)
  (when (featurep! :editor lispy) (add-hook 'dune-mode-hook #'lispyville-mode)))

;;;; Minor-modes
;;;;; outshine

(after! outshine
  (map! :map outshine-mode-map
        :desc "narrow to subtree" :n "zn" #'outshine-narrow-to-subtree))

;;; Assorted

(setq confirm-kill-emacs nil)

;;; Keybindings

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

;; `doom/window-enlargen' just looks ugly, and there seems to be
;; no benefit over `doom/window-maximize-buffer'. So we set that
;; to the keybinding I'm most used to.
;; NOTE Both of these are undo'd by `winner-undo' (~SPC w u~).
;; `winner-undo' is really powerful. Try to use it more.
(map!
 (:leader
  (:prefix "w"
   :desc "maximize buffer" :n "o" #'doom/window-maximize-buffer
   :desc "+zoom"           :n "z" #'+hydra/text-zoom/body
   :desc "+navi"           :n "." #'+hydra/window-nav/body
   ;; remove previous one-shot bindings.
   ;; the hydra takes care of all these cases much better.
   "+" nil
   "-" nil
   "<" nil
   ">" nil)))

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

(map!
 :desc "scroll other window down"       :n "M-j"   (cmd! (scroll-other-window 2))
 :desc "scroll other window down a lot" :n "M-S-j" (cmd! (scroll-other-window))
 :desc "scroll other window up"         :n "M-k"   (cmd! (scroll-other-window-down 2))
 :desc "scroll other window up a lot"   :n "M-S-k" (cmd! (scroll-other-window-down)))

(map!
 :desc "format region" :v "gq" #'+format/region ; This is the keybinding I always reach for to format a region
 :desc "format buffer" :n "gQ" #'+format/buffer)

(map! :leader :desc "random-themes-hydra" :n "h T" #'hydra-random-themes/body)

;;; Notes

;; TODO Check out rougier's `svg-lib' - seems pretty fun
