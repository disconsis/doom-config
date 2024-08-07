;;; -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Ketan Kanishka"
      user-mail-address "ketan.kanishka@nyu.edu")

;;; Utility functions
;; Due to name visibility issues, this section needs to be at the top.

;; 12-hour time -> 24-hour time
(defun pm (hour) (mod (+ hour 12) 24))
(defun am (hour) hour)

(defun my/load-doom-theme (&optional theme)
  "Load the currently set `doom-theme'. If THEME is provided, set it to `doom-theme' first."
  (setq doom-theme (or theme doom-theme))
  (load-theme doom-theme t nil))

(defun silently (fn)
  "Run FN without showing any messages in echo area."
  (let ((inhibit-message t))
    (funcall fn)))

(defun my/custom-theme-set-faces-test! (theme &rest specs)
  "Function to test theme modifications before committing theme with `custom-theme-set-faces!'."
  (let (custom--inhibit-theme-enable)
    (dolist (theme (ensure-list (or theme 'user)))
      (when (or (eq theme 'user)
                (custom-theme-enabled-p theme))
        (apply #'custom-theme-set-faces theme
               (mapcan #'doom--custom-theme-set-face specs))))))

(defun my/update-doom-features ()
  "Run after upgrading doom. Ediff's the current `init.el' with the example in doom-emacs-dir."
  (interactive)
  (require 'f)
  (ediff-files (f-join doom-user-dir "init.el")
               (f-join doom-emacs-dir "templates" "init.example.el")))

(advice-add #'doom/upgrade :after #'my/update-doom-features)

(defun my/file-contents (filename &optional noerror)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents filename)
          (buffer-string))
      (file-error
       (funcall (if noerror #'message #'user-error)
                "Unable to read file %s" filename)))))

;; add local packages to load path
(let ((default-directory (expand-file-name "local-packages" doom-user-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;;; Fixes

;; doom installs this advice which, if the formatter is not installed in the format-all executable table,
;; silently ignores it. this is strange, since the formatter utility function `set-formatter!' DOES NOT
;; install the executable. the advice doesn't do too else much anyway, so it's fine to remove it.
(advice-remove 'format-all-buffer--from-hook #'+format--all-buffer-from-hook-a)

;;; Command line arguments
(defun my/handle-extra-args ()
  "Handle extra args passed on the command line."
  (pcase argi
    ("config"
     (doom/goto-private-config-file))
    ("init"
     (doom/goto-private-init-file))
    ((or "package" "packages")
     (doom/goto-private-packages-file))))

(push #'my/handle-extra-args command-line-functions)

;;; Org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/notes/")

;; fix breakage of ',' as localleader
;; https://github.com/doomemacs/doomemacs/issues/4242#issuecomment-731436096
(add-hook! 'org-mode-hook #'+org-init-keybinds-h)

;;;; Look

(after! org
  (setq org-hide-emphasis-markers t
        org-fontify-quote-and-verse-blocks nil))

(after! imenu-list
  (custom-theme-set-faces! 'user
    '(imenu-list-entry-subalist-face-0 :bold nil :underline nil)
    '(imenu-list-entry-subalist-face-1 :bold nil :underline nil)
    '(imenu-list-entry-subalist-face-2 :bold nil :underline nil)
    '(imenu-list-entry-subalist-face-3 :bold nil :underline nil :inherit imenu-list-entry-face-3)))

(use-package! org-modern
  :defer
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-table-vertical 1)
  (setq org-modern-checkbox
        `((?X  . ,(all-the-icons-material "check_box"))
          (?\s . ,(all-the-icons-material "check_box_outline_blank"))
          (?-  . ,(all-the-icons-material "indeterminate_check_box"))))
  (setq org-modern-star (list "◆"))
  (add-hook 'doom-load-theme-hook #'org-modern--update-label-face))

(use-package! org-modern-indent
  :defer
  :hook (org-mode . org-modern-indent-mode))

(after! mixed-pitch
  (push 'org-drawer mixed-pitch-fixed-pitch-faces))

;;;; Structure templates (like <s)
(after! org
  (setq org-structure-template-alist
        '(("c"  . "comment")
          ("C"  . "center")
          ("e"  . "example")
          ("q"  . "quote")
          ("v"  . "verse")
          ("s"  . "src")
          ("el" . "src emacs-lisp")
          ("py" . "src python")
          ("hs" . "src haskell")
          ("jv" . "src java")
          ("E"  . "export")
          ("h"  . "export html")
          ("l"  . "export latex")))
  ;; required to actually use these
  (require 'org-tempo))

;;;; Capture templates
(after! org
  (setq org-capture-templates
        `(("w" "Work tasks" entry
           (file ,(expand-file-name "tasks.org" org-directory))
           "* TODO %^{title}\n:PROPERTIES:\nDATE: %t\n:END:\n%?"
           :prepend t
           :jump-to-capture t
           :empty-lines 1))))

;;;; Latex preview
(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! org
  ;; org-block backgrounds peek out from behind latex preview images.
  ;; This disables the org-block background whenever latex is previewed in any org buffer.
  (defun my/org-block-remove-bg (&rest _)
    (set-face-background 'org-block 'unspecified))
  (advice-add #'org--latex-preview-region :after #'my/org-block-remove-bg)

  (defun my/org-regen-latex-previews ()
    "Regenerate latex previews in this buffer."
    (interactive)
    (when (and (display-graphic-p)
               (fboundp #'+org--toggle-inline-images-in-subtree))
      (message "regenerating latex previews...")
      (let ((beg (point-min))
            (end (point-max)))
        (+org--toggle-inline-images-in-subtree beg end)
        (org-clear-latex-preview beg end)
        (org--latex-preview-region beg end))
      (message "regenerating latex previews... done.")))

  ;; FIXME this recurses infinitely for some reason
  ;; (add-hook 'doom-load-theme-hook #'my/org-regen-latex-previews)
  )

;;;; Org src block
;; change the org-src-edit buffer name to something less ugly
(after! org
  (defun my/org-src--construct-edit-buffer-name (org-buffer-name lang)
    (format "*org-src %s:%s*" org-buffer-name lang))

  (advice-add #'org-src--construct-edit-buffer-name
              :override
              #'my/org-src--construct-edit-buffer-name))

;;;; Inline Task
(after! org
  (map! :map org-mode-map
        :localleader
        :desc "insert inline task"
        "T"
        (cmd! (require 'org-inlinetask) ;; not usually loaded
              (call-interactively #'org-inlinetask-insert-task))))

;;;; Wrapping / indent mode
;; Fix wrapping in `org-indent-mode' issues caused by `adaptive-wrap-prefix-mode'
(after! org
  (add-hook 'org-mode-hook (cmd! (adaptive-wrap-prefix-mode -1))))

;;;; Export
(after! org
  (setq org-export-with-toc nil))

;;;; Agenda
(after! org
  (setq org-extend-today-until 6) ;; fine, I have a horrible sleep schedule
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t))

;;;; Habit
;; load with org
(add-to-list 'org-modules 'org-habit t)

;; settings that are not exactly org-habit specific, but which are customized for it:
(after! org
  ;; (setq org-treat-insert-todo-heading-as-state-change t) ;; I prefer to log TODO creation also
  (setq org-log-into-drawer t) ;; log into LOGBOOK drawer
  )
;;; Filesystem
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(after! dired-x
  (setq dired-omit-files
        (rx (or (seq line-start (repeat 1 2 (char ".")))
                (seq (char ".") "hie" line-end)))))

(after! recentf
  (add-to-list 'recentf-exclude (concat "^" doom-local-dir))
  (run-with-idle-timer 5 t #'recentf-cleanup))

;;; Evil
(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t
        evil-echo-state nil
        evil-kill-on-visual-paste nil)) ;; Don't put overwritten text in the kill ring


;;; UI

(setq display-line-numbers-type t)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; better than pixel-scroll-mode
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(use-package! highlight-indent-guides
  :config
  ;; stop doom from autoloading this
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-method 'bitmap
        ;; highlight-indent-guides-character ?\|
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line))

;;;; put the window on the right-most monitor if present
;; (when (display-graphic-p)
;;   (setq my/single-monitor-width 1920)
;;  (let ((rightmost-monitor-left-pos
;;         (* my/single-monitor-width (1- (/ (x-display-pixel-width) my/single-monitor-width)))))
;;    (setq initial-frame-alist
;;          `((top . 29) (left . ,rightmost-monitor-left-pos) (height . 58) (width . 267)))))


;;;; Font

(setq my/good-fonts
      (list
       (font-spec :family "Maple Mono" :size 12)
       (font-spec :family "DankMono" :size 15)
       (font-spec :family "Inconsolata" :size 16 :width 'semi-condensed)
       (font-spec :family "Rec Mono Semicasual" :size 14)
       (font-spec :family "Rec Mono Duotone" :size 14)
       (font-spec :family "Rec Mono Linear" :size 14)
       (font-spec :family "Iosevka" :size 12)
       (font-spec :family "CommitMono" :size 12)
       (font-spec :family "Agave" :size 14)
       (font-spec :family "Binchotan_Sharp" :size 14)))

(defun my/cycle-fonts ()
  (interactive)
  (unless (> (length my/good-fonts) 0)
    (user-error "`my/good-fonts' is empty!"))

  (let* ((curr-font doom-font)
         (curr-idx
          (--find-index
           (equal (font-get curr-font :family)
                  (font-get it :family))
           my/good-fonts)))
    (let ((done
           (catch 'done
             (dolist (font (--doto
                               (-rotate (- (1+ (or curr-idx -1))) my/good-fonts)))
               (setq doom-font font)
               (if (not (ignore-errors (doom/reload-font) t))
                   (message "font %s doesn't exist, skipping" (font-get font :family))
                 (message "set font: %s" (font-get doom-font :family))
                 (throw 'done t))
               nil))))
      (unless done
        (message "no font found, resetting to %s" (font-get curr-font :family))
        (setq doom-font curr-font)
        (doom/reload-font)))))

(setq
 doom-font (car my/good-fonts)
 ;; doom-variable-pitch-font (font-spec :family "Bitter Thin")
 doom-font-increment 1)

;; fix weird Info-manual faces
(custom-theme-set-faces! 'user
  '(fixed-pitch-serif :inherit default)
  '(info-menu-star    :inherit dired-mark)
  '(info-menu-header  :inherit dired-header))

;;;; Theme
;;;;; Timed changes

(use-package! timed-themes
  :commands timed-themes-minor-mode
  :custom
  (timed-themes/change-theme-if-manually-set t)
  (timed-themes/change-theme-default-on-ask-timeout t)
  (timed-themes/theme-timings
   `((modus-operandi-tinted . ,(am 6))
     (doom-tomorrow-night   . ,(pm 4)))))

;;;;; Sync with windows system theme from WSL

;; Probably better to change this to be a detection method in `auto-dark'
(use-package! windows-theme
  :commands windows-theme-minor-mode
  :custom
  (windows-theme/themes
   '((light . modus-operandi-tinted)
     (dark  . kaolin-bubblegum)
     (error . adwaita))))

;;;;; Sync with system theme

(use-package! auto-dark
  :commands auto-dark-mode
  :custom
  (auto-dark-light-theme 'modus-operandi-tinted)
  (auto-dark-dark-theme 'doom-moonlight))

(auto-dark-mode)


;;;;; Theme modifications

(after! doom-themes
  (setq doom-themes-enable-italic nil)
  (setq doom-tokyo-night-brighter-comments t)
  (custom-theme-set-faces! 'doom-ayu-mirage
    '(line-number :foreground "gray28"))
  (custom-theme-set-faces! 'doom-tokyo-night
    '(shadow :foreground "gray22")
    '(line-number :foreground unspecified :background unspecified :inherit shadow))
  (custom-theme-set-faces! 'doom-tomorrow-night
    '(line-number :inherit shadow)
    '(line-number-current-line :inherit default)))

(after! modus-themes
  (custom-theme-set-faces! 'modus-operandi-tinted
    '(line-number :foreground "burlywood3" :background unspecified)
    '(fringe :background unspecified)
    '(git-gutter-fr:added    :foreground "#006800" :background unspecified)
    '(git-gutter-fr:modified :foreground "tan3"    :background unspecified)
    '(git-gutter-fr:deleted  :foreground "#d84a4f" :background unspecified))
  (custom-theme-set-faces! 'modus-vivendi-tinted
    '(line-number :foreground "#61e157578e0d" :background unspecified)
    '(fringe :background unspecified)
    '(git-gutter-fr:added    :foreground "#237f4f" :background unspecified)
    '(git-gutter-fr:modified :foreground "#8a7a00" :background unspecified)
    '(git-gutter-fr:deleted  :foreground "#b81a26" :background unspecified))
  (custom-theme-set-faces! 'modus-vivendi
    '(line-number :foreground "#1e1e1e" :background unspecified)
    '(fringe :background unspecified)
    '(git-gutter-fr:added    :foreground "#237f4f" :background unspecified)
    '(git-gutter-fr:modified :foreground "#8a7a00" :background unspecified)
    '(git-gutter-fr:deleted  :foreground "#b81a26" :background unspecified)))

(after! light-soap-theme
  (custom-theme-set-faces! 'light-soap
    '(fringe :background unspecified)))

(after! autumn-light-theme
  (custom-theme-set-faces! 'autumn-light
    '(mode-line :background "tan")
    '(org-verbatim :inherit org-priority)
    '(doom-modeline-buffer-modified :foreground "firebrick4" :bold t)))

(after! kaolin-themes
  (custom-theme-set-faces! 'kaolin-blossom
    '(header-line :background "#271f1f"))
  (custom-theme-set-faces! 'kaolin-valley-light
    '(header-line :background "#EEE6D3"))
  (custom-theme-set-faces! 'kaolin-bubblegum
    '(header-line :background "#111")))

;;;;; Prism

(after! prism
  ;; when prism loads up, it's supposed to have prism-faces set to nil.
  ;; `prism-mode' looks at this when starting and decides whether to call
  ;; `prism-set-colors' to set the colors according to the current theme.
  ;;
  ;; for some reason, this is always set to a full list, so calling `prism-mode'
  ;; never actually sets the correct colors without manually calling
  ;; `prism-set-colors' (or `prism-randomize-colors').
  (setq prism-faces nil) ;; this line is *very* important. do NOT remove.

  (setq prism-num-faces 16
        prism-color-distance 20000
        prism-desaturations '(0)
        prism-parens nil
        prism-lightens '(0))

  ;; NOTE not required, prism already advises `load-theme'
  ;; --- (add-hook 'doom-load-theme-hook #'prism-set-colors)
  )

;;;;; Font based on light or dark

(defun my/set-font-weight-by-light-or-dark (&rest _)
  (let ((weight
         (pcase (frame-parameter nil 'background-mode)
           ('light 'semibold)
           ('dark  'light)
           (_      (progn (message "Unknown background-mode... defaulting to 'dark") 'light))))
        (curr-weight (or (font-get doom-font :weight) 'normal)))
    (unless (equal weight curr-weight)
      (font-put doom-font :weight weight)
      (doom/reload-font))))

;; (add-hook 'doom-load-theme-hook #'my/set-font-weight-by-light-or-dark -50)
;; (add-hook 'doom-init-ui-hook #'my/set-font-weight-by-light-or-dark)

;;;; Modeline

(after! doom-modeline
  (setq doom-modeline-hud nil
        doom-modeline-major-mode-icon nil
        doom-modeline-icon nil
        doom-modeline-modal nil
        ;; reduce the size of icons in the modeline so that it doesn't get cut off at the end
        all-the-icons-scale-factor 1.1)

  (remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)

  (custom-theme-set-faces! 'user
    `(doom-modeline-debug             :weight unspecified :inherit font-lock-doc-face)
    `(doom-modeline-info              :weight unspecified :inherit info)
    `(doom-modeline-warning           :weight unspecified :inherit warning)
    `(doom-modeline-urgent            :weight unspecified :inherit error)
    `(doom-modeline-lsp-error         :weight unspecified :inherit error)
    `(doom-modeline-lsp-warning       :weight unspecified :inherit warning)
    `(doom-modeline-lsp-success       :weight unspecified :inherit success)
    `(doom-modeline-buffer-major-mode :weight ,(face-attribute 'default :weight) :inherit doom-modeline-buffer-path)
    `(doom-modeline-repl-success      :inherit success)
    `(doom-modeline-repl-warning      :inherit error))

  ;; Change the lsp icon to be something nicer
  (defun my/doom-modeline-lsp-icon (text face)
    (doom-modeline-icon 'material "blur_circular" "{lsp}" text :face face))
  (advice-add 'doom-modeline-lsp-icon :override #'my/doom-modeline-lsp-icon)

  ;; Change checker icon
  (defun doom-modeline-update-flycheck-icon (&optional status)
    "Update flycheck icon via STATUS."
    (setq doom-modeline--flycheck-icon
          (when-let
              ((icon
                (pcase status
                  ('finished  (if flycheck-current-errors
                                  (let-alist (doom-modeline--flycheck-count-errors)
                                    (propertize
                                     "!"
                                     'face
                                     (cond ((> .error 0) 'doom-modeline-urgent)
                                           ((> .warning 0) 'doom-modeline-warning)
                                           (t 'doom-modeline-info))))
                                (propertize "✓"  'face 'doom-modeline-info)))
                  ('running     (propertize "*"   'face 'doom-modeline-debug))
                  ('no-checker  (propertize "-"   'face 'doom-modeline-debug))
                  ('errored     (propertize "!!!" 'face 'doom-modeline-urgent))
                  ('interrupted (propertize "="   'face 'doom-modeline-debug))
                  ('suspicious  (propertize "!!!" 'face 'doom-modeline-urgent))
                  (_ nil))))
            (propertize icon
                        'help-echo (concat "Flycheck\n"
                                           (pcase status
                                             ('finished "mouse-1: Display minor mode menu
mouse-2: Show help for minor mode")
                                             ('running "Running...")
                                             ('no-checker "No Checker")
                                             ('errored "Error")
                                             ('interrupted "Interrupted")
                                             ('suspicious "Suspicious")))
                        'mouse-face 'mode-line-highlight
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line down-mouse-1]
                                                 flycheck-mode-menu-map)
                                     (define-key map [mode-line mouse-2]
                                                 (cmd! (describe-function 'flycheck-mode)))
                                     map)))))

  ;; simpler vcs icon - use text fallback instead of the actual icon
  (defun doom-modeline-vcs-icon (icon &optional unicode text face voffset)
    (propertize text 'face face))

  (doom-modeline-def-segment spacing
    (doom-modeline-wspc))

  (doom-modeline-def-segment line-with-max
    (let* ((lc '(column-number-mode
                 (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                 "%l"))
           (curr-line (format-mode-line lc))
           (max-line (save-excursion
                       (goto-char (point-max))
                       (format-mode-line "%l"))))
      (concat
       (doom-modeline-wspc)
       (s-concat curr-line "/" max-line))))

  (doom-modeline-def-segment theme
    (when (doom-modeline--active)
      (let* ((theme-name (symbol-name
                          (or (car-safe custom-enabled-themes)
                              'default)))
             (timed-themes-enabled (bound-and-true-p timed-themes-minor-mode))
             (windows-theme-enabled (bound-and-true-p windows-theme-minor-mode)))
        (concat
         (propertize
          (--> theme-name
               (if timed-themes-enabled (concat "(" it ")") it)
               (if windows-theme-enabled (concat "[" it "]") it))
          'face 'doom-modeline-buffer-minor-mode
          'mouse-face 'mode-line-highlight
          'help-echo "Current theme")
         (doom-modeline-spc)))))

  (after! lsp-mode
    (defun my/lsp-modeline--code-actions-icon (face)
      "Build the icon for modeline code actions using FACE."
      (doom-modeline-icon 'octicon "light-bulb"
                          lsp-modeline-code-action-fallback-icon "{!}"
                          :face face))
    (advice-add 'lsp-modeline--code-actions-icon
                :override #'my/lsp-modeline--code-actions-icon))


  ;; NOTE misc-info shows whatever is in `global-mode-string'
  ;; in `lsp-mode', this is code actions and diagnostics data. (if their respective modes are enabled),
  ;; so it makes sense to keep it beside `lsp'
  (doom-modeline-def-modeline 'main
    '(workspace-name window-number matches buffer-info remote-host line-with-max word-count parrot selection-info)
    '(objed-state persp-name battery grip irc mu4e gnus github debug repl misc-info lsp minor-modes input-method indent-info buffer-encoding theme major-mode process vcs)))

;;;; Dashboard

(when (display-graphic-p)
  (require 'f)
  (setq +doom-dashboard-banner-dir (f-join doom-user-dir "banner-pictures")
        +doom-dashboard-banner-file "planet.png"
        +doom-dashboard-banner-padding '(6 . 6)))

;; minimal dashboard
(setq +doom-dashboard-functions
      (list #'doom-dashboard-widget-banner
            #'doom-dashboard-widget-loaded))

;; don't echo the init time since it's in the dashboard anyway
(remove-hook 'doom-after-init-hook #'doom-display-benchmark-h)

;; remove mode-line and cursor from doom-dashboard
(setq-hook! '+doom-dashboard-mode-hook
  mode-line-format nil
  evil-normal-state-cursor (list nil))

(add-hook #'+doom-dashboard-mode-hook #'clear-minibuffer-message)

(after! doom-modeline
  (setq doom-modeline-mode-alist
        (assq-delete-all '+doom-dashboard-mode doom-modeline-mode-alist)))

;;;; Treemacs

(use-package! treemacs
  :init
  (setq +treemacs-git-mode 'deferred)
  :config
  (setq treemacs-collapse-dirs 3)
  (add-hook 'treemacs-mode #'treemacs-follow-mode))

;;; General programming utilities
;;;; LSP

(after! lsp-mode
  ;;;;; from emacs-lsp-booster docs
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  ;;;;; /end from emacs-lsp-booster docs

  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-enable t
        lsp-eldoc-enable-hover nil
        lsp-signature-render-documentation nil
        lsp-enable-folding t
        lsp-warn-no-matched-clients nil
        lsp-auto-execute-action nil)

  (defun my/lsp--read-rename-no-placeholder (at-point)
    "Modify `lsp--read-rename' (the function that reads the new name for symbol when `lsp-rename' is called to not use a placeholder name.
The placeholder name is usually the old name itself, which irks me as I have to delete it before I can do the rename.
This is almost a complete copy of the original method, with a few very minor deletions to remove the placeholder-calculation."
    (unless at-point
      (user-error "`lsp-rename' is invalid here"))
    (-let* ((((start . end) . _placeholder) at-point)
            (rename-me (buffer-substring-no-properties start end))
            overlay)
      ;; We need unwind protect, as the user might cancel here, causing the
      ;; overlay to linger.
      (unwind-protect
          (progn
            (setq overlay (make-overlay start end))
            (overlay-put overlay 'face 'lsp-face-rename)

            (read-string (format "Rename %s to: " rename-me) nil
                         'lsp-rename-history))
        (and overlay (delete-overlay overlay)))))

  (advice-add #'lsp--read-rename :override #'my/lsp--read-rename-no-placeholder)

  (advice-add #'lsp-rename
              :after
              (defun my/save-project-buffers (&rest _)
                (require 'projectile)
                (when (projectile-project-root)
                  (projectile-save-project-buffers))))

  (map! (:map prog-mode-map
              (:leader
               (:prefix ("l" . "lsp")
                :desc "start lsp server" :n "l" #'lsp!
                :desc "restart lsp server" :n "r" #'lsp-workspace-restart
                :desc "stop lsp server" :n "k" #'lsp-workspace-shutdown))))

  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-arrow ""))

;;;; Project management

(after! projectile
  (add-hook 'projectile-after-switch-project-hook
            (defun my/set-default-directory-to-project-root ()
              (setq default-directory (projectile-project-root)))))

;;;; Code context

(use-package! window-stool
  :commands window-stool-mode
  :hook (prog-mode . window-stool-mode)

  :init
  (map! :map prog-mode-map
        :leader
        :desc "code context" :n "t x" (cmd! (window-stool-mode 'toggle)))

  :config
  (set-face-attribute 'window-stool-face nil :inherit 'region)

  (setq window-stool-n-from-top 10
        window-stool-n-from-bottom 10)

  (add-hook 'window-stool-mode-hook
            (defun my/fix-conflict-b/w-window-stool-&-lsp-headerline ()
              (if window-stool-mode
                  (when (bound-and-true-p lsp-headerline-breadcrumb-mode)
                    (lsp-headerline-breadcrumb-mode -1))
                (when (and (bound-and-true-p lsp-mode)
                           (bound-and-true-p lsp-headerline-breadcrumb-enable))
                  (lsp-headerline-breadcrumb-mode))))))

;; TODO modify `window-stool-fn' to return outline/outshine headers as well.


;;;; Dir locals

;; Reload dir-locals.
;; TODO add a function that does this to all buffers of current project.
;;
;; https://emacs.stackexchange.com/a/13096

(defun my/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

;;;; Version control

(after! git-commit
  ;; the visual indicator is enough for me - don't want to explicitly confirm
  (delq! 'overlong-summary-line git-commit-style-convention-checks))

(map! :when (modulep! :ui vc-gutter)
      :after git-gutter

      (:leader
       :desc "Show git diff at point"
       :n "g d" #'git-gutter:popup-diff)

      :desc "Jump to previous hunk"
      :n "[ g" #'git-gutter:previous-hunk

      :desc "Jump to next hunk"
      :n "] g" #'git-gutter:next-hunk)

(after! vc
  (run-with-idle-timer 5 t #'vc-refresh-state))

;;;; Glasses mode (camelcase -> view as snake case)

(use-package! glasses
  :init
  (setq glasses-separator "-"
        glasses-original-separator "_"
        glasses-uncapitalize-p t
        glasses-separate-parentheses-p nil
        glasses-separate-capital-groups nil
        glasses-face nil))

;;;; Directory tree

(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

;;; Language-specific configs

;; NOTE: localleader-p (~, p~) -> package manager hydra

(map! :map prog-mode-map
      :localleader
      :desc "package manager"
      "p" (cmd! (message "no package manager interface defined for %s" major-mode)))

;;;; Emacs lisp

(after! elisp-mode
  (require 'delight)
  (delight 'emacs-lisp-mode "elisp" :major)
  (map! :map emacs-lisp-mode-map
        :localleader
        :desc "edit doom packages"
        "p" #'doom/goto-private-packages-file)

  ;; the checker for emacs-lisp sucks
  (add-hook 'emacs-lisp-mode-hook (cmd! (flycheck-mode -1)))

  (add-hook 'emacs-lisp-mode-hook #'prism-mode))

;;;; OCaml

(defun my/find-dune-file ()
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
        :desc "visit corresp. dune file" "d" #'my/find-dune-file)

  (setq-hook! 'tuareg-mode-hook
    +default-want-RET-continue-comments nil
    +evil-want-o/O-to-continue-comments nil
    evil-surround-pairs-alist
    (append
     '((?\b . ("begin"  . "end"))
       (?\m . ("sig"    . "end"))
       (?\s . ("struct" . "end")))
     evil-surround-pairs-alist))

  (add-to-list
   'hs-special-modes-alist
   `(tuareg-mode
     ,(regexp-opt '("sig" "struct" "begin"))
     "end"
     nil nil))

  (add-hook 'tuareg-mode-hook #'hs-minor-mode))

(after! dune
  ;; switch back-and-forth b/w ocaml and dune file
  (map! :map dune-mode-map
        :localleader
        :desc "visit previous file" "d" #'evil-switch-to-windows-last-buffer))

;;;; Purescript
(after! purescript-mode
  ;; (set-formatter! 'purty "purty -" :modes '(purescript-mode))
  (set-formatter! 'purs-tidy "purs-tidy format" :modes '(purescript-mode))
  (setq-hook! 'purescript-mode-hook +format-with-lsp nil)

  ;; the pursls lsp shows a *lot* of lens things
  (setq-hook! 'purescript-mode-hook lsp-lens-enable nil))

;;;; Dhall
(use-package! dhall-mode
  :mode "\\.dhall\\'")

;;;; Elm
(use-package! elm-mode
  :hook (elm-mode . elm-format-on-save-mode)
  :config
  (defun my/elm-color-hex-to-rgb (color-hex)
    (let ((color-hex (if (s-starts-with? "#" color-hex)
                         (substring color-hex 1)
                       color-hex)))
      (when (/= 6 (length color-hex))
        (user-error "invalid color hex string length"))
      (require 'kurecolor)
      (require 'dash)
      (require 's)
      (s-join " " (cons "rgb255"
                        (--map (number-to-string (round (* it 255)))
                               (kurecolor-hex-to-rgb color-hex))))))

  (defun my/elm-replace-color-hex-to-rgb255 ()
    (interactive)
    (require 'kurecolor)
    (kurecolor-replace-current #'my/elm-color-hex-to-rgb)))

;; TODO add font-lock keywords to highlight `rgb255 r g b' expressions with appr color

;;;; Haskell
(after! haskell-mode
  (defun my/haskell-hpack-find-pkg-desc (dir &optional allow-multiple)
    "Return hpack (package.yaml) files instead of the generated cabal file."
    (let* ((cabal-files
            (cl-remove-if 'file-directory-p
                          (cl-remove-if-not 'file-exists-p
                                            (directory-files dir t "package\\.yaml\\'")))))
      (cond
       ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
       (allow-multiple cabal-files) ;; pass-thru multiple candidates
       (t nil))))

  (advice-add #'haskell-cabal-find-pkg-desc :before-until #'my/haskell-hpack-find-pkg-desc)

  ;; Integrate `haskell-hide-all' etc. with the general fold bindings
  (map! :map haskell-mode-map
        :n "z o" #'haskell-hide-toggle
        :n "z c" #'haskell-hide-toggle
        :n "z a" #'haskell-hide-toggle
        :n "z m" #'haskell-hide-toggle-all
        :n "z r" #'haskell-hide-toggle-all)

  (setq haskell-process-suggest-remove-import-lines nil
        haskell-process-suggest-hoogle-imports t
        haskell-interactive-popup-errors nil))

(use-package! flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package! hiedb
  ;; :hook (haskell-mode . hiedb-mode)
  :config
  (advice-add #'hiedb-module-from-path
              :before
              (defun my/hiedb-set-project-paths ()
                (when-let ((root (and
                                  (fboundp 'projectile-project-root)
                                  (projectile-project-root))))
                  (setq hiedb-project-root root
                        hiedb-dbfile (expand-file-name ".hiedb" root)))))

  ;; FIXME hiedb functions throws errors
  (set-lookup-handlers! 'hiedb-mode
    :definition #'hiedb-interactive-defs
    :references #'hiedb-interactive-refs
    :documentation #'hiedb-interactive-info
    :type-definition #'hiedb-interactive-types))

;;;; Latex

(after! latex
  (defun my/latex-preview-pane-start-or-update ()
    (interactive)
    (if (bound-and-true-p latex-preview-pane-mode)
        (latex-preview-pane-update)
      (latex-preview-pane-mode)))
  (map! :map LaTeX-mode-map
        :localleader
        :desc "start or update latex preview pane" "p" #'my/latex-preview-pane-start-or-update))

;;;; Clojure

(after! clojure-mode
  ;; rama macros
  (put-clojure-indent '<<sources 1)
  (put-clojure-indent '<<if 1)
  (put-clojure-indent 'ifexpr 1)

  (add-hook 'clojure-mode-hook #'prism-mode)

  (defun my/clojure-visit-project-file ()
    (interactive)
    (let* ((proj-type (cider-project-type))
           (proj-dir (clojure-project-dir))
           (proj-rel-file (cl-case proj-type
                            (lein "project.clj")
                            (t (error "todo: unhandled project type: %s" proj-type))))
           (proj-file (f-join proj-dir proj-rel-file)))
      (if (f-file? proj-file)
          (find-file proj-file)
        (error "couldn't file %s" proj-rel-file))))

  (map! :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
        :localleader
        :desc "visit project file" "p" #'my/clojure-visit-project-file)

  ;; Using any sort of indentation/formatting in clojure which is
  ;; based on emacs' `read' is going to be flawed, since the elisp reader
  ;; differs from clojure's.
  ;; For example, try evaluating the following sexp:
  ;; > (read "(.. System getProperties)")
  ;; Note that the `..' is escaped into `\..'!
  ;; So every time you try to format this expression (or any region containing it),
  ;; it will be "formatted" into:
  ;; > (\.. System getProperties)
  ;;
  ;; I noticed this when using `lispyville' (specifically, `lispyville-prettify'),
  ;; which defers to `lispy', which eventually calls emacs' `read'.
  ;;
  ;; To solve this, in clojure modes (i.e. clj, cljs, cljc),
  ;; *only* use `cider' formatting. Cider internally defers to `cljfmt'.
  ;; `lispyville-prettify' is bound to '=', so we shadow this.

  (when (and (modulep! :editor lispy) (modulep! :editor evil)) ;; i.e. lispyville
    (evil-define-operator my/cider-prettify-operator (_beg end)
      "Prettify lists using `cider' formatting.
Inspired by `lispyville-prettify'."
      :move-point nil
      (interactive "<r>")
      (let ((orig-pos (point)))
        (lispy--out-backward 1)
        (let ((beg (point)))
          (while (and (ignore-errors (lispyville--forward-list))
                      (> end (save-excursion (backward-list))))
            (let ((end (point)))
              (cider-format-region beg end))))
        (goto-char orig-pos)))

    (map! :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
          :n "=" #'my/cider-prettify-operator)))


;;;; Rust

(after! rustic
  (when (modulep! :lang rust +lsp)
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"))

  (defun my/cargo-packages ()
    (save-window-excursion
      (lsp-rust-analyzer-open-cargo-toml)
      (goto-char (point-min))
      (when (re-search-forward "^\\[dependencies\\]$" nil 'noerror)
        (let ((matches '()))
          (while (re-search-forward "^[a-zA-Z]+" nil 'noerror)
            (push (match-string-no-properties 0) matches))
          matches))))

  (defun my/cargo-rm ()
    (interactive)
    (let ((pkg (completing-read "Crate: " (my/cargo-packages) nil t)))
      (rustic-run-cargo-command (format "%s rm %s" (rustic-cargo-bin) pkg))))

  (map! :map rustic-mode-map
        :localleader
        (:prefix ("p" . "packages")
         :desc "cargo-add" "a" #'rustic-cargo-add
         :desc "cargo-rm"  "d" #'my/cargo-rm))

  (setq rustic-cargo-use-last-stored-arguments t))

;;;; AutoHotKey

(after! ahk-mode
  (when (modulep! :tools lookup)
    (defun my/ahk-lookup-web-v2 (symbol)
      (interactive (list (ahk-command-at-point)))
      (browse-url (format "https://www.autohotkey.com/docs/v2/lib/%s.htm" symbol)))

    (set-lookup-handlers! 'ahk-mode
      :async t
      :documentation #'my/ahk-lookup-web-v2)))

;;;; Typescript

(use-package! typescript-mode
  :hook (typescript-mode . glasses-mode))

;;;; Web

(use-package! web-mode
  :config
  (setq web-mode-auto-close-style 2))

;;;; Tailwind

(use-package! lsp-tailwindcss
  :init
  ;; *has* to be set before the package loads
  (setq lsp-tailwindcss-add-on-mode t))

;;;; Minor-modes
;;;;; outshine

(use-package! outshine
  :hook (prog-mode . outshine-mode)

  :config
  (map! :map outshine-mode-map
        :desc "narrow to subtree" :n "zn" #'outshine-narrow-to-subtree)

  (add-hook 'outshine-mode-hook
            (defun my/outshine-compose-start ()
              "Make outshine (outline) headings prettier."
              (when outshine-mode
                (add-to-list 'font-lock-extra-managed-props 'display)
                (let* ((outline-rgxp (substring outline-regexp 0 -8))
                       (font-lock-new-keywords
                        `((,(concat outline-rgxp "\\{1\\} ") 0 '(face outshine-level-1 display "[1] ") t)
                          (,(concat outline-rgxp "\\{2\\} ") 0 '(face outshine-level-2 display "[2] ") t)
                          (,(concat outline-rgxp "\\{3\\} ") 0 '(face outshine-level-3 display "[3] ") t)
                          (,(concat outline-rgxp "\\{4\\} ") 0 '(face outshine-level-4 display "[4] ") t)
                          (,(concat outline-rgxp "\\{5\\} ") 0 '(face outshine-level-5 display "[5] ") t)
                          (,(concat outline-rgxp "\\{6\\} ") 0 '(face outshine-level-6 display "[6] ") t)
                          (,(concat outline-rgxp "\\{7\\} ") 0 '(face outshine-level-7 display "[7] ") t)
                          (,(concat outline-rgxp "\\{8\\} ") 0 '(face outshine-level-8 display "[8] ") t))))
                  ;; need to add these new keywords to outshine's so that they are disabled together
                  (add-to-list 'outshine-font-lock-keywords font-lock-new-keywords)
                  (font-lock-add-keywords nil font-lock-new-keywords)
                  (outshine-font-lock-flush)))))

  (setq outshine-fontify-whole-heading-line t)

  (require 'magit)
  (custom-theme-set-faces! 'user
    '(outshine-level-1 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-2 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-3 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-4 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-5 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-6 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-7 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-8 :inherit magit-diff-hunk-heading-highlight)))

;;;;; lispyville

(use-package! lispyville
  :hook (lisp-mode . lispyville-mode)
  :config
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          prettify
          atom-movement
          slurp/barf-lispy
          commentary
          additional-movement
          additional-wrap))
  (lispyville-set-key-theme)
  (map! :map lispyville-mode-map :n "[" nil)
  (map! :map lispyville-mode-map :v "g c" #'lispyville-comment-or-uncomment))

;;;;; zen / writeroom

(when (modulep! :ui zen)
  ;; don't zoom
  (setq +zen-text-scale 0))

(after! writeroom-mode
  (setq writeroom-width 0.6)
  (add-hook 'writeroom-mode-enable-hook (cmd! (setq display-line-numbers nil))))

;;; Assorted

(setq confirm-kill-emacs nil)

;; stop the constant "Cleaning up the recentf list...done" messages
(advice-add 'recentf-cleanup :around #'silently)

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; don't want to put my cursor at the absolute edge.
;; get some context.
(setq scroll-margin 5)

(setq byte-compile-warnings nil)

;; keep this as bash since everything else assumes it,
;; even if I'm using a non-standard shell (like nushell)
(setq shell-file-name "/bin/bash")

;;; Keybindings
;;;; Main

(setq doom-leader-key "SPC"
      doom-leader-alt-key "M-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; learned habit from IntelliJ and VS Code
(map! :nv "C-/" #'evilnc-comment-or-uncomment-lines)

;; similar to VsVim
(map! :n "g r" #'+lookup/references)

(map! :leader :desc "M-x" "SPC" #'execute-extended-command)

(map! :leader "s I" #'imenu-list)


;;;; Line numbers

(map! :leader :desc "" "t l"
      (cmd! (setq display-line-numbers (not display-line-numbers))))

;;;; Window

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
   :desc "ace-swap"        :n "S" #'ace-swap-window
   ;; remove previous one-shot bindings.
   ;; the hydra takes care of all these cases much better.
   "+" nil
   "-" nil
   "<" nil
   ">" nil)))

(map!
 :desc "scroll other window down"       :n "M-j"   (cmd! (scroll-other-window 2))
 :desc "scroll other window down a lot" :n "M-S-j" (cmd! (scroll-other-window))
 :desc "scroll other window up"         :n "M-k"   (cmd! (scroll-other-window-down 2))
 :desc "scroll other window up a lot"   :n "M-S-k" (cmd! (scroll-other-window-down)))

(map! :desc "Frame maximize" :leader "t F" #'toggle-frame-maximized)

;;;; Terminal(s)

;; keep keybinds consistent even in emacs' different terminals.
;; for this, unmap C-k and C-j from moving between prompts.
;; these are still available through ~g k~ and ~g j~
(map! :map term-mode-map
      :n "C-k" nil
      :n "C-j" nil)

(after! vterm
  (setq vterm-clear-scrollback-when-clearing t))

(map! :map vterm-mode-map
      :n "g k" #'vterm-previous-prompt
      :n "g j" #'vterm-next-prompt
      :i "C-l" #'vterm-clear)

;;;; Errors

(map! :after flycheck :leader :n "e l" #'flycheck-list-errors)

;;;; Vim

;; take back ~s~
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; vim-vinegar
(map! :m "-" #'dired-jump)

(map!
 :desc "format region" :v "gq" #'+format/region ; This is the keybinding I always reach for to format a region
 :desc "format buffer" :n "gQ" #'+format/buffer)

;; (map! :leader :desc "random-themes-hydra" :n "h T"
;;       (cmd!
;;        (load! "local-packages/random-themes/random-themes.el" doom-user-dir)
;;        (random-themes--hydra/body)))

(use-package! random-themes
  :commands random-themes--hydra/body
  :init
  (map! :leader :desc "random-themes-hydra" :n "h T" #'random-themes--hydra/body))

(map! :when (modulep! :ui hl-todo) :leader :desc "search for todos" :n "s t" #'hl-todo-occur)


;;; Notes
;; TODO Check out what `hyperbole' can do for you
;; TODO Change `imenu-list' to differentiate between headlines of diff depths
;; TODO Check out dynamic views in `org-ql' to make a better agenda
;; TODO Fix org-edit-src:
;;      1. Remove the top-bar saying C-c, C-k - can be removed by setting `org-edit-src-persistent-message'
;;      2. Bind :w and :q to write/abort
;; TODO make the various native compilation buffers not pop up. also stop the messages in the minibuffer
;; TODO don't print 'No buttons!' when pressing j/k in doom-dashboard
;; TODO increase fringe-width when in fullscreen mode (~SPC t F~) to increase readability of git-gutter (and other info in fringe)
;; TODO modify `prism-randomize-colors' to:
;; 1. be consistent (seed with a set value)
;; 2. not produce colors with low contrast
;; TODO add fallback unicode font
