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

(defun kk/load-doom-theme (&optional theme)
  "Load the currently set `doom-theme'. If THEME is provided, set it to `doom-theme' first."
  (setq doom-theme (or theme doom-theme))
  (load-theme doom-theme t nil))

;;; Org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;;;; Look

(after! org
  (setq org-hide-emphasis-markers t))

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
  (add-hook 'doom-load-theme-hook #'org-modern--update-label-face))

(use-package! org-modern-indent
  :defer
  :hook (org-mode . org-modern-indent-mode))

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
  (defun kk/org-block-remove-bg (&rest _)
    (set-face-background 'org-block 'unspecified))
  (advice-add #'org--latex-preview-region :after #'kk/org-block-remove-bg)

  (defun kk/org-regen-latex-previews ()
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

  (add-hook 'doom-load-theme-hook #'kk/org-regen-latex-previews))

;;; LSP
;; TODO this does not isolate this to prog-mode-map
(map! :map prog-mode-map
      (:leader
       (:prefix ("l" . "lsp")
        :desc "start lsp server"   :n "l" #'lsp!
        :desc "restart lsp server" :n "r" #'lsp-workspace-restart
        :desc "stop lsp server"    :n "k" #'lsp-workspace-shutdown)))

(after! lsp-mode
  (advice-add #'lsp-rename
              :after
              (defun kk/save-project-buffers (&rest _)
                (require 'projectile)
                (when (projectile-project-root)
                  (projectile-save-project-buffers)))))

;;; Filesystem
(add-hook! dired-mode #'dired-hide-details-mode)

(after! dired-x
  (setq dired-omit-files
        (rx (or (seq line-start (repeat 1 2 (char ".")))
                (seq (char ".") "hie" line-end)))))

(after! recentf
  (add-to-list 'recentf-exclude (concat "^" doom-local-dir))
  (run-with-idle-timer 5 t #'recentf-cleanup))

;;; Evil
(use-package! evil
  :config
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t
        evil-echo-state nil))

;;; LSP

(setq lsp-ui-doc-enable nil
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-sideline-enable t
      lsp-eldoc-enable-hover nil
      lsp-signature-render-documentation nil
      lsp-enable-folding t
      lsp-warn-no-matched-clients nil)

(after! lsp-mode
  (defun kk/lsp--read-rename-no-placeholder (at-point)
    "Modify `lsp--read-rename' (the function that reads the new name for symbol when `lsp-rename' is called to not use a placeholder name.
The placeholder name is usually the old name itself, which irks me as I have to delete it before I can do the rename.
This is almost a complete copy of the original method, with a few very minor deletions to remove the placeholder-calculation."
    (unless at-point
      (user-error "`lsp-rename' is invalid here"))
    (-let* ((((start . end) . _placeholder) at-point)
            (rename-me (buffer-substring start end))
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

  (advice-add #'lsp--read-rename :override #'kk/lsp--read-rename-no-placeholder))

;;; UI

(setq display-line-numbers-type t)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(use-package! highlight-indent-guides
  :config
  ;; stop doom from autoloading this
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-method 'bitmap
        ;; highlight-indent-guides-character ?\|
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line))

;; put the window on the right-most monitor if present
(when (display-graphic-p)
  (setq kk/single-monitor-width 1920)
  (let ((rightmost-monitor-left-pos
         (* kk/single-monitor-width (1- (/ (x-display-pixel-width) kk/single-monitor-width)))))
    (setq initial-frame-alist
          `((top . 29) (left . ,rightmost-monitor-left-pos) (height . 58) (width . 267)))))


;;;; Font

(setq doom-font (font-spec :family "Iosevka" :size 14 :weight 'light)
      doom-variable-pitch-font doom-font ;; (font-spec :family "Roboto Slab")
      doom-font-increment 1)

;; fix weird Info-manual faces
(custom-theme-set-faces! 'user
  '(fixed-pitch-serif :inherit 'default)
  '(info-menu-star    :inherit 'dired-mark)
  '(info-menu-header  :inherit 'dired-header))

;;;; Theme
;;;;; Timed changes

;; (load! "local-packages/timed-themes/timed-themes.el" doom-user-dir)

;; (setq timed-themes/theme-timings
;;       `((autumn-light        . (,(am 6) . ,(pm 1)))
;;         (doom-tomorrow-night . (,(pm 1) . ,(am 6))))

;;       timed-themes/default-theme 'wombat ;; different enough to be noticeable, but won't accidentally blind me
;;       timed-themes/change-theme-if-manually-set nil
;;       timed-themes/change-theme-default-on-ask-timeout nil)

;; (setq doom-theme (timed-themes/theme-for-time))
;; (timed-themes-minor-mode)

(setq doom-theme 'doom-tomorrow-night)

;;;;; Theme modifications

(after! doom-themes
  (setq doom-themes-enable-italic nil)
  (setq doom-tokyo-night-brighter-comments t)
  (custom-theme-set-faces! 'doom-ayu-mirage
    '(line-number :foreground "gray28")))

(after! modus-themes
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-subtle-line-numbers t)


  (custom-theme-set-faces! 'modus-operandi
   '(modus-themes-fringe-green  :foreground "#62c86a" :background unspecified)
   '(modus-themes-fringe-red    :foreground "#f08290" :background unspecified)
   '(modus-themes-fringe-yellow :foreground "#dbba3f" :background unspecified)))

(after! light-soap-theme
  (custom-theme-set-faces! 'light-soap
    '(fringe :background nil)))

(after! autumn-light-theme
  (custom-theme-set-faces! 'autumn-light
    '(mode-line :background "tan")
    '(org-verbatim :inherit org-priority)
    '(doom-modeline-buffer-modified :foreground "firebrick4" :bold t)))

;;;;; Prism

(after! prism
  (setq prism-colors
        '(font-lock-type-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-builtin-face
          font-lock-string-face
          font-lock-preprocessor-face)
        prism-num-faces 16
        prism-color-distance 20000
        prism-desaturations '(0)
        prism-parens t
        prism-lightens '(0)))

;;;;; Font based on light or dark

(defun kk/set-font-weight-by-light-or-dark ()
  (let ((weight
         (pcase (frame-parameter nil 'background-mode)
           ('light 'normal)
           ('dark  'light)
           (_      (progn (message "Unknown background-mode... defaulting to 'dark") 'light))))
        (curr-weight (or (font-get doom-font :weight) 'normal)))
    (unless (equal weight curr-weight)
      (font-put doom-font :weight weight)
      (doom/reload-font))))

(add-hook 'doom-load-theme-hook #'kk/set-font-weight-by-light-or-dark -50)

;;;; Modeline

(after! doom-modeline
  (setq doom-modeline-hud nil
        doom-modeline-major-mode-icon nil
        doom-modeline-icon nil
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
    `(doom-modeline-lsp-success       :weight unspecified :inherit info)
    `(doom-modeline-buffer-major-mode :weight ,(face-attribute 'default :weight) :inherit doom-modeline-buffer-path))

  ;; Change the lsp icon to be something nicer
  (defun kk/doom-modeline-lsp-icon (text face)
    (doom-modeline-icon 'material "blur_circular" "{lsp}" text :face face))
  (advice-add 'doom-modeline-lsp-icon :override #'kk/doom-modeline-lsp-icon)

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
                                       (lambda ()
                                         (interactive)
                                         (describe-function 'flycheck-mode)))
                                     map)))))

  ;; simpler vcs icon - use text fallback instead of the actual icon
  (defun doom-modeline-vcs-icon (icon &optional unicode text face voffset)
    (propertize text 'face face))

  (doom-modeline-def-segment spacing
    doom-modeline-wspc)

  (doom-modeline-def-segment line-with-max
    (let* ((lc '(column-number-mode
                 (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                 "%l"))
           (curr-line (format-mode-line lc))
           (max-line (save-excursion
                       (goto-char (point-max))
                       (format-mode-line "%l"))))
      (concat
       doom-modeline-wspc
       (s-concat curr-line "/" max-line))))

  (doom-modeline-def-segment theme
    (let* ((face (if (doom-modeline--active)
                     'doom-modeline-buffer-minor-mode
                   'mode-line-inactive))
           (theme-name (symbol-name
                        (or (car-safe custom-enabled-themes)
                            'default)))
           (timed-themes-enabled (bound-and-true-p timed-themes-minor-mode)))
      (concat
       (propertize (if timed-themes-enabled
                       (concat "(" theme-name ")")
                     theme-name)
                   'face face
                   'mouse-face 'mode-line-highlight
                   'help-echo "Current theme")
       doom-modeline-spc)))

  (require 'delight)
  (delight 'emacs-lisp-mode "elisp" :major)

  (remove-hook '+doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)
  (add-hook '+doom-dashboard-mode-hook #'hide-mode-line-mode)

  (doom-modeline-def-modeline 'main
    '(workspace-name window-number matches buffer-info remote-host line-with-max word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding theme major-mode process vcs checker)))

;;;; Banner

(when (display-graphic-p)
  (require 'f)
  (setq +doom-dashboard-banner-dir (f-join doom-user-dir "banner-pictures")
        +doom-dashboard-banner-file "cacochan.png"
        +doom-dashboard-banner-padding '(6 . 6)))

;;; Projectile

(after! projectile
  (add-hook 'projectile-after-switch-project-hook
            (defun kk/set-default-directory-to-project-root ()
              (setq default-directory (projectile-project-root)))))

;;; Language-specific configs
;;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook #'prism-mode)

;; the checker for emacs-lisp sucks
(add-hook 'emacs-lisp-mode-hook (cmd! (flycheck-mode -1)))

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

  (add-hook! 'tuareg-mode-hook #'hs-minor-mode))

(after! dune
  ;; switch back-and-forth b/w ocaml and dune file
  (map! :map dune-mode-map
        :localleader
        :desc "visit previous file" "d" #'evil-switch-to-windows-last-buffer)
  (when (modulep! :editor lispy) (add-hook 'dune-mode-hook #'lispyville-mode)))

;;;; Purescript
(after! purescript-mode
  ;; (set-formatter! 'purty "purty -" :modes '(purescript-mode))
  (set-formatter! 'purs-tidy "purs-tidy format" :modes '(purescript-mode)))

;;;; Dhall
(use-package! dhall-mode
  :mode "\\.dhall\\'")

;;;; Elm
(use-package! elm-mode
  :hook (elm-mode . elm-format-on-save-mode)
  :config
  (defun kk/elm-color-hex-to-rgb (color-hex)
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

  (defun kk/elm-replace-color-hex-to-rgb255 ()
    (interactive)
    (require 'kurecolor)
    (kurecolor-replace-current #'kk/elm-color-hex-to-rgb)))

;; TODO add font-lock keywords to highlight `rgb255 r g b' expressions with appr color

;;;; Haskell
(after! haskell-mode
  (defun kk/haskell-hpack-find-pkg-desc (dir &optional allow-multiple)
    "Return hpack ('package.yaml') files instead of the generated cabal file."
    (let* ((cabal-files
            (cl-remove-if 'file-directory-p
                          (cl-remove-if-not 'file-exists-p
                                            (directory-files dir t "package\\.yaml\\'")))))
      (cond
       ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
       (allow-multiple cabal-files) ;; pass-thru multiple candidates
       (t nil))))

  (advice-add #'haskell-cabal-find-pkg-desc :before-until #'kk/haskell-hpack-find-pkg-desc)

  ;; Integrate `haskell-hide-all' etc. with the general fold bindings
  (map! :map haskell-mode-map
        :n "z o" #'haskell-hide-toggle
        :n "z c" #'haskell-hide-toggle
        :n "z a" #'haskell-hide-toggle
        :n "z m" #'haskell-hide-toggle-all
        :n "z r" #'haskell-hide-toggle-all)

  (setq haskell-process-suggest-remove-import-lines nil
        haskell-process-suggest-hoogle-imports t))

(use-package! flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package! hiedb
  ;; :hook (haskell-mode . hiedb-mode)
  :config
  (advice-add #'hiedb-module-from-path
              :before
              (defun kk/hiedb-set-project-paths ()
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

;;;; Minor-modes
;;;;; outshine

(after! outshine
  (map! :map outshine-mode-map
        :desc "narrow to subtree" :n "zn" #'outshine-narrow-to-subtree)

  (add-hook 'outshine-mode-hook
            (defun kk/outshine-compose-start ()
              "Make outshine (outline) headings prettier."
              (when outshine-mode
                (add-to-list 'font-lock-extra-managed-props 'display)
                (let* ((outline-rgxp (substring outline-regexp 0 -8))
                       (font-lock-new-keywords
                        `((,(concat outline-rgxp "\\{1\\} ") 0 '(face 'outshine-level-1 display "[1] ") t)
                          (,(concat outline-rgxp "\\{2\\} ") 0 '(face 'outshine-level-2 display "[2] ") t)
                          (,(concat outline-rgxp "\\{3\\} ") 0 '(face 'outshine-level-3 display "[3] ") t)
                          (,(concat outline-rgxp "\\{4\\} ") 0 '(face 'outshine-level-4 display "[4] ") t)
                          (,(concat outline-rgxp "\\{5\\} ") 0 '(face 'outshine-level-5 display "[5] ") t)
                          (,(concat outline-rgxp "\\{6\\} ") 0 '(face 'outshine-level-6 display "[6] ") t)
                          (,(concat outline-rgxp "\\{7\\} ") 0 '(face 'outshine-level-7 display "[7] ") t)
                          (,(concat outline-rgxp "\\{8\\} ") 0 '(face 'outshine-level-8 display "[8] ") t))))
                  (setf (car outshine-font-lock-keywords) (append (car outshine-font-lock-keywords) font-lock-new-keywords))
                  (font-lock-add-keywords nil font-lock-new-keywords 'append)
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

(after! lispyville
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
  (map! :map lispyville-mode-map :n "[" nil))

;;; Assorted

(setq confirm-kill-emacs nil)

;; stop the constant "Cleaning up the recentf list...done" messages
(defun silently (fn)
  "Run FN without showing any messages in echo area."
  (let ((inhibit-message t))
    (funcall fn)))

(advice-add 'recentf-cleanup :around #'silently)

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; don't want to put my cursor at the absolute edge.
;; get some context.
(setq scroll-margin 5)

;;; Keybindings
;;;; Main

(setq doom-leader-key "SPC"
      doom-leader-alt-key "M-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; learned habit from IntelliJ and VS Code
(map! :nv "C-/" #'evilnc-comment-or-uncomment-lines)

(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

(map! :leader "s I" #'imenu-list)

;;;; Line numbers

(map! :leader :desc "toggle line numbers" "t l"
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

(map! :leader :desc "random-themes-hydra" :n "h T"
      (cmd!
       (load! "local-packages/random-themes/random-themes.el" doom-user-dir)
       (random-themes--hydra/body)))
(map! :when (modulep! :ui hl-todo) :leader :desc "search for todos" :n "s t" #'hl-todo-occur)

;;;; LSP
(map! :when (modulep! :tools lsp) :map lsp-mode-map :localleader :desc "rename" :n "r" #'lsp-rename)

;;;; Version control
(map! :when (modulep! :ui vc-gutter) :leader
      :desc "Show git diff at point" :n "g d" #'git-gutter:popup-diff)

(when (modulep! :emacs vc)
  (run-with-idle-timer 5 t #'vc-refresh-state))

;;; Notes
;; TODO Check out what `hyperbole' can do for you
;; TODO Change `imenu-list' to differentiate between headlines of diff depths
;; TODO Check out dynamic views in `org-ql' to make a better agenda
;; TODO Fix org-edit-src:
;;      1. Make the buffer name be not as horrible
;;      2. Remove the top-bar saying C-c, C-k
;;      3. Bind :w and :q to write/abort
