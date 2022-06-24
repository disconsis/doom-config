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

(defun kk/nshuffle (sequence)
  "Shuffle SEQUENCE in place. Picked up from somewhere on the internet."
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

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
          (?-  . ,(all-the-icons-material "indeterminate_check_box")))))

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
    (message "regenerating latex previews...")
    (when (fboundp #'+org--toggle-inline-images-in-subtree)
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

;;; Filesystem
(add-hook! dired-mode #'dired-hide-details-mode)

(after! dired-x
  (setq dired-omit-files (rx line-start (repeat 1 2 (char ".")))))

(after! recentf
  (add-to-list 'recentf-exclude (concat "^" doom-local-dir)))

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
      lsp-enable-folding t)

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
(setq kk/single-monitor-width 1920)
(let ((rightmost-monitor-left-pos (* kk/single-monitor-width (1- (mod (x-display-pixel-width) kk/single-monitor-width)))))
  (setq initial-frame-alist
        `((top . 29) (left . ,rightmost-monitor-left-pos) (height . 58) (width . 267))))

;;;; Font

(setq doom-font (font-spec :family "Iosevka" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Lato"))

;; fix the weird fixed-pitch font in Info manuals
(custom-theme-set-faces! 'user
  '(fixed-pitch-serif :inherit 'default))

(setq doom-font-increment 1)

;;;; Theme
;;;;; Timed changes

(load! "local-packages/timed-themes/timed-themes.el" doom-private-dir)

(setq timed-themes/theme-timings
      `((light-soap             . (,(am 6) . ,(pm 1)))
        (autumn-light           . (,(pm 1) . ,(pm 5)))
        (doom-monokai-octagon   . (,(pm 5) . ,(am 5))))

      timed-themes/default-theme 'wombat ;; different enough to be noticeable, but won't accidentally blind me
      timed-themes/change-theme-if-manually-set nil
      timed-themes/change-theme-default-on-ask-timeout nil)

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
  (kk/nshuffle kk/random-themes-list)
  (kk/set-random-theme-idx 0))

(defvar kk/random-theme-at-start 'default
  "The theme that `hydra-random-themes' was entered with, in case the user wants to go back to it.")

(defun hydra-random-themes-gen-docstring (num-surrounding)
  "Generate docstring for `hydra-random-themes' with NUM-SURROUNDING themes before and after (each) the current one."
  (require 'cl)
  (let* ((num-themes (length kk/random-themes-list))
         (-frame-width- (frame-width))
         (rows (--reduce-r-from
                (destructuring-bind (theme-row num-row) acc
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

    (destructuring-bind (theme-row num-row) rows
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
  ("h" (kk/set-random-theme-idx (1- kk/random-theme-idx)) "prev-theme")
  ("l" (kk/set-random-theme-idx (1+ kk/random-theme-idx)) "next-theme")
  ("r" kk/random-themes-reshuffle "randomize")
  ("/" kk/search-random-theme "search")
  ("q" (load-theme kk/random-theme-at-start t nil) "reset to initial" :color blue)
  ("." nil "confirm and quit" :color blue))

;;;;; Theme modifications

(after! doom
  (custom-theme-set-faces! 'doom-ayu-mirage
    '(line-number :foreground "gray28")))

(after! modus-themes
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-subtle-line-numbers t))

(after! light-soap-theme
  (custom-theme-set-faces! 'light-soap
    '(fringe :background nil)))

(after! autumn-light-theme
  (custom-theme-set-faces! 'autumn-light
    '(mode-line :background "tan")))

;;;;; Prism

(after! prism
  (setq prism-colors
        '(font-lock-type-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-builtin-face)
        prism-num-faces 8
        prism-color-distance 40000
        prism-desaturations '(0)
        prism-parens t
        prism-lightens '(0)))

;;;; Modeline

(after! doom-modeline
  (setq doom-modeline-hud t
        doom-modeline-major-mode-icon t
        ;; reduce the size of icons in the modeline so that it doesn't get cut off at the end
        all-the-icons-scale-factor 1.1)

  (remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)
  (custom-theme-set-faces! 'user
    '(doom-modeline-debug       :weight normal :inherit font-lock-doc-face)
    '(doom-modeline-info        :weight normal :inherit info)
    '(doom-modeline-warning     :weight normal :inherit warning)
    '(doom-modeline-urgent      :weight normal :inherit error)
    '(doom-modeline-lsp-error   :weight normal :inherit error)
    '(doom-modeline-lsp-warning :weight normal :inherit warning)
    '(doom-modeline-lsp-success :weight normal :inherit info))

  ;; Change the lsp icon to be something nicer
  (defun kk/doom-modeline-lsp-icon (text face)
    (doom-modeline-icon 'material "blur_circular" "{lsp}" text :face face))
  (advice-add 'doom-modeline-lsp-icon :override #'kk/doom-modeline-lsp-icon)

  (doom-modeline-def-segment theme
    (let* ((face (if (doom-modeline--active)
                     'doom-modeline-buffer-minor-mode
                   'mode-line-inactive))
           (theme-name (symbol-name
                        (or (car-safe custom-enabled-themes)
                            'default)))
           (timed-themes-enabled (bound-and-true-p timed-themes-minor-mode))
           (prism-enabled (bound-and-true-p prism-mode))
           (special-modes-enabled (or timed-themes-enabled prism-enabled)))
      (concat
       (doom-modeline-spc)
       (when doom-modeline-icon
         (concat
          (unless special-modes-enabled
            (doom-modeline-icon 'material "color_lens" "" "" :face face))
          (when timed-themes-enabled
            (propertize (doom-modeline-icon 'material "timer" "" "" :face face)
                        'face face
                        'mouse-face 'mode-line-highlight
                        'help-echo "Timed theme changes\nmouse-1: Disable"
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1] (cmd! (timed-themes-minor-mode -1)))
                                     map)))
          (when prism-enabled
            (propertize (doom-modeline-icon 'material "details" "" "" :face face)
                        'face face
                        'mouse-face 'mode-line-highlight
                        'help-echo "Color distributed according to depth\nmouse-1: Disable"
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1] (cmd! (prism-mode -1)))
                                     map)))
          (doom-modeline-vspc)))
       (propertize (concat theme-name
                           (when (and (not doom-modeline-icon) special-modes-enabled)
                             (concat
                              "("
                              (when timed-themes-enabled "t")
                              (when prism-enabled "p")
                              ")")))
                   'face face
                   'mouse-face 'mode-line-highlight
                   'help-echo "Current theme")
       (doom-modeline-spc))))

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding theme major-mode process vcs checker)))

;;; Language-specific configs
;;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook #'prism-mode)

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
  (when (featurep! :editor lispy) (add-hook 'dune-mode-hook #'lispyville-mode)))

;;;; Purescript
(after! purescript-mode
  ;; (set-formatter! 'purty "purty -" :modes '(purescript-mode))
  (set-formatter! 'purs-tidy "purs-tidy format" :modes '(purescript-mode)))

;;;; Dhall
(use-package! dhall-mode
  :mode "\\.dhall\\'")

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
                        `((,(concat outline-rgxp "\\{1\\} ") 0 '(face 'outshine-level-1 display "○ ") t)
                          (,(concat outline-rgxp "\\{2\\} ") 0 '(face 'outshine-level-2 display "○○ ") t)
                          (,(concat outline-rgxp "\\{3\\} ") 0 '(face 'outshine-level-3 display "○○○ ") t)
                          (,(concat outline-rgxp "\\{4\\} ") 0 '(face 'outshine-level-4 display "○○○○ ") t)
                          (,(concat outline-rgxp "\\{5\\} ") 0 '(face 'outshine-level-5 display "○○○○○ ") t)
                          (,(concat outline-rgxp "\\{6\\} ") 0 '(face 'outshine-level-6 display "○○○○○○ ") t)
                          (,(concat outline-rgxp "\\{7\\} ") 0 '(face 'outshine-level-7 display "○○○○○○○ ") t)
                          (,(concat outline-rgxp "\\{8\\} ") 0 '(face 'outshine-level-8 display "○○○○○○○○ ") t))))
                  (setf (car outshine-font-lock-keywords) (append (car outshine-font-lock-keywords) font-lock-new-keywords))
                  (font-lock-add-keywords nil font-lock-new-keywords 'append)
                  (outshine-font-lock-flush)))))

  (setq outshine-fontify-whole-heading-line t)
  (custom-theme-set-faces! 'user
    '(outshine-level-1 :height 2.0 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-2 :height 1.8 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-3 :height 1.6 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-4 :height 1.4 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-5 :height 1.3 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-6 :height 1.2 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-7 :height 1.1 :inherit magit-diff-hunk-heading-highlight)
    '(outshine-level-8 :height 1.1 :inherit magit-diff-hunk-heading-highlight)))

;;; Assorted

(setq confirm-kill-emacs nil)

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

(map! :leader :desc "random-themes-hydra" :n "h T" #'hydra-random-themes/body)
(map! :when (featurep! :ui hl-todo) :leader :desc "search for todos" :n "s t" #'hl-todo-occur)

;;;; LSP
(map! :when (featurep! :tools lsp) :map lsp-mode-map :localleader :desc "rename" :n "r" #'lsp-rename)

;;;; Version control
(map! :when (featurep! :ui vc-gutter) :leader
      :desc "Show git diff at point" :n "g d" #'git-gutter:popup-diff)

;;; Notes
;; TODO Check out what `hyperbole' can do for you
;; TODO Change `imenu-list' to differentiate between headlines of diff depths
