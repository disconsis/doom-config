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

;;;; Look
(use-package! valign
  :defer
  :init
  (setq valign-fancy-bar t)
  (add-hook 'org-mode-hook #'valign-mode))

;;;; Capture templates

(setq +org-capture-work-tasks-file (expand-file-name "work_tasks.org" org-directory))

(setq org-capture-templates
      '(("w" "Work tasks" entry
         (file +org-capture-work-tasks-file)
         "* TODO %^{title}\n:PROPERTIES:\nDATE: %t\n:END:\n%?"
         :prepend t
         :jump-to-capture t
         :empty-lines 1)))

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

;;;; Font

(setq doom-font (font-spec :family "Iosevka" :size 14))

;; fix the weird fixed-pitch font in Info manuals
(custom-theme-set-faces! 'user
  '(fixed-pitch-serif :inherit 'default))

(setq doom-font-increment 1)

;;;; Theme
;;;;; Timed changes

(load! "local-packages/timed-themes/timed-themes.el" doom-private-dir)

(setq timed-themes/theme-timings
      `((doom-one-light         . (,(am 6) . ,(pm 3)))
        (doom-ayu-mirage        . (,(pm 4) . ,(pm 8)))
        (doom-sourcerer         . (,(pm 9) . ,(am 5))))

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
  (setq modus-themes-mode-line '(accented borderless)))

(after! prism
  (setq prism-colors
        '(font-lock-type-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-builtin-face)
        prism-num-faces 8
        prism-color-distance 40000
        prism-desaturations '(0)
        prism-lightens '(0)))

;;;; Modeline

(after! doom-modeline
  (setq doom-modeline-hud t
        doom-modeline-major-mode-icon t
        ;; reduce the size of icons in the modeline so that it doesn't get cut off at the end
        all-the-icons-scale-factor 1.1)

  (remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)
  (custom-theme-set-faces! 'user
    ;; Modeline icons use this face, which has :inherit `success' and `bold'.
    ;; I don't know any way of removing the :inherit `bold' without just specifying everything else it inherits from.
    '(doom-modeline-info :inherit 'success)
    ;; similarly for warning
    '(doom-modeline-warning :inherit 'warning))

  (doom-modeline-def-segment theme
    (let ((face (if (doom-modeline--active)
                    'doom-modeline-buffer-minor-mode
                  'mode-line-inactive))
          (theme-name (symbol-name
                       (or (car-safe custom-enabled-themes)
                           'default))))
      (concat
       (doom-modeline-spc)
       (and doom-modeline-icon
            (concat (doom-modeline-icon
                     'material "color_lens" "" ""
                     :face face)
                    (doom-modeline-vspc)))
       (propertize theme-name
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
        :desc "narrow to subtree" :n "zn" #'outshine-narrow-to-subtree))

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

;;;; Other

;; take back ~s~
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; vim-vinegar
(map! :m "-" #'dired-jump)

(map!
 :desc "format region" :v "gq" #'+format/region ; This is the keybinding I always reach for to format a region
 :desc "format buffer" :n "gQ" #'+format/buffer)

(map! :leader :desc "random-themes-hydra" :n "h T" #'hydra-random-themes/body)

(when (featurep! :ui hl-todo)
  (map! :leader :desc "search for todos" :n "s t" #'hl-todo-occur))

;;; Notes

;; TODO Check out rougier's `svg-lib' - seems pretty fun
