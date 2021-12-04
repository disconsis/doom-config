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

(defun between (start item end)
  (and (<= start item)
       (<= item end)))

(defun kk/load-doom-theme ()
  "Load the currently set `doom-theme'."
  (interactive)
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

;;;; LSP
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

;;;; Font
(setq doom-font (font-spec :family "Iosevka" :size 14))

;;;; Theme
(setq kk/theme-timings `((doom-flatwhite         . (,(am 6) . ,(pm 3)))
                         (doom-monokai-ristretto . (,(pm 4) . ,(pm 8)))
                         (doom-sourcerer         . (,(pm 9) . ,(am 5)))))

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
       :localleader
       :desc "visit corresp. dune file" "d" #'kk/find-dune-file)
 (setq +default-want-RET-continue-comments nil
       +evil-want-o/O-to-continue-comments nil))

;;; Assorted

(setq confirm-kill-emacs nil)

(defun kk/try-all-themes ()
  "Go through all themes in a shuffled order."
  (interactive)
  (let* ((all-themes (nshuffle (custom-available-themes)))
         (theme-idx 1)
         (num-themes (length all-themes)))
   (dolist (theme all-themes)
     (setq theme-idx (+ 3 theme-idx))
     (load-theme theme t nil)
     (when (not (yes-or-no-p (format "current theme: %s [%d/%d]  Go to next theme? " theme theme-idx num-themes)))
       (return theme)))))

(setq initial-frame-alist '((top . 23) (left . 0) (height . 56) (width . 272)))

;;; Notes

;; TODO Check out rougier's `svg-lib' - seems pretty fun
