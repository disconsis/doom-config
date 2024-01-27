;;; random-themes.el --- random theme chooser
;;; -*- lexical-binding: t; -*-

;;; Author: Ketan Kanishka

;;; Commentary:

;;; Code:

(require 'cl)
(require 'cl-lib)

(defun random-themes/nshuffle (seq)
  "Shuffle SEQ in place. Picked up from somewhere on the internet."
  (loop
   for i from (length seq) downto 2
   do (rotatef (elt seq (random i))
               (elt seq (1- i)))
   finally return seq))

(defvar random-themes--themes nil)

(defvar random-themes--idx nil)

(defun random-themes--set (idx)
  (setq random-themes--idx idx)
  (load-theme (elt random-themes--themes random-themes--idx) t nil))

(defun random-themes--prev ()
  (random-themes--set
        (% (1- random-themes--idx) (length random-themes--themes))))

(defun random-themes--next ()
  (random-themes--set
        (% (1+ random-themes--idx) (length random-themes--themes))))

(defun random-themes--shuffle ()
  (setq random-themes--themes (or random-themes--themes (apply #'vector (custom-available-themes))))
  (random-themes/nshuffle random-themes--themes)
  (random-themes--set 0))

(defvar random-themes--theme-at-start 'default
  "The theme that `random-themes-hydra' was entered with, in case the user wants to go back to it.")

(defun random-themes--hydra-gen-docstring (num-surrounding)
  "Generate docstring for `random-themes-hydra' with NUM-SURROUNDING themes before and after (each) the current one."
  (let* ((num-themes (length random-themes--themes))
         (rows (--reduce-r-from
                (destructuring-bind (theme-row num-row) acc
                  (let* ((theme-idx (mod (+ it random-themes--idx) num-themes))
                         (theme-name (symbol-name (elt random-themes--themes theme-idx)))
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
              (s-concat (s-center (frame-width) (s-join "   " it)) "\n")
              (list (-concat '("...") theme-row '("..."))
                    (-concat '("   ")  num-row  '("   "))))))))

(defun random-themes--search ()
  (interactive)
  (-> (completing-read "Find theme: "
                       (mapcar #'symbol-name random-themes--themes)
                       nil t)
      intern
      (position random-themes--themes)
      random-themes--set))

(defhydra random-themes--hydra
  (:color red
   :body-pre
   (progn
     (setq random-themes--theme-at-start (or (car-safe custom-enabled-themes) 'default))
     (if (and random-themes--themes random-themes--idx)
         (load-theme (elt random-themes--themes random-themes--idx) t nil)
       (random-themes--shuffle))))
  "
%s(random-themes--hydra-gen-docstring 3)
"
  ("k" (random-themes--prev) "prev-theme")
  ("j" (random-themes--next) "next-theme")
  ("h" (random-themes--prev) "prev-theme")
  ("l" (random-themes--next) "next-theme")
  ("r" random-themes--shuffle "randomize")
  ("/" random-themes--search "search")
  ("q" (load-theme random-themes--theme-at-start t nil) "reset to initial" :color blue)
  ("." nil "confirm and quit" :color blue))
