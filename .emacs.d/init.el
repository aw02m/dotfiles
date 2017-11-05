;;;----
;;; Basic settings
;;;----
;; initialization package system
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; elisp path
(setq load-path (cons "~/.emacs.d/elisp" load-path))
(load (expand-file-name "~/.roswell/helper.el"))

;; set cording to UTF-8
(prefer-coding-system 'utf-8)

;; don't show startup message
(setq inhibit-startup-message t)

;; show line and column number
(global-linum-mode t)
(setq linum-format "%3d ")
(column-number-mode t)

;; don't sound ring-bell
(setq ring-bell-function 'ignore)

;; highlight parentheses
(show-paren-mode t)

;; indent settings
(setq-default tab-width 2 indent-tabs-mode nil)

;; show file size
(size-indication-mode t)

;; don't show tool and menu bar
(if window-system (tool-bar-mode -1) (tool-bar-mode -1))
(if window-system (menu-bar-mode -1) (menu-bar-mode -1))

;; show path on title bar
(setq frame-title-format "%f")

;; color theme
(load-theme 'wombat t)

;;autosave settings
;;dont make save-list-file
(setq auto-save-list-file-prefix nil)
;;tmpolary file is maked on...
(setq auto-save-file-name-transforms   '((".*" "~/tmp/" t)))
;; save interval
(setq auto-save-timeout 10)
(setq auto-save-interval 100)
;; backup is on "~/.ehist"
(setq backup-directory-alist '((".*" . "~/.ehist")))
;; save some file
(setq version-control     t)
(setq kept-new-versions   4)
(setq kept-old-versions   1)
(setq delete-old-versions t)

;;;----
;;; Keybinds
;;;----
;; set backspace to C-h
(define-key global-map (kbd "C-h") 'delete-backward-char)

;; set numeric keypad
(global-set-key (kbd "M-O k") "+")
(global-set-key (kbd "M-O m") "-")
(global-set-key (kbd "M-O j") "*")
(global-set-key (kbd "M-O o") "/")

;; window resizer
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(define-key global-map "\C-q" (make-sparse-keymap))
(global-set-key "\C-q\C-q" 'quoted-insert)
(global-set-key "\C-q\C-r" 'window-resizer)
(global-set-key "\C-ql" 'windmove-right)
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)

;;;----
;;; Company-mode
;;;----
(use-package company
             :config
             (global-company-mode)
             (setq company-idle-delay 0.1
                   company-minimum-prefix-length 2
                   company-selection-wrap-around t)

             (bind-keys :map company-mode-map
                        ("C-i" . company-complete))
             (bind-keys :map company-active-map
                        ("C-n" . company-select-next)
                        ("C-p" . company-select-previous)
                        ("C-s" . company-search-words-regexp))
             (bind-keys :map company-search-map
                        ("C-n" . company-select-next)
                        ("C-p" . company-select-previous)))
(global-company-mode)

;;;----
;;; Scheme-mode
;;;----
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'scheme-mode "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window
        (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))
(define-key global-map "\C-cs" 'scheme-other-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company clj-refactor cider clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;----
;;; Clojure
;;;----
(use-package clojure-mode
             :init
             (add-hook 'clojure-mode-hook #'yas-minor-mode)
             (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
             :init
             (add-hook 'cider-mode-hook #'clj-refactor-mode)
             (add-hook 'cider-mode-hook #'company-mode)
             (add-hook 'cider-mode-hook #'eldoc-mode)
             (add-hook 'cider-repl-mode-hook #'company-mode)
             (add-hook 'cider-repl-mode-hook #'eldoc-mode)
             :diminish subword-mode
             :config
             (setq nrepl-log-messages t
                   cider-repl-display-in-current-window t
                   cider-repl-use-clojure-font-lock t
                   cider-prompt-save-file-on-load 'always-save
                   cider-font-lock-dynamically '(macro core function var)
                   cider-overlays-use-font-lock t)
             (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
             :diminish clj-refactor-mode
               :config (cljr-add-keybindings-with-prefix "C-c j"))
