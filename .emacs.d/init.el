;;===========================================
;;  基本設定
;;===========================================
;;----
;; elisp path
;;----

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "~/.emacs.d/elisp" load-path))

;;----
;; UTF-8
;;----
(prefer-coding-system 'utf-8)

;;----
;; スタートアップページを表示しない
;;----
(setq inhibit-startup-message t)

;;----
;; 行番号表示
;;----
(global-linum-mode t)
(setq linum-format "%3d ")

;;----
;; カラム番号
;;----
(column-number-mode t)

;;----
;; ビープ音を消す
;;----
(setq ring-bell-function 'ignore)

;;----
;; カーソル行に下線を表示
;;----
;; 対応する括弧を強調表示
;;----
(show-paren-mode t)

;;----
;; indent settings
;;----
(setq-default tab-width 2 indent-tabs-mode nil)

;;----
;; ファイルサイズ表示
;;----
(size-indication-mode t)

;;----
;; ツールバー,メニューバーを非表示
;; M-x tool-bar-mode, M-x menu-bar-mode で表示非表示を切り替えられる
;;----
(if window-system (tool-bar-mode -1) (tool-bar-mode -1))
(if window-system (menu-bar-mode -1) (menu-bar-mode -1))

;;----
;; タイトルバーにフルパス表示
;;----
(setq frame-title-format "%f")

;;----
;; カラーテーマ
;;----
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))
(set-alpha 90)
(load-theme 'wombat t)
;;color-theme.el
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-zenburn)

;;----
;; eww default search engine
;;----
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;;===========================================
;; キーボード操作系
;;===========================================
;;----
;; バックスペースをC-hにする
;;----
(define-key global-map (kbd "C-h") 'delete-backward-char)

;;----
;; window resizer
;;----
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


;;----
;; window move (shift-pointerkey)
;;----
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;===========================================
;; オートセーブ系
;;===========================================
;;configure of autosavefile
;;dont make save-list-file
(setq auto-save-list-file-prefix nil)
;;tmpolary file is maked on...
(setq auto-save-file-name-transforms   '((".*" "~/tmp/" t)))
;; 保存の間隔
(setq auto-save-timeout 10)     ;; 秒   (デフォルト : 30)
(setq auto-save-interval 100)   ;; 打鍵 (デフォルト : 300)
;;backup file
(setq backup-directory-alist '((".*" . "~/.ehist")))
;; 番号付けによる複数保存
(setq version-control     t)  ;; 実行の有無
(setq kept-new-versions   4)  ;; 最新の保持数
(setq kept-old-versions   1)  ;; 最古の保持数
(setq delete-old-versions t)  ;; 範囲外を削除


;;=============================================
;; Scheme-mode
;;=============================================
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


;;===============================================
;; Common-Lisp
;;===============================================
;(setq inferior-lisp-program "sbcl")
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/slime"))
;(require 'slime)
;(slime-setup '(slime-repl slime-fancy slime-banner))

;;===============================================
;; roswell
;;===============================================
(load (expand-file-name "~/.roswell/helper.el"))
