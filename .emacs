;; ====== packages ======

;; Specify packages that should be loaded
(setq package-list
      '(web-mode
        multiple-cursors
        flycheck
        lsp-mode
        ccls
	company
        use-package
        clang-format
        sublime-themes))

;; list the repositories containing them
(setq package-archives '(("elpa"      . "http://tromey.com/elpa/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; Actiave all packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load theme
(load-theme 'spolsky t)

;; ====== External files ========

;; Add the home folder to the load path
(add-to-list 'load-path "~/")

;; load my custom built packages
(require '.handout)
(require '.latex)
(require '.auto-jump)
(require '.block-comment)
(require '.manage)

;; (use-package typst-ts-mode
;;   :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git"))


;; ====== ispell ======

(setq ispell-program-name "/usr/bin/hunspell")
(ispell-change-dictionary "english" 't)

;; ====== auto-jump ======

(global-set-key (kbd "C-c SPC") 'auto-jump-deduce)
(global-set-key (kbd "C-c M-t") 'toggle-handout)
(global-set-key (kbd "C-c M-a") 'add-handout)

;; ====== CCLS ======

;; (use-package lsp-mode :commands lsp)

;; (setq lsp-diagnostic-package :flycheck)
;; (setq lsp-diagnostics-provider :flycheck)

;; (setq lsp-enable-file-watchers nil)

;; (setq lsp-lens-enable nil)
;; (setq lsp-enable-on-type-formatting nil)

;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

;; (setq ccls-executable (executable-find "ccls"))

;; ====== multiple-cursors settings ======

(require 'multiple-cursors)

(defun multi-selection-up ()
  (interactive)
    (if (not (memq 'multi-selection-up mc/cmds-to-run-once))
      (add-to-list 'mc/cmds-to-run-once 'multi-selection-up))
  (mc/mark-previous-lines 1))

(defun multi-selection-down ()
  (interactive)
  (if (not (memq 'multi-selection-down mc/cmds-to-run-once))
      (add-to-list 'mc/cmds-to-run-once 'multi-selection-down))
  (mc/mark-next-lines 1))

(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)

(global-set-key [M-down] 'multi-selection-down)
(global-set-key [M-up] 'multi-selection-up)

;; ====== Web Mode ======

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; ====== C mode ======

(setq-default fill-column 80)
(add-hook 'c-mode-hook
          '(lambda ()
             (local-set-key [13] 'c-return)
             (c-set-style "bsd")
             (setq c-basic-offset 4)
             (setq fill-column 80)
             (c-set-offset 'substatement-open 0)))

;; ====== C++ mode ======

(require 'cc-mode)

;; associate file names with c++-mode
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))

(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "bsd")
             (setq c-basic-offset 4)
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'statement-cont 0)
             (local-set-key [C-tab] 'clang-format-region)
             (hs-minor-mode)
             (local-set-key (kbd "M-s M-a") 'hs-show-block)
             (local-set-key (kbd "M-s M-d") 'hs-hide-block)
             (local-set-key (kbd "M-s M-s") 'hs-toggle-hiding)
             (hide-ifdef-mode)
             (local-set-key (kbd "M-s M-q") 'show-ifdef-block)
             (local-set-key (kbd "M-s M-e") 'hide-ifdef-block)))

;; ====== Storm ======

(if (file-exists-p "~/git/storm/Plugin/emacs.el")
    (progn (load "~/git/storm/Plugin/emacs.el")
           (setq storm-mode-compiler "~/git/storm/debug/Storm")
           (setq storm-mode-root "~/git/storm/root")
           (add-to-list 'auto-mode-alist '("\\.bs$" . java-mode))
           (global-storm-mode t)))

;; ====== settings ======

;; remove unneccessary GUI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; activate windmove
(windmove-default-keybindings)

;; font size

;; Normal
(set-face-attribute 'default nil :height 110)

;; Lecture
;; (set-face-attribute 'default nil :height 180)

;; no tabs
(setq-default indent-tabs-mode nil)

;; C-k kills entire line if at start of line
(setq kill-whole-line t)

;; don't generate backup files
(setq make-backup-files nil)

;; show position in file
(setq line-number-mode t)
(setq column-number-mode t)

;; show lines to the left
(global-display-line-numbers-mode 1)

;; smooth scrolling
(setq scroll-step 1)

;; don't add newlines after current line
(setq next-line-add-newlines nil)

;; make sure there is always a newline at end
(setq require-final-newline t)

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; Don't wrap lines
(set-default 'truncate-lines t)

(defun make-center-banner ()
  (interactive)
  (let* ((content       (delete-and-extract-region (mark) (point)))
         (len           (length content))
         (padding-len   (- 80 len))
         (left-padding  (/ padding-len 2))
         (right-padding (- padding-len left-padding)))
    (insert ";")
    (dotimes (_ (- left-padding 2)) (insert "="))
    (insert " ")
    (insert content)
    (insert " ")
    (dotimes (_ (- right-padding 2)) (insert "="))
    (insert ";")))

;; ====== Auto generated ======
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cmake-mode use-package web-mode typst-ts-mode sublime-themes multiple-cursors highlight-doxygen flycheck company clang-format ccls))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
