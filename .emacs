;; ====== packages ======

;; Specify packages that should be loaded
(setq package-list
      '(web-mode
        multiple-cursors))

;; list the repositories containing them
(setq package-archives '(("elpa"      . "http://tromey.com/elpa/")
			 ("melpa"     . "https://melpa.org/packages/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Actiave all packages
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ====== multiple-cursors settings ======

(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor) 

;; ====== functions ======

;; make emacs open two vertical windows instead of horizontal
(defun 2-windows-vertical-to-horizontal ()
  "Make Emacs open two vertical windows instead of horizontal."
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (<= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers))
      (switch-buffers-between-frames))))

;; ====== settings ======

;; remove unneccessary GUI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; font size
(set-face-attribute 'default nil :height 90)

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
(global-linum-mode t)

;; smooth scrolling
(setq scroll-step 1)

;; don't add newlines after current line
(setq next-line-add-newlines nil)

;; make sure there is always a newline at end
(setq require-final-newline t)

;; make sure emacs open multiple files horizontally, not vertically
(add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; ====== auto generated ======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (multiple-cursors web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
