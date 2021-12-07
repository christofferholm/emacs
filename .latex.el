(provide '.latex)

;; (add-hook 'latex-mode-hook 'latex-setup)

;; (defun latex-setup ()
;;   (local-set-key (kbd "C-c f") 'latex-frame))

;; (defun latex-frame ()
;;   (interactive))

;; (defun latex-environment-generate (environment format &optional values)
;;   (interactive)
;;   (insert "\\begin{" environment "}")
;;   (indent-for-tab-command)
;;   (let ((to (latex-environment-generate-format format values)))
;;     (insert "\\end{" environment "}")
;;     (indent-for-tab-command)
;;     (goto-char to)))

;; (defun latex-environment-generate-format (format values)
;;   (let ((current (car format)))

(require '.auto-jump)

(add-hook 'latex-mode-hook 'latex-setup)

(defun latex-setup ()
  (local-set-key (kbd "M-p") 'compile-latex)
  (setq fill-column 60)
  (local-set-key (kbd "<C-return>") 'fill-paragraph)
  (local-set-key (kbd "C-c i") 'latex-italics)
  (local-set-key (kbd "C-c v") 'latex-verb)
  (local-set-key (kbd "C-c b") 'latex-bold)
  (local-set-key (kbd "C-c u") 'latex-itemize)
  (local-set-key (kbd "C-c f") 'latex-frame)
  (local-set-key (kbd "C-c h") 'latex-handout-frame)
  (local-set-key (kbd "C-c l") 'latex-lstlisting)
  (local-set-key (kbd "C-c M-f") 'latex-frame-toggle-fragile)
  (local-set-key (kbd "C-x w") 'ispell-word)
  (local-set-key (kbd "C-c c") 'latex-columns)
  (local-set-key (kbd "C-c <") 'latex-onlyenv)
  (add-to-list 'fill-nobreak-predicate 'my-latex-fill-nobreak-predicate)
  (add-to-list 'fill-nobreak-predicate 'my-math-fill-nobreak-predicate)
  (auto-jump-init '(("frame" . ("\\begin{frame}{" "}\n" "\n\\end{frame}"))
                    ("lst" . ("\\lstinline" ""))))
  )

(defvar latex-compiling-file nil)

(defun marked-string ()
  (if mark-active
      (let ((m (buffer-substring (mark) (point))))
	(delete-region (mark) (point))
	m)
    ""))

(defun latex-italics ()
  (interactive)
  (let ((marked (marked-string)))
    (insert "\\emph{}")
    (goto-char (- (point) 1))
    (insert marked)))

(defun latex-verb ()
  (interactive)
  (let ((marked (marked-string)))
    (insert "\\texttt{}")
    (goto-char (- (point) 1))
    (insert marked)))

(defun latex-bold ()
  (interactive)
  (let ((marked (marked-string)))
    (insert "\\textbf{}")
    (goto-char (- (point) 1))
    (insert marked)))

(defun latex-itemize ()
  (interactive)
  (insert "\\begin{itemize}")
  (indent-for-tab-command)
  (insert "\n\\item ")
  (indent-for-tab-command)
  (let ((to (point)))
    (insert "\n\\end{itemize}")
    (indent-for-tab-command)
    ;; (insert "\n")
    ;; (if (not (blank-line))
    ;; 	(indent-for-tab-command))
    (goto-char to)))

(defun latex-find-previous-section-title ()
  (interactive)
  (let ((home (point)))
    (if (search-backward "\\section{" nil t)
        (progn
          (search-forward "{")
          (let ((start (point)))
            (if (search-forward "}" nil t)
                (progn 
                  (backward-char 1)
                  (let ((title (buffer-substring-no-properties start (point))))
                    (goto-char home)
                    title))
              nil)))
      nil)))


(defun latex-frame-toggle-fragile ()
  (interactive)
  (save-excursion
    (let* ((home (point))
           (frame-start (progn (search-backward "\\begin{frame}") (point)))
           (frame-end (progn (search-forward "\\end{frame}") (point))))
      (if (and (< frame-start home) (< home frame-end))
          (save-restriction
            (narrow-to-region frame-start frame-end)
            (goto-char frame-start)
            (if (re-search-forward "\\\\begin{frame}[\\n\\t ]*[[]" nil t)
                (let* ((options-start (point))
                       (options-end (progn (search-forward "]") (- (point) 1)))
                       (options (split-string (delete-and-extract-region
                                               options-start options-end)
                                              ",")))
                  (delete-region (- options-start 1) (+ options-start 1))
                  (if (cl-find-if (lambda (x) (string-match ".*fragile.*" x)) options)
                      (setq options (cl-remove-if 
                                     (lambda (x) (or (string-match ".*fragile.*" x) (string= x ""))) 
                                     options))
                    (setq options (append (cl-remove-if (lambda (x) (string= x "")) options) '("fragile"))))
                  (goto-char (- options-start 1))
                  (if options
                      (insert "[" (cl-reduce (lambda (a b) (concat a "," b)) options) "]")))
              (progn 
                (search-forward "\\begin{frame}")
                (insert "[fragile]"))))))))

(defun latex-frame ()
  (interactive)
  (let ((title (latex-find-previous-section-title)))
    (if title
        ;; if there is a previous section title then use it as a frame title
        (auto-jump-generate '("\\begin{frame}{" "}\n\\framesubtitle{" "}\n" "\n\\end{frame}")
                            (cons title '()))
      ;; if there is no previous section title then there is no frame title at all
      (auto-jump-generate '("\\begin{frame}\n\\framesubtitle{" "}\n" "\n\\end{frame}") '()))))

(defun latex-handout-frame ()
  (interactive)
  (let ((title (latex-find-previous-section-title))
        (snippets '("%%[BEGIN HANDOUT]\n\\begin{frame}{" "}\n\\framesubtitle{" "}\n" "\n\\end{frame}\n%%[END HANDOUT]")))
    (auto-jump-generate snippets (cons title '()))))

(defun latex-onlyenv ()
  (interactive)
  (auto-jump-generate '("\\begin{onlyenv}<" ">\n" "\n\\end{onlyenv}")))

(defun latex-lstlisting ()
  (interactive)
  (insert "\\begin{lstlisting}")
  (indent-for-tab-command)
  (insert "\n")
  (let ((home (point)))
    (insert "\n\\end{lstlisting}")
    (indent-for-tab-command)
    (goto-char home)))

(defun latex-columns ()
  (interactive)
  (let* ((count (read-number "How many columns?" 2))
         (width (/ 1.0 count))
        (snippets '()))
    (dotimes (i count)
      (setq snippets
            (cons (concat 
                   (if (= i (- count 1)) 
                       "\\begin{columns}\n\\begin{column}{" 
                     "\n\\end{column}\n\\begin{column}{") 
                   (number-to-string width) 
                   "\\textwidth}\n") snippets)))
    (setq snippets (append snippets '("\n\\end{column}\n\\end{columns}")))
    (auto-jump-generate snippets)))
    

(defun my-latex-fill-nobreak-predicate ()
  "Make sure there are no line-breaks inside \\texttt, \\textit and so on"
  (save-excursion
    (skip-chars-backward " ")
    ;; Don't break after \ since `\ ' has special meaning.
    (or (and (not (bobp)) (memq (char-syntax (char-before)) '(?\\ ?/)))
	(let ((opoint (point))
	      inside)
	  (beginning-of-line)
	  (while (or (re-search-forward "\\\\text..{" opoint t)
		     (re-search-forward "\\\\emph{" opoint t))
	    (unless (re-search-forward "\\}" opoint t)
	      (setq inside t)))
	  inside))))

(defun my-math-fill-nobreak-predicate ()
  "Make sure not to break lines inside math mode"
  (and
   (> (point) 1)
   (eq (get-text-property (1- (point)) 'face) 'tex-math)
   (eq (get-text-property (point) 'face) 'tex-math)))


(require 'cl)

;; Advice compilation-start to not show the compilation buffer.
(setq compile-advice-defined nil)
  
(defun compile-noshow (cmd)
  (if (not compile-advice-defined)
      ;; If we define the advice too early, it will always be in effect for some reason...
      (progn
	(setq compile-advice-defined t)
	(defadvice compilation-start
	  (around inhibit-display
		  (command &optional mode name-function highlight-regexp)) 
	  (if (not (string-match "^\\(find\\|grep\\)" command))
	      (flet ((display-buffer)
		     (set-window-point)
		     (goto-char))
		(fset 'display-buffer 'ignore)
		(fset 'goto-char 'ignore)
		(fset 'set-window-point 'ignore)
		(save-window-excursion 
		  ad-do-it))
	    ad-do-it))))
  (ad-activate 'compilation-start)
  (compile cmd)
  (ad-deactivate 'compilation-start))

(defcustom latex-master nil "Master-file for LaTeX compilation" :safe 'stringp)
(defcustom latex-mode 'xelatex "Mode for LaTeX compilation. Either 'xelatex or 'pdflatex." :safe 'symbolp)

(defun find-latex-master ()
  (interactive)
  (if (eq latex-master 'nil)
      buffer-file-name
    (concat (file-name-directory buffer-file-name)
	    latex-master ".tex")))

(defun latex-command ()
  (cond ((eq latex-mode 'pdflatex) "latexmk -pdf")
	((eq latex-mode 'xelatex)  "latexmk -xelatex")
	(t "latexmk -xelatex")))

(defun compile-latex ()
  (interactive)
  (setq latex-compiling-file (find-latex-master))
  (let* ((dir (file-name-directory latex-compiling-file))
	 (name (file-name-nondirectory latex-compiling-file))
	 (default-directory dir))
    (setq compilation-exit-message-function 'latex-compile-done)
    (compile-noshow (concat (latex-command) " -interaction=nonstopmode " name))))

(defun latex-compile-done (status exit-status exit-msg)
  (if (and (eq status 'exit) (zerop exit-status))
      (let ((buffer (get-file-buffer (replace-ext latex-compiling-file))))
	(if (not (eq buffer 'nil))
	    (let ((old-buffer (current-buffer)))
	      (set-buffer buffer)
	      (doc-view-revert-buffer t t)
	      (set-buffer old-buffer))
	  (message "No PDF buffer opened yet.")))
    (let ((last-alist display-buffer-alist))
      (setq display-buffer-alist 
	    '((".*" . ((display-buffer-reuse-window display-buffer-below-selected) . nil))))
      (display-buffer (get-buffer "*compilation*"))
      (setq display-buffer-alist last-alist)))
  (setq compilation-exit-message-function nil)
  (cons exit-msg exit-status))

(defun replace-ext (name)
  (let ((len (length name)))
    (if (> len 4)
	(if (string= (substring name -4) ".tex")
	    (concat (substring name 0 (- len 4)) ".pdf")
	  name)
      name)))

(setq split-height-threshold 20)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
