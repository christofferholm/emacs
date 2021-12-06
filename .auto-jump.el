(provide '.auto-jump)

(define-minor-mode auto-jump-mode
  "Toggle Auto Jump Mode.
Should not be activated manually, is used by the Auto Jump features."
  ;; initial value
  nil
  ;; Indicator for the mode line
  " Auto Jump"
  '())

(define-key auto-jump-mode-map (kbd "TAB") 'auto-jump-goto-next)
(define-key auto-jump-mode-map (kbd "C-g") 'auto-jump-abort)

(defvar-local auto-jump-points '()
  "List containing the queue of available jump points.
Will only contain the current skeletons auto-jumps.")

(defvar-local auto-jump-deduction-table '()
  "Alist containing keywords used for deducing a skeleton.
These keywords will be used whenever the user calls `auto-jump-deduce`,
the skeleton defined by corresponding keyword will be inserted using
auto-jump generate.")

(defun auto-jump-init (&optional deductions)
  "Initialize the deduction table in the current buffer.
deductions must be a list of key value pairs."
  (if deductions
      (setq-local auto-jump-deduction-table deductions))
  (setq-local auto-jump-points '()))

(defun auto-jump-deduce ()
  "Go back one word and lookup said word in the deduction table.
If it exists, generate corresponding skeleton."
  (interactive)
  ;; result will contain the potential skeleton
  (let ((result '()))
    (save-restriction
      ;; restrict our search to the current line
      (let* ((home (point))
             (line-start (progn (beginning-of-line) (point)))
             (line-end   (progn (end-of-line) (point))))
        (narrow-to-region line-start line-end)
        (goto-char home))
      (save-excursion
        (let* ((start (progn (backward-word) (point)))
               (end   (progn (forward-word)  (point)))
               ;; extract the deduction word
               (key   (buffer-substring-no-properties start end))
               ;; lookup the deduction skeleton
               (entry (assoc-string key auto-jump-deduction-table)))
          ;; store the skeleton in 'result'
          (setq result (if entry 
                           (progn (delete-region start end)
                                  (cdr entry)))))))
    ;; did we find a skeleton? If so, generate it
    (if result
        (auto-jump-generate result))))

(defun auto-jump-abort ()
  "Will exit auto-jump-mode and remove all auto jump points."
  (interactive)
  (setq auto-jump-points '())
  (auto-jump-mode 0))

(defun auto-jump-goto-next ()
  "Goto the next auto jump point.
If there are more auto jump points in the queue the cursor will jump there.
If the queue is empty, this function will disable auto-jump-mode."
  (interactive)
  ;; get next point
  (let ((target (auto-jump-dequeue)))
    ;; if there was a point, jump to it
    ;; if it was the last point, abort
    (if target
        (progn (goto-char target)
               (unless auto-jump-points
                 (auto-jump-abort)))
      (auto-jump-abort))))

(defun auto-jump-generate (snippets &optional values)
    "Generate a skeleton with jump points inside.
`snippets` must be a list of strings, where each string will be inserted into the buffer, 
with auto jump points in-between. Each non-nil element in the values list will be inserted 
instead of jump points at corresponding jump point in the skeleton."
  (interactive)
  (let ((old-points auto-jump-points))
    ;; remove any previous auto jumps
    (auto-jump-abort)
    ;; go through each snippet
    (dolist (snippet snippets)
      ;; split the snippet into individual lines
      ;; insert and indent all lines individually
      (let ((lines (split-string snippet "\n")))
        (dolist (str (butlast lines 1))
          (insert str)
          (indent-for-tab-command)
          (insert "\n"))
        (insert (car (last lines 1)))
        (indent-for-tab-command))
      ;; if there is a corresponding value insert it
      ;; otherwise add a jump-point
      (if values
          (let ((value (car values)))
            (if value (insert value)) ;; only insert value if it's set
            (setq values (cdr values)))
        (auto-jump-enqueue)))
    (setq auto-jump-points (append (butlast auto-jump-points 1) old-points)))
  ;; if there where any jump points added then
  ;; enable auto-jump-mode
  (if auto-jump-points
      (progn (auto-jump-mode 1)
             (goto-char (auto-jump-dequeue)))))

(defun auto-jump-enqueue ()
  "Add a jump point to the queue."
  (add-to-list 'auto-jump-points (point-marker) t))
(defun auto-jump-dequeue ()
  "Remove a jump point from the queue, and return it."
  (let* ((element (if auto-jump-points 
                      (car auto-jump-points)))
         (position (if element 
                       (marker-position element))))
    (setq auto-jump-points 
          (if auto-jump-points 
              (cdr auto-jump-points)))
    (if element 
        (set-marker element nil))
    position))
