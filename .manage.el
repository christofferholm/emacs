;; Manage student hand-ins
;; --------------------------
;;
;; This file implements a set of functions that keeps track of student hand-ins, and is able to
;; select who shall present their solutions. The data is kept in a plain text-file that can be
;; printed, and read anywhere. The file can also be edited manually, if desired (be careful to not
;; ruin the formatting).
;;
;; Usage:
;;
;; First, load the contents of this file into Emacs. Open the file (using C-x C-f), then issue the
;; command M-x eval-buffer <RET> ("eval-buffer" is typed in the mini-buffer). This loads all the
;; code into Emacs, and needs to be done everytime you restart Emacs and want to use these features
;; (unless, you dump the entire file in ~/.emacs, of course).
;;
;; To start using the system, open or create a file where you want to store the results. If it is a
;; new file, you need some setup first. You can either do this manually (configuring headers and
;; dividers as you like), or use the automatic import from Webreg (see below).
;;
;; Manual setup is done as follows. First, create a header using M-x update-header <RET>. The
;; command will ask of the number of exercises. If you put the wrong number, you can always change
;; it later using the same command. To make it easier to differentiate between different sets of
;; problems (you want to keep all sets in the same file so that the script can keep track of who has
;; presented in previous sessions), use M-x add-divider <RET> to add dividers. E.g. M-x add-divider
;; <RET> 8 <RET>. It is also convenient to manually add students to the list. That way you get
;; auto-complete when entering students later on.
;;
;; Automatic setup is done by pasting a list of students in an empty file and running the command
;; M-x create-tdiu11-course. This command simply extracts everything that looks like a LiU-ID from
;; the text and uses those to create a header and dividers for the course TDIU11. This way, you can
;; simply copy-paste from Webreg (either the webpage itself, or from the "course summary" that is
;; available in the top-right corner of Webreg beta (next to the drop-down where you select the
;; year)).
;;
;; When you get student hand-ins, use M-x add-student <RET>. This command will ask for the student's
;; LiU-id, and then a set of exercises the student claims to have solved. These are entered either
;; as 1,2,3,4 etc, or as 1-8, or even combined as 1-3,5-8. This command never removes a solved
;; exercise, so it is possible to add results for the same student in multiple calls to
;; "add-student".
;;
;; You can otherwise prepare a file in the format LiUID:1,2 3-4 5 6 use M-x add-students-from-file <RET>. 
;; This command will ask for the filename and then run the add-student command for every line in the file.
;;
;; Since it is cumbersome to translate the numbers from later sessions to their absolute numbers in
;; the table (e.g. exercise 2 of session 2 is number 10), you can tell the script to do this
;; conversion for you using the command M-x set-offset <RET>. This asks for a number that will be
;; added to all exercise numbers the script ask you for. So, when entering solutions for the second
;; session, it is convenient to first do M-x set-offset <RET> 8 <RET> (the string "(+08)" appears in
;; the header), and then results can be added using the numbering in the exercises. The offset can
;; of course be re-set by doing M-x set-offset <RET> 0 <RET>.
;;
;; When all exercises have been added for a session, you can randomize presenters using M-x do-random <RET>
;; This will ask for the exercises which the script shall select presenters for (this input also
;; respects the offset discussed previously). The format accepted here is the same as when adding
;; results for a student, even though usually you want to enter 1-8. The selected students are
;; marked with a "+" before the "S" indicating that they have solved the exercise. The randomization
;; process will make sure to not pick a student twice in the same session; it prefers not having
;; anyone presenting an exercise over picking the same student twice in the same
;; session. Furthermore, students with the least amount of total presentations so far (even if they
;; are in the "future", ie. higher numbered tasks), are prioritized, but if a student has presented
;; an exercise in a previous session, that student is still eligible for presenting in a latter
;; session.
;;
;; Note that the randomization process does not try to optimize the presentations in any way, it
;; just greedily assigns presenters randomly according to the rules outlined above. This means that
;; when few students have attempted to solve an exercise, it is possible that the script assigns all
;; students who solved that exercise to another exercise (which many solved), meaning that no
;; presenter will be selected for that particular exercise. If this is seen, the "do-random" command
;; can be executed again, which will clear any presenters for the given exercises and do the
;; randomization again in hope for better results. Of course, the assigned presenters can be altered
;; manually if desired. However, remember that the randomization logic is unaware of what we
;; consider to be a "session" (it ignores the dividers), and thus randomizing exercises 1 to 8 one
;; at a time could select the same student for presentation multiple times, while randomizing the
;; same exercises at once ensures that this will not happen.
;;
;; Finally, the command M-x student-stats <RET> can be used to show a summary of the information in
;; the table. This information is shown in a new buffer, that may be saved to file using C-x C-s.

(provide '.manage)

(defun update-header (ex-count)
  (interactive "sNumber of exercises: ")
  (let* ((ex (if (stringp ex-count) (string-to-number ex-count) ex-count))
	 (dividers (find-dividers))
	 (header (cl-reduce (lambda (a b)
			      (if (cl-find b dividers)
				  (format "%s|%02d" a (1+ b))
				(format "%s %02d" a (1+ b))))
			    (cons "LiU-id   |" (iota ex)))))

    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward "[\n\r]")
    (kill-region (point-min) (point))
    (insert header "\n")))

(defun add-divider (offset)
  (interactive "sDivider after: ")

  (let* ((ex (if (stringp offset) (string-to-number offset) offset))
	 (pos (+ (* ex 3) 10)))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((ins (+ (point) pos))
	    (eol (line-end-position)))
	(if (>= ins eol)
	    (progn
	      (end-of-line)
	      (insert (make-string (- ins eol) ? ) "|"))
	  (progn
	    (forward-char pos)
	    (delete-char 1)
	    (insert-char ?|)
	    (end-of-line)))
	(forward-char 1)))))

(defun set-offset (offset)
  (interactive "sExercise offset: ")
  (when (stringp offset)
    (setq offset (string-to-number offset)))

  (remove-offset)
  (unless (= 0 offset)
    (goto-char (point-min))
    (end-of-line)
    (insert (format " (+%02d)" offset))
    (beginning-of-line)))

(defun add-students-from-file (filename)
  "Add or update students from FILENAME.
Format: student123:1,2,3, 5 6 7-8"
  (interactive "fStudents file: ")
  (let ((target-buffer (current-buffer)))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents filename)
        (dolist (raw (split-string (buffer-string) "\n"))
          (let ((line (string-trim raw)))
            (cond
             ;; skip blank lines
             ((string-empty-p line) nil)

             ;; skip comments
             ((string-prefix-p "#" line) nil)

             ;; valid entry
             ((string-match "\\`\\([^:]+\\):\\(.+\\)\\'" line)
              (let ((name   (string-trim (match-string 1 line)))
                    (solved (string-trim (match-string 2 line))))
                (message "Adding student %s → %s" name solved)
                (with-current-buffer target-buffer
                  (add-student name solved))))

             ;; malformed line
             (t
              (message "Skipping malformed line: %s" line)))))))))

(defun add-student (name solved)
  (interactive (list (completing-read "Student id: " (all-student-names))
		     (read-from-minibuffer "Solved: ")))
  (let ((ex-offset  (get-offset))
	(separators (find-dividers)))
    (goto-char (point-min))
    (let ((original
	   (cons
	    name
	    (if (search-forward-regexp (concat "^" name) nil t)
		(let* ((start (progn (beginning-of-line) (point)))
		       (end   (progn (end-of-line) (point)))
		       (str   (buffer-substring start end)))
		  (end-of-line)
		  (skip-chars-forward "[\n\r]")
		  (kill-region start (point))
		  (extract-results str))
	      (let* ((last-line-begin (progn (goto-char (point-max)) (beginning-of-line) (point))))
		(unless (= last-line-begin (point-max))
		  (insert "\n"))
		nil)))))

      (insert-results (append original (parse-solved solved ex-offset)) separators)
      )
    )
  )

(defun create-tdiu11-course (do-it)
  (interactive (list (yes-or-no-p "This will destroy the contents of the current buffer. Proceed? ")))
  (when do-it
    (let ((assignments 40)
	  (dividers '(8 16 24 32))
	  (students (find-all-liu-ids)))
      (erase-buffer)
      (update-header assignments)
      (mapc #'add-divider dividers)

      (mapc (lambda (student)
	      (goto-char (point-max))
	      (message "%S" student)
	      (insert-results
	       (list student)
	       dividers)
	      students)
	    students)

      (goto-char (point-min)))))

(defun find-all-liu-ids ()
  (let ((res '()))
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "[a-z]\\{5,6\\}[0-9]\\{3\\}" nil t)
      (setq res (cons (match-string 0) res))))
  (reverse res)))

(defun do-random (ex)
  (interactive "sWhich exercises: ")
  (let* ((ex-offset  (get-offset))
	 (students   (all-students))
	 (separators (find-dividers))
	 (check      (parse-solved ex ex-offset)))

    ;; Clear old results:
    (setq students (mapcar (lambda (x) (remove-pick check x)) students))
    (dolist (e students)
      (put-results e separators))

    (dolist (e check)
      (let ((here (pick-for-ex students e))
    	    (pick nil))

    	(unless (cl-endp here)
    	  (setq pick (if (cl-endp here) nil (nth (random (length here)) here)))

    	  ;; Update the table.
    	  (put-results pick separators e)
	  
    	  ;; Remove the chosen one!
    	  (setq students (cl-remove-if (lambda (x) (string= (cl-first x) (cl-first pick))) students))
    	  )
    	)
      )
    ))

(defun student-stats ()
  (interactive)

  (let* ((students   (all-students))
	 (separators (find-dividers))
	 (results    (mapcar (lambda (x) (stats-row x separators)) students))
	 (buffer     (get-buffer-create "*student-stats*")))
    
    (with-current-buffer buffer
      (kill-region (point-min) (point-max))

      (insert (stats-header separators) "\n")
      (dolist (x (reverse results))
	(insert (format-stats x) "\n")))

    (display-buffer buffer)))


(defun stats-row (student separators)
  (let* ((name (cl-first student))
	 (presented (cl-count-if 'consp (cl-rest student)))
	 (solved    (length (cl-rest student)))
	 (grade     (compute-grade solved))
	 (ranges    (stats-ranges (cl-rest student) separators)))

    (cons name (cons presented (cons solved (cons grade ranges))))))

(defun compute-grade (solved)
  (if (>= solved 16)
      (format "G:%d" (/ (- solved 16) 4))
    "-"))

(defun stats-ranges (solved separators)
  (if (cl-endp separators)
      (cons (length solved) nil)
    (let* ((limit (cl-first separators))
	   (here (cl-count-if (lambda (x) (<= (get-id x) limit)) solved))
	   (next (cl-remove-if (lambda (x) (<= (get-id x) limit)) solved)))
      (cons here (stats-ranges next (cl-rest separators))))))

(defun stats-header (separators)
  (let ((pairs (cl-maplist (lambda (x) (cons (1+ (cl-first x)) (if (cl-endp (cdr x)) 99 (cl-second x)))) (cons 0 separators))))
    (cl-reduce (lambda (a b) (format "%s %2d-%2d" a (car b) (cdr b)))
	       (cons "LiU-id   |  Demo Total Grade" pairs))))

(defun format-stats (row)
  (cl-reduce (lambda (a b) (format "%s %5s" a b))
	     (cons (format "%-9s|" (cl-first row)) (cl-rest row))))

(defun remove-pick (range student)
  (mapcar (lambda (x)
	    (let ((id (get-id x)))
	      (if (cl-find id range)
		  id
		x)))
	  student))

(defun pick-for-ex (students ex &optional solved)
  (let* ((count (if (null solved) 0 solved))
	 (found (cl-remove-if-not (lambda (x) (should-pick x ex count)) students)))
    (if (and (cl-endp found) (< count 10))
	(pick-for-ex students ex (1+ count))
      found)))

(defun should-pick (student ex max-picked)
  (if (> (length (cl-remove-if-not 'consp (cl-rest student))) max-picked)
      nil
    (has-ex (cl-rest student) ex)))

(defun has-ex (student ex)
  (cond ((cl-endp student) nil)
	((= (get-id (cl-first student)) ex) t)
	(t (has-ex (cl-rest student) ex))))

(defun str-remove-until (str until)
  (let ((found (cl-search until str)))
    (if found
	(substring str (1+ found))
      str)))

(defun extract-results (str)
  (let* ((str (str-remove-until str "|"))
	 (at  0)
	 (len (length str))
	 (result '()))
    (while (< at len)
      (let* ((to (+ 3 at))
	     (id (1+ (/ at 3)))
	     (checked (string-has str at to ?S))
	     (chosen  (string-has str at to ?+)))
	(setq at to)
	(cond ((and checked chosen)
	       (setq result (cons (cons 'pick id) result)))
	      (checked
	       (setq result (cons id result))))))
    result))

(defun string-has (string from to ch)
  (if (< from to)
      (if (and (< from (length string))
	       (= ch (string-to-char (substring string from))))
	  t
	(string-has string (1+ from) to ch))
    nil))

(defun all-student-names ()
  "Get a list of all student names"
  (save-excursion
    (mapcar #'cl-first (all-students))))

(defun all-students ()
  "Get a list, each element describing a student's results."
  (goto-char (point-min))
  (next-line)
  (let ((result '()))
    (while (search-forward-regexp "^\\([^ ]+\\) *|[|Ss+* ]*$" nil t)
      (setq result (cons
		    (cons (match-string 1) (extract-results (match-string 0)))
		    result)))

    result))

(defun put-results (results separators &optional special)
  (let* ((student (cl-first results)))
    (goto-char (point-min))
    (if (search-forward-regexp (concat "^" student) nil t)
	(let* ((start (progn (beginning-of-line) (point))))
	  (end-of-line)
	  (skip-chars-forward "[\n\r]")
	  (kill-region start (point)))
      (let* ((last-line-begin (progn (goto-char (point-max)) (beginning-of-line) (point))))
	(unless (= last-line-begin (point-max))
	  (insert "\n"))
	nil))

    (insert-results results separators special)))


(defun insert-results (results separators &optional special)
  (let* ((student (cl-first results))
	 (ex      (cl-rest results))
	 (max     (max (cl-reduce 'max-result (cons 0 ex)) ;; Adding a zero to not crash when ex is empty
		       (if (cl-endp separators)
			   0
			 (1+ (cl-reduce 'max separators)))))
	 (start   (progn
		    (insert (format "%-9s|" student))
		    (point))))

    (insert (make-string (* 3 max) ? ) "\n")
    (dolist (e ex)
      (let ((id (if (consp e) (cdr e) e))
	    (check (consp e)))
	(goto-char (+ start (* 3 id) -1))
	(delete-char 1)
	(insert-char ?S)

	(when check
	  (goto-char (+ start (* 3 id) -2))
	  (delete-char 1)
	  (insert-char ?+))))
    (dolist (e separators)
      (goto-char (+ start (* 3 e)))
      (delete-char 1)
      (insert-char ?|))
    (when special
      (goto-char (+ start (* 3 special) -2))
      (delete-char 1)
      (insert-char ?+))
    (beginning-of-line)))

(defun find-dividers ()
  (goto-char (point-min))
  (end-of-line)
  (let ((pos (point))
	(result '()))
    (goto-char (point-min))
    (while (>= pos (+ 10 (point-min)))
      (when (string= (buffer-substring pos (1+ pos)) "|")
	(setq result
	      (cons (/ (- pos (point-min) 10) 3)
		    result)))

      (setq pos (1- pos)))
    result))

(defun get-offset ()
  (goto-char (point-min))
  (end-of-line)
  (prog1
      (if (search-backward-regexp "(\\+\\([0-9]+\\)) *$" nil t)
	  (string-to-number (match-string 1))
	0)
    (beginning-of-line)))

(defun remove-offset ()
  (goto-char (point-min))
  (end-of-line)
  (let ((end (point)))
    (when (search-backward-regexp "[^ ] *(\\+[0-9]+) *$" nil t)
      (forward-char 1)
      (delete-char (- end (point)))))
  (beginning-of-line))

(defun max-result (a b)
  (max (get-id a) (get-id b)))

(defun get-id (x)
  (if (consp x)
      (cdr x)
    x))

(defun parse-solved (solved &optional offset)
  (unless offset
    (setq offset 0))

  (setq solved
	(replace-regexp-in-string "[ \t\n]" "" solved))

  (cl-reduce (lambda (a b)
	       (append a (parse-solved-part b offset)))
	     (cons '() (split-string solved "[ ,]"))))

(defun parse-solved-part (part offset)
  (if (string-equal part "")
      '()
    (let ((r (mapcar 'string-to-number (split-string part "-"))))
      (if (> (length r) 1)
	  (iota-range (+ (cl-first r) offset) (+ 1 (cl-second r) offset))
	(cons
	 (+ (cl-first r) offset)
	 'nil)))))

(defun iota (count)
  (iota-help 0 count))

(defun iota-range (start end)
  (iota-help start end))

(defun iota-help (now count)
  (if (= now count)
      nil
    (cons now (iota-help (1+ now) count))))
