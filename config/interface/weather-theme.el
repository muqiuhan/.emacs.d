(defconst *weather-theme-file* "~/.weather-theme")

(shell-command (concat "curl wttr.in>" *weather-theme-file*))

(setq weather "")

(let* ((weather-string (funcall
			(lambda ()
			  (string-to-list
			   (nth 2 (split-string
				   (with-temp-buffer
				     (insert-file-contents *weather-theme-file*)
				     (buffer-string))
				   "\n"))))))
       (weather-string-length (1- (length weather-string)))
       (concat-flag t))

  (while (> weather-string-length 0)
    (if (and
	 (char-equal (nth weather-string-length weather-string) (string-to-char " "))
	 (char-equal (nth (- weather-string-length 1) weather-string) (string-to-char "m"))
	 (char-equal (nth (- weather-string-length 2) weather-string) (string-to-char "0")))
	(setq concat-flag nil)
      (when concat-flag
	(setq weather (concat weather (char-to-string (nth weather-string-length weather-string))))))
    (setq weather-string-length (1- weather-string-length))))
