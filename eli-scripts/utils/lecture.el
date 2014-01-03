;; -*- Emacs-Lisp -*-

;; a popup window is fit to the height of the text, but never outside
;; these values times the frame height
(defvar min-popup-height 0.3)
(defvar max-popup-height 0.9)
;; if a number, increase font size as long as the text fits, and up to this
;; many steps
(defvar resize-popup-font 24)
;; and each step is a scale of this factor
(setq text-scale-mode-step 1.1)

(setq display-time-use-mail-icon nil)
(setq display-time-mail-string "")
(display-time-mode 1)
(set-scroll-bar-mode nil)
(set-default-font "Consolas 19")
(simple-make-face 'yellow/red2-uninverse-scale50 'mode-line)

(let ((f (selected-frame)))
  (set-frame-position f 0 0)
  (set-frame-size     f 72 27)
  (frame-parameter f 'outer-window-id)
  (call-process "/usr/bin/sawfish-client" nil 0 nil "-e"
                (let* ((w (frame-parameter f 'outer-window-id))
                       (w (string-to-number w))
                       (cmd `(let ((w (get-window-by-id ,w)))
                               ;(call-command 'set-frame:unframed w)
                               (move-window-to w -2 -19))))
                  (format "%S" cmd))))

;; echo area
(setq message-truncate-lines t)
;; (it would be nice to hide it or make the font tiny there)

(defun do-lecture-highlights ()
  (add-color-pattern "^=\\{72\\}\n" '*/blue4)
  (add-color-pattern "^ *>>>.*\n" 'yellow/red4-bold 0 t)
  (add-color-pattern "^ *>> .*\n" 'yellow/h500 0 t)
  (add-color-pattern "^ *<<<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]>>> *\n"
                     'yellow/purple4-bold 0 t)
  (add-color-pattern "^ *\\(\\[\\[\\[\\)\\(.*\\)\\(\\]\\]\\]\\) *$"
                     'yellow/h404-bold 2 nil 'gray25/* 1 nil 'gray25/* 3 nil))

(defvar focus-prev-wconf nil)
(make-variable-buffer-local 'focus-prev-wconf)
(defvar focus-update-data nil)
(make-variable-buffer-local 'focus-update-data)

(defun guess-focus-region ()
  (save-excursion
    (save-restriction
      (forward-line 0)
      ;; climb up current s-expression, if any
      (condition-case nil (while (looking-at "^ ") (up-list 1)
                                 (backward-sexp) (forward-line 0))
        (error nil))
      (unless (looking-at "^\n*\\( +\\)") (error "Cannot guess focus"))
      ;; the region includes lines with this indentation and empty
      ;; lines, but ignore a prefix/suffix of empty lines
      (let ((re (concat "^\\(?:" (match-string-no-properties 1) "\\| *$\\)"))
            b)
        ;; find beginning of focus region
        (while (and (looking-at re) (not (bobp))) (forward-line -1))
        (unless (bobp) (forward-line 1))
        ;; skip empty-line prefix and beginning-of-file lines
        (while (looking-at " *\\(--+\\(<<<[^\n>]+>>>\\)?--+ *\\)?$")
          (forward-line 1))
        (setq b (point))
        ;; find end of focus region
        (while (and (looking-at re) (not (eobp))) (forward-line 1))
        ;; skip (back) empty lines and end-of-file lines
        (forward-line -1)
        (while (looking-at " *\\(---+ *\\)?$") (forward-line -1))
        (forward-line 1)
        (list b (point))))))

(defun get-focus-region (maybe-focused-text)
  (let* ((r (cond (mark-active
                   (list (region-beginning) (region-end)))
                  ((and maybe-focused-text focus-prev-wconf)
                   (list (point-min) (point-max)))
                  (t (guess-focus-region))))
         (b (car r))
         (e (cadr r)))
    (when (or (not b) (not e) (and (= b (point-min)) (= e (point-max))))
      (error "Cannot guess focus"))
    (save-excursion
      (goto-char b) (skip-chars-forward  "\n " e) (setq b (point))
      (goto-char e) (skip-chars-backward "\n " b) (setq e (point))
      (when (equal (char-after) ?\n) (setq e (1+ e))))
    (when (= b e) (error "Empty focus"))
    (list b e)))

(defun get-focus-text (&rest flags)
  (let* ((r (get-focus-region (memq 'maybe-focused-text flags)))
         (b (car r))
         (e (cadr r))
         (text   (buffer-substring-no-properties b e))
         (text0  nil)
         (lines  (split-string text "\n"))
         (line1  (car lines))
         (lines  (cdr lines))
         (indent nil)
         (pos    nil))
    (when lines
      (setq indent (let ((indents (remq nil (mapcar (lambda (l)
                                                      (string-match "[^ ]" l))
                                                    lines))))
                     (and (consp indents) (apply 'min indents))))
      (let ((indent1 (save-excursion (goto-char b) (current-column))))
        (cond ((or (not indent) (= indent indent1)) nil)
              ((> indent indent1) (setq indent indent1))
              (t (let ((b1 (- b (- indent1 indent))))
                   (setq text0 (buffer-substring-no-properties b1 b))
                   (setq b b1)))))
      (when (equal 0 indent) (setq indent nil))
      (when indent
        (setq text (concat line1 "\n" (mapconcat (lambda (l)
                                                   (if (<= indent (length l))
                                                     (substring l indent) l))
                                                 lines "\n"))))
      (when text0
        (setq text (concat text0 text))))
    (if (memq 'include-update-and-pos flags)
      (list text (list b e text (current-buffer) indent)
            (save-restriction (save-excursion
                                (narrow-to-region b e)
                                (let ((line (line-number-at-pos))
                                      (col  (current-column)))
                                  (cons line (- col (or indent 0)))))))
      text)))

(defun update-orig-text (b e oldtext oldbuf indent)
  (save-excursion
    (let ((newtext (buffer-substring-no-properties (point-min) (point-max)))
          (newbuf  (current-buffer)))
      (set-buffer oldbuf)
      (unless (equal newtext oldtext)
        (save-excursion
          (goto-char b)
          (delete-region b e)
          (when indent
            (setq newtext (replace-regexp-in-string
                           "\n\\(.\\)"
                           (concat "\n" (make-string indent 32)"\\1")
                           newtext)))
          (insert newtext)
          (goto-char b)))
      (save-buffer)
      (set-buffer newbuf)
      (set-buffer-modified-p nil)
      t)))

(defun lecture-focus ()
  (interactive)
  (let* ((text+  (get-focus-text 'include-update-and-pos))
         (text   (nth 0 text+))
         (update (nth 1 text+))
         (pos    (nth 2 text+))
         (oldbuf (current-buffer))
         (newbuf nil)
         (wconf  (current-window-configuration)))
    (let ((count 1))
      (while (get-buffer (format "%s:%s" oldbuf count))
        (setq count (1+ count)))
      (setq newbuf (get-buffer-create (format "%s:%s" oldbuf count))))
    (switch-to-buffer newbuf)
    (insert text)
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (goto-char (point-min))
    (when (looking-at "\\`[ \t\r\n\f]*\\([[(;]\\|#lang\\)")
      (scheme-mode))
    (setq focus-prev-wconf wconf) ; *after* scheme-mode (resets locals)
    (setq focus-update-data update)
    (setq require-final-newline nil)
    (delete-other-windows)
    (let ((win1 (selected-window)) (win2 (split-window)))
      (select-window win1)
      (switch-to-buffer (get-buffer-create "*filler*"))
      (setq mode-line-format "-%-"
            show-trailing-whitespace   nil
            indicate-empty-lines       nil
            indicate-buffer-boundaries nil
            truncate-lines             t)
      (erase-buffer)
      (select-window win2))
    (do-lecture-highlights)
    (setq truncate-lines nil)
    (redisplay t)
    (when resize-popup-font
      (let ((fw  (min (frame-pixel-width) (display-pixel-width)))
            (fh  (frame-height))
            (fch (frame-char-height))
            (maxlinepos   0)
            (maxlineend   0)
            (maxlinewidth 0)
            (maxliney     0)
            (N (1+ resize-popup-font))) ; we decrease a step in the end
        (goto-char (point-min))
        (let (curlinepos)
          (while (not (eobp))
            (setq curlinepos (point))
            (forward-line 1)
            (when (> (- (point) curlinepos) maxlinewidth)
              (setq maxlinepos curlinepos
                    maxlinewidth (- (point) curlinepos)))))
        (goto-char maxlinepos)
        (redisplay t)
        (setq maxlineend (+ maxlinepos maxlinewidth -1)
              maxliney   (cdr (posn-x-y (posn-at-point maxlinepos))))
        (text-scale-increase 0)
        (while (and
                (<= 0 (setq N (1- N)))
                (let* ((xy (posn-x-y (posn-at-point maxlineend)))
                       (x (car xy))
                       (y (cdr xy)))
                  (and xy
                       (<= (- y (1- (round (* fch (expt text-scale-mode-step
                                                        text-scale-mode-amount)
                                              0.5))))
                           maxliney)
                       (< x fw))))
          (text-scale-increase 1))
        (text-scale-increase -1)
        (goto-char (point-min))
        (fit-window-to-buffer nil
                              (round (* fh max-popup-height))
                              (round (* fh min-popup-height)))
        (let ((scale text-scale-mode-amount))
          (with-current-buffer "*filler*"
            (text-scale-increase 0)
            (text-scale-increase scale)))))
    (goto-char (point-min))
    (setq mark-active nil)
    (setq truncate-lines t)
    (redisplay t)
    (forward-line (1- (car pos)))
    (goto-char (+ (cdr pos) (point)))
    ;; hooks
    (add-hook 'kill-buffer-hook 'restore-pre-focus-wconf nil t)
    (setq buffer-file-name (buffer-name oldbuf))
    (add-hook 'local-write-file-hooks 'propagate-text-changes nil t)))

(defun restore-pre-focus-wconf ()
  (let ((wconf   focus-prev-wconf))
    (when wconf
      (set-window-configuration focus-prev-wconf)
      (condition-case nil (bury-buffer "*filler*") (error nil)))))
(defun propagate-text-changes ()
  (and focus-update-data (apply 'update-orig-text focus-update-data)))

(defun lecture-send-to-drracket ()
  (interactive)
  (let* ((str (get-focus-text 'maybe-focused-text))
         (str (if (string-match "#lang" str)
                str (concat "#lang pl\n\n" str)))
         (f (let ((count 1))
              (while (file-exists-p (format "/tmp/xpl-%s.rkt" count))
                (setq count (1+ count)))
              (format "/tmp/xpl-%s.rkt" count))))
    (write-region str nil f nil 'silent nil 'excl)
    (call-process (let ((h (getenv "PLTHOME")))
                    (if h (concat h "/bin/drracket") "drracket"))
                  nil nil nil "-singleInstance" f)))

(do-lecture-highlights)
(global-set-key [(meta f1)] 'lecture-focus)
(global-set-key [(meta f2)] 'lecture-send-to-drracket)

(run-with-timer 0.05 nil (lambda () (re-search-forward "\f") (recenter)))

;; (error "Done.") ; to work with eval-buffer
