;; Mo's MicroSnippet

;; GPLv2 or later
;; (c) 2019 Moritz Molle

;;;;;;;;;;;;; Insert this into your .emacs

;;;;;;;; mo microsnippets
;; (load "~/.emacs.d/mo_microsnippet.el")  ;; to load this file (adjust the path if needed)
;; (global-set-key (kbd "C-= w") 'mms-copy-region)     ;; I like C-= as prefix key
;; (global-set-key (kbd "C-= y") 'mms-insert-snippet)
;;;;;;;;;;;;;;;;;;;;

;; create simple snippets
;; @(...)-parts, you can jump to with [TAB]
;; @[012]-parts will automatically counted up
;; you can use $() or %() or whatever by changing "mms-jump-marker"


(defcustom mms-jump-marker "@"
  "This is the mms jump marker with which to mark fields in mms-snippets")
(defvar mms-increment-counter 0
  "the mms increment count. Gets zeroed on mms-copy-region")
(defvar mms-the-snippet ""
  "this is the mms snippet clipboard")

;; stolen helper-functions
;; source: https://www.emacswiki.org/emacs/IncrementNumber
(defun mms--increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun mms--count-marker ()
  (let ((count-marker (concat mms-jump-marker "["))
        (tmp mms-the-snippet)
        (count 0))
    (while (> (length tmp) (length count-marker))
      (if (string-prefix-p count-marker tmp)
          (setq count (+ 1 count)))
      (setq tmp (substring tmp 1 nil)))
    count))

(defun mms-copy-region (p beg end)
  "This function copies the region into the mms snippet clipboard (variable mms-the-snippet)
If called with prefix argument asks for a new jump marker
With '@' as jump-marker, @[123]-like fields will automatically incremented by mms-insert-snippet and
@(foo) are fields which have to be filled in when pasting the snippet"
  (interactive "P\nr")
  (if p
      (setq mms-jump-marker (read-from-minibuffer "Set new jumpmarker: " mms-jump-marker)))
  (setq mms-the-snippet (buffer-substring-no-properties beg end))
  (deactivate-mark)
  (setq mms-increment-counter 0))

(defun mms-insert-snippet (p)
  (interactive "p")
  (dotimes (n p nil)
    (let ((count-marker (concat mms-jump-marker "["))
          (edit-marker (concat mms-jump-marker "("))
          (p (point))
          (end-marker nil)
          (field-name))
      (insert mms-the-snippet)
      (setq end-marker (point-marker))
      
      ;; incrementing numbers
      (goto-char p)
      (dotimes (number (mms--count-marker) nil)
	(search-forward count-marker)
	(backward-delete-char (length count-marker))
	(mms--increment-number-decimal mms-increment-counter)
	(search-forward "]")
	(backward-delete-char 1))
      (setq mms-increment-counter (+ 1 mms-increment-counter))
      
      ;; editing edit-fields
      (goto-char p)
      (while (and (< (point) end-marker)
                  (search-forward edit-marker end-marker 'not-nil-and-not-t))
	(backward-delete-char (length edit-marker))
	(save-mark-and-excursion
	  (set-mark (point))
	  (search-forward ")")
	  (setq field-name (buffer-substring-no-properties (mark) (- (point) 1)))
	  (delete-region (mark) (point)))
	
	(insert (read-from-minibuffer "Field: " field-name))))))
