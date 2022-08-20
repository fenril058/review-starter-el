(require 'font-lock)

(defvar review-starter-minimum-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." st)    ; "
    (modify-syntax-entry ?\\ "/" st)    ; \
    (modify-syntax-entry ?\r ">" st)    ; \r
    (modify-syntax-entry ?\n ">" st)    ; \n
    (modify-syntax-entry ?\f ">" st)    ; ^L
    (modify-syntax-entry ?\C-@ "w" st)  ; ^@
    (modify-syntax-entry ?@ "_" st)     ; @
    (modify-syntax-entry ?* "_" st)     ; *
    (modify-syntax-entry ?\t " " st)    ; Tab
    ;; (modify-syntax-entry ?$ "$$" st)    ; $ (TeX like)
    (modify-syntax-entry ?& "." st)     ; &
    (modify-syntax-entry ?_ "." st)     ; _
    (modify-syntax-entry ?^ "." st)     ; ^
    st)
  "Syntax table used while in Re:VIEW starter mode.")

(defvar review-starter-minimum--syntax-properties
  (list 'review-starter-list-item         nil
        'review-starter-block-begin       nil
        'review-starter-block             nil
        'review-starter-block-end         nil
        'review-starter-code-block-begin  nil
        'review-starter-code-block        nil
        'review-starter-code-block-end    nil
        'review-starter-quote-block-begin nil
        'review-starter-quote-block      nil
        'review-starter-quote-block-end  nil
        'review-starter-heading          nil
        'review-starter-heading-1        nil
        'review-starter-heading-2        nil
        'review-starter-heading-3        nil
        'review-starter-heading-4        nil
        'review-starter-heading-5        nil
        )
  "Property list of all Re:VIEW Starter syntactic properties.")

(defun review-starter-minimum-remove-text-properties (start end)
  (remove-text-properties start end review-starter-minimum--syntax-properties))

(eval-and-compile
  (defvar review-minimum-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     ("\\(^#@#\\)" (1 "<"))
     ("\\(^//.+{$\\)\\(\\(?:.\\|\n\\)*?\\)\\(^//}$\\)" ; match any block
      (1 (ignore
          (let ((begin (match-beginning 1))
                 (end (match-end 1)))
             (review-starter-minimum-remove-text-properties begin end)
             (put-text-property begin end
                                'review-starter-block-begin
                                (list begin end)))))
      (2 (ignore
          (let ((begin (1+ (match-beginning 2)))
                 (end (match-end 2)))
             (review-starter-minimum-remove-text-properties begin end)
             (put-text-property begin end
                                'review-starter-block
                                (list begin end)))))
      (3 (ignore
          (let ((begin (1+ (match-beginning 3)))
                 (end (match-end 3)))
             (review-starter-minimum-remove-text-properties begin end)
             (put-text-property begin end
                                'review-starter-block-end
                                (list begin end))))))
     ;; ("\\(^//\\(?:em\\)?list\\(?:num\\)?{$\\)\\(\\(?:.\\|\n\\)+?\\)\\(^//}$\\)"
     ;;  (1 (prog1 "_"
     ;;       (let ((begin (match-beginning 1))
     ;;             (end (match-end 1)))
     ;;         (review-starter-minimum-remove-text-properties begin end)
     ;;         (put-text-property begin end
     ;;                            'review-starter-code-block-begin
     ;;                            (list begin end)))))
     ;;  (2 (ignore
     ;;      (let ((begin (match-beginning 2))
     ;;            (end (match-end 2)))
     ;;        (review-starter-minimum-remove-text-properties begin end)
     ;;        (put-text-property begin end
     ;;                           'review-starter-code-block
     ;;                           (list begin end)))))
     ;;  (3 (prog1 "_"
     ;;       (let ((begin (match-beginning 3))
     ;;             (end (match-end 3)))
     ;;         (review-starter-minimum-remove-text-properties begin end)
     ;;         (put-text-property begin end
     ;;                            'review-starter-code-block-end
     ;;                            (list begin end)))))
     ;;  )
     ;; ("\\(^//quote{$\\)\\(\\(?:.\\|\n\\)*?\\)\\(^//}$\\)"
     ;;  (1 (prog1 "_"
     ;;       (let ((begin (match-beginning 1))
     ;;             (end (match-end 1)))
     ;;         (review-starter-minimum-remove-text-properties begin end)
     ;;         (put-text-property begin end
     ;;                            'review-starter-quote-block-begin
     ;;                            (list begin end)))))
     ;;  (2 (ignore
     ;;      (let ((begin (match-beginning 2))
     ;;            (end (match-end 2)))
     ;;        (review-starter-minimum-remove-text-properties begin end)
     ;;        (put-text-property begin end
     ;;                           'review-starter-quote-block
     ;;                           (list begin end)))))
     ;;  (3 (prog1 "_"
     ;;       (let ((begin (match-beginning 3))
     ;;             (end (match-end 3)))
     ;;         (review-starter-minimum-remove-text-properties begin end)
     ;;         (put-text-property begin end
     ;;                            'review-starter-quote-end
     ;;                            (list begin end)))))
     ;;  )
     )
    "Syntax-propertize rules Re:VIEW.
These have to be run via `review-starter-mode-syntax-propertize'")

  (defvar review-minimum-syntax-propertize-rules+
    (syntax-propertize-precompile-rules
     review-minimum-syntax-propertize-rules
     ;; Use the `b' style of comments to avoid interference with the
     ;; #@+++ ... #--- comments recognized
     ("\\(^#@\\+\\+\\+\\)" (1 "< b"))
     ("\\(^#@---\\)" (1 "> b"))
     )
    "Syntax-propertize rules Re:VIEW Starter.
These have to be run via `review-starter-mode-syntax-propertize'")
  )

(defun review-minimum-match-propertized-text-old (property last)
  "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
  (let ((saved (get-text-property (point) property))
        pos)
    (unless saved
      (setq pos (next-single-property-change (point) property nil last))
      (unless (= pos last)
        (setq saved (get-text-property pos property))))
    (when saved
      (set-match-data saved)
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      saved)))

(defun review-minimum-match-propertized-text (property last)
  "Match text with PROPERTY from point to LAST."
  (let* ((beg (next-single-property-change (point) property nil last))
         (end (next-single-property-change beg property nil last)))
    (unless (= beg end)
      (set-match-data (list beg end))
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max))))
    ))

(defun review-minimum-match-block-begin (last)
  "Match blockquotes from point to LAST.
Use data stored in \\='markdown-blockquote text property during syntax
analysis."
  (review-minimum-match-propertized-text 'review-starter-block-begin last)
  )

(defun review-minimum-match-block-end (last)
  "Match blockquotes from point to LAST.
Use data stored in \\='markdown-blockquote text property during syntax
analysis."
  (review-minimum-match-propertized-text 'review-starter-block-end last)
  )

(defun review-minimum-fontify-block-begin (last)
  "Apply font-lock properties to blockquotes from point to LAST."
  (when (review-minimum-match-block-begin last)
    (font-lock-append-text-property
     (match-beginning 0) (match-end 0) 'face font-lock-type-face)
    t))

(defun review-minimum-fontify-block-begin (last)
  "Apply font-lock properties to blockquotes from point to LAST."
  (when (review-minimum-match-block-end last)
    (font-lock-append-text-property
     (match-beginning 0) (match-end 0) 'face font-lock-function-name-face)
    t))

(defun review-minimum-comment-p ()
  "Print \"t\" in minibuffer, if cursor position is inside a comment.
Otherwise Print nil."
  (interactive)
  (message "%s" (review-starter-in-comment-p)))

(defvar review-minimum-font-lock-keywords nil)
(setq review-minimum-font-lock-keywords
      '(("\\(@<B>{\\)\\(.*?[^\\]\\)\\(}\\)"
         (1 font-lock-function-name-face t)
         (2 'review-starter-bold-face append))
        (review-minimum-match-block-begin . ((0 font-lock-type-face)))
        (review-minimum-match-block-end . ((0 font-lock-function-name-face)))
        ;; font-lock defaultのkeyword-onlyがnilでも
        ;; これがないと#@---の #
        ;; までしか faceがつかない.
        ("^#@---"
         (0 font-lock-comment-delimiter-face t))
        ))

(define-derived-mode review-starter-minimum-mode text-mode "MIN:Re:VIEW"
  "Major mode for editing Re:VIEW Starter."
  (setq-local comment-start "#@# ")

  ;; Syntax
  (setq-local syntax-propertize-function (syntax-propertize-rules
                                          review-minimum-syntax-propertize-rules+))

  (setq-local font-lock-defaults
              '((review-minimum-font-lock-keywords) ; KEYWORDS
    	        nil nil nil nil
                (font-lock-multiline . t)
    	        ;; (font-lock-mark-block-function . mark-paragraph) ; default of text-mode?
                ;; (font-lock-syntactic-keywords . nil)
                ))
  (define-key review-starter-minimum-mode-map (kbd "C-c C-v") 'review-minimum-comment-p)
  (define-key review-starter-minimum-mode-map (kbd "C-c C-p") 'describe-text-properties)
  (run-hooks 'review-starter-minimu-mode-hook))

(provide 'review-starter-minimum)
