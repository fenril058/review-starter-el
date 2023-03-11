;;; review-starter-mode.el --- major mode for Re:VIEW & Re:VIEW Starter -*- lexical-binding: t -*-

;; Copyright 2022 ril <fenri.nh@gmail.com>

;; Author: ril <fenri.nh@gmail.com>
;; Keywords: Re:VIEW, ReVIEW Starter
;; URL: https://github.com/fenril058/review-starter-el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; "Re:VIEW" & "Re:View starter" text editing mode.
;;
;; This package support Re:VIEW Starter expnasion, if
;; `review-starter-use-expansion' is non-nil.
;; Not fully but almost compatible with review-mode.el
;; (https://github.com/kmuto/review-el/).
;;
;; See the README.md file for details.

;;; Code:

(require 'outline)
(require 'imenu)
(require 'font-lock)
(require 'compile)
(require 'review-starter-face)


;;;
;;; Global Variables
;;;
(defgroup review-starter nil
  "Major mode for editing text files in Re:VIEW Starter format."
  :prefix "review-starter-"
  :group 'text)

(defconst review-starter-mode-version "0.2.1"
  "Re:VIEW Starter mode version number.")

(defvar review-starter-default-role-alist
  '(("編集者" . "編集注")
    ("翻訳者" . "翻訳注")
    ("監訳"   . "監注")
    ("著者"   . "注")
    ("DTP"    . "注")
    (review-starter-user-name . "注"))
  "役割の名前と注のリスト.")

(defvar review-starter-dtp-name nil
  "現在のDTP.")

(defvar review-starter-standard-block-names
  '("bibpaper[][]" "caution"      "cmd"       "embed[]"
    "emlist"       "emlistnum"    "emtable[]" "graph[][][]"
    "image[][]"    "imgtable[][]" "important" "indepimage[]"
    "info"         "lead"         "list[][]"  "listnum[][]"
    "memo"         "note"         "notice"    "numberlessimage[]"
    "quote"        "read"         "source"    "table[][]"
    "texequation"  "tip"          "warning")
  "Standard Re:VIEW block names.")

(defvar review-starter-standard-single-line-block-names
  '("beginchild"      "blankline"     "endchild"  "endnote[][]"
    "firstlinenum[]"  "footnote[][]"  "noindent"  "printendnotes"
    "raw"             "tsize[]")
  "Standard Re:VIEW block names.")

(defvar review-starter-standard-block-names+
  '("abstract" "terminal")
  "Block names added by Re:VIEW Starter.")

(defvar review-starter-standard-single-line-block-names+
  '("vspace" "needvspace" "clearpage")
  "Single line BLock  names added by Re:VIEW Starter.")

(defvar review-starter-standard-inline-names
  '("ami"     "b"     "balloon" "bib"
    "bou"     "br"    "chap"    "chapref"
    "chapter" "code"  "column"  "comment"
    "em"      "embed" "endnote" "eq"
    "fn"      "hd"    "hidx"    "href"
    "i"       "icon"  "idx"     "img"
    "kw"      "list"  "m"       "raw"
    "ruby"    "sec"   "secref"  "sectitle"
    "strong"  "table" "tcy"     "title"
    "tt"      "ttb"   "tti"     "u"
    "uchar"   "w"     "wb")
  "Standard Re:VIEW inline command names.")

(defvar review-starter-standard-inline-names+
  '("B"         "nop"     "file"
    "small"     "xsmall"  "xxsmall"
    "large"     "xlarge"  "xxlarge"
    "LaTeX"     "TeX"     "hearts"
    "userinput" "cursor"  "term")
  "Inline command names added by Re:VIEW Starter.")

(defvar review-starter-default-block-name "list"
  "Block name suggested by `review-starter-block-region'.")

(defvar review-starter-default-inline-command "b"
  "Command name suggested by `review-starter-inline-region'.")

(defvar review-starter-hankaku-zenkaku-mapping-alist
  '(("[" . "【")
    ("]" . "】")
    ("(" . "（")
    (")" . "）")
    ("8" . "（")
    ("9" . "）")
    ("-" . "－")
    ("+" . "＋")
    ("*" . "＊")
    ("/" . "／")
    ("=" . "＝")
    ("\\" . "￥")
    (" " . "　")
    (":" . "：")
    ("<" . "<\\<>"))
  "全角置換キー.")


;;;
;;; Customizable Variables
;;;
(defcustom review-starter-mode-name-string+ "Re:VIEW+"
  "Name displayed in modeline `review-starter-use-expansion' is non-nil."
  :type 'mode-line-format
  :group 'review-starter)

(defcustom review-starter-mode-name-string "Re:VIEW"
  "Name displayed in modeline when `review-starter-use-expansion' is nil."
  :type 'mode-line-format
  :group 'review-starter)

(defcustom review-starter-use-expansion t
  "If non-nil, use Re:VIEW Starter Expansion."
  :type 'boolean
  :group 'review-starter)

(defcustom review-starter-default-compile-command "rake pdf"
  "Default command to compile Re:VIEW Starter files."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-block-names nil
  "User defined review block names.
Combined with `review-starter-standard-block-names' for
minibuffer completion."
  :type '(repeat string)
  :group 'review-starter)

(defcustom review-starter-single-line-block-names nil
  "User defined review single line block names.
Combined with `review-starter-standard-single-line-block-names' for
minibuffer completion."
  :type '(repeat string)
  :group 'review-starter)

(defcustom review-starter-inline-names nil
  "User defined review inline tag names.
Combined with `review-starter-standard-inline-names' for
minibuffer completion."
  :type '(repeat string)
  :group 'review-starter)

(defcustom review-starter-imenu-indent-string ". "
  "String to add repeated in front of nested sectional units for Imenu.
An alternative value is \" . \", if you use a font with a narrow period."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-display-role-name-in-mode-line t
  "If non-nil show `review-starter-role-name' in mode line."
  :type 'boolean
  :group 'review-starter)

(defcustom review-starter-use-whitespace-mode nil
  "If non-nil enable `whitespace-mode'."
  :type 'boolean
  :group 'review-starter)

(defcustom review-starter-use-which-function-mode nil
  "If non-nil enable `which-function-mode'."
  :type 'boolean
  :group 'review-starter)

(defcustom review-starter-comment-start "◆→"
  "編集タグの開始文字."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-comment-end "←◆"
  "編集タグの終了文字."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-dtp-list
  '("DTP連絡")
  "DTP担当名リスト."
  :type '(repeat string)
  :group 'review-starter)

(defcustom review-starter-user-name ""
  "User name."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-role-name "著者"
  "ユーザーの役割.

編集者, 翻訳者, 監訳, 著者, DTP, `review-starter-user-name'が想定
している選択肢です.  詳しくは`review-starter-default-role-alist'
を参照してください."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-tip-name "注"
  "注釈時の名前."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-mode-dtp "DTP連絡"
  "DTP担当の名前."
  :type 'string
  :group 'review-starter)

(defcustom review-starter-use-outline-commands t
  "Whether use outline commnds or not."
  :type 'boolean
  :group 'review-starter)


;;;
;;; Syntax
;;;
(defvar review-starter-mode-syntax-table
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
    (modify-syntax-entry ?& "." st)     ; &
    (modify-syntax-entry ?_ "." st)     ; _
    (modify-syntax-entry ?^ "." st)     ; ^
    st)
  "Syntax table used while in Re:VIEW starter mode.")

(eval-and-compile
  (defconst review-starter-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     ("^#@#"
      (0 "<"))
     ("@\\(<\\).+?\\(>\\){.*[^\\]?}"
      (1 "(")
      (2 ")"))
     ("@\\(<\\).+?\\(>\\)\\(\\$\\).*?\\(\\$\\)"
      (1 "(")
      (2 ")")
      (3 "(")
      (4 ")"))
     ("@\\(<\\).+?\\(>\\)\\(|\\).*?\\(|\\)"
      (1 "(")
      (2 ")")
      (3 "(")
      (4 ")")))
    "Syntax-propertize rules Re:VIEW.
These have to be run via `review-starter-mode-syntax-propertize'")

  (defconst review-starter-syntax-propertize-rules+
    (syntax-propertize-precompile-rules
     review-starter-syntax-propertize-rules
     ;; Use the `b' style of comments to avoid interference with the
     ;; #@+++ ... #--- comments recognized
     ("\\(^#@\\+\\+\\+\\)"
      (1 "< b"))
     ("\\(^#@---\\)"
      (1 "> b")))
    "Syntax-propertize rules Re:VIEW Starter.
These have to be run via `review-starter-mode-syntax-propertize'"))


;;;
;;; Font-Lock support
;;;

(defun review-starter-remove-bracket (string-list)
  "STRING-LISTから\"[]\"をとってその要素の全てにマッチする正規表現にして返す."
  (let ((names string-list)
        (names-wo-paren '())
        (name))
    (while names
      (setq name (pop names))
      (while (string-match "\\[]" name)
        (setq name (replace-match "" nil nil name)))
      (push name names-wo-paren))
    (regexp-opt names-wo-paren nil)))

(defvar review-starter-regexp-block-begin
  (concat "^//"
          (review-starter-remove-bracket
           (append
            review-starter-block-names
            review-starter-standard-block-names
            ))
          "\\(?:\\[.*]\\)*?{\n"))

(defvar review-starter-regexp-block-begin+
  (concat "^//"
          (review-starter-remove-bracket
           review-starter-standard-block-names+)
          "\\(?:\\[.*]\\)*?{\n"))

(defvar review-starter-regexp-single-block
  (concat "^//"
          (review-starter-remove-bracket
           (append
            review-starter-single-line-block-names
            review-starter-standard-single-line-block-names
            ))
          "\\(?:\\[.*]\\)*?\n"))

(defvar review-starter-regexp-single-block+
  (concat "^//"
          (review-starter-remove-bracket
           review-starter-standard-single-line-block-names+)
          "\\(?:\\[.*]\\)*\n"))

(defvar review-starter-regexp-inline
  (concat "\\(@<"
          (review-starter-remove-bracket
           (append
            review-starter-inline-names
            review-starter-standard-inline-names
            ))
          ">\\){.*?[^\\]?}"))

(defvar review-starter-regexp-inline+
  (concat "\\(@<"
          (review-starter-remove-bracket
           review-starter-standard-inline-names+
           )
          ">\\){.*?[^\\]?}"))

(defconst review-starter-regexp-block-end
  "\\(^//}$\\)"
  "Regular expression for matching the end of block.")

(defconst review-starter-regexp-texequation-block-begin
  "\\(^//texequation{\n\\)"
  "Regular expression for matching the beginning of a texequation block.")

(defconst review-starter-regexp-texequation-block
  (concat review-starter-regexp-texequation-block-begin
          "\\(\\(?:.\\|\n\\)*?\\)"
          review-starter-regexp-block-end)
  "Regular expression for matching a texequation block.")

(defconst review-starter-regexp-reference
  (concat "@<"
          (regexp-opt
           '("chap" "title" "chapref" "list"
             "img" "table" "eq" "hd" "column"
             "fn" "bib" "icon"))
          ">{.*?}"))

(defvar review-starter-font-lock-keywords nil
  "Expressions to highlight in Re:VIEW Starter mode.")

(defvar review-starter-font-lock-keywords-default
  `(("^= .*" . 'review-starter-header-face)
    ("^==\\(\\[.*\\]\\)?\\({.*}\\)? .*" . 'review-starter-header1-face)
    ("^===\\(\\[.*\\]\\)?\\({.*}\\)? .*" . 'review-starter-header2-face)
    ("^====\\(\\[.*\\]\\)?\\({.*}\\)? .*" . 'review-starter-header3-face)
    ("^=====\\(\\[.*\\]\\)?\\({.*}\\)? .*" . 'review-starter-header4-face)
    ("^======\\(\\[.*\\]\\)?\\({.*}\\)? .*" . 'review-starter-header5-face)
    ("^=\\{2,6\\}\\[/column\\]" . 'review-starter-column-end-face)
    ("^ \\(\*+\\|[0-9]+\\.\\|:\\) " . 'review-starter-list-item-face)
    ("－" . 'review-starter-fullwidth-hyphen-minus-face)
    ("−"  . 'review-starter-minus-sign-face)
    ("‐" . 'review-starter-hyphen-face)
    ("‒"  . 'review-starter-figure-dash-face)
    ("–"  . 'review-starter-en-dash-face)
    ("―" . 'review-starter-em-dash-face)
    ("―" . 'review-starter-horizontal-bar-face)
    ("“" . 'review-starter-left-quote-face)
    ("”" . 'review-starter-right-quote-face)
    ("‟"  . 'review-starter-reversed-quote-face)
    ("″" . 'review-starter-double-prime-face)
    (,review-starter-regexp-single-block
     (0 'review-starter-block-face))
    (,review-starter-regexp-texequation-block
     (2 'review-starter-math-face t))
    (,review-starter-regexp-block-begin
     (0 'review-starter-block-face t))
    (,review-starter-regexp-block-end
     (0 'review-starter-block-face t))
    (,review-starter-regexp-inline
     (1 'review-starter-inline-tag-face t))
    ("\\(@<ami>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-ami-face append))
    ("\\(@<kw>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-keyword-face append))
    ("\\(@<\\(?:b\\|strong\\|em\\)>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-bold-face append))
    ("\\(@<i>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-oblique-face append))
    ("\\(@<bou>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-bold-face append))
    ("\\(@<u>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-underline-face append))
    ("\\(@<del>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-strike-through-face append))
    ("\\(@<uchar>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-uchar-face append))
    ("\\(@<code>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-code-face append))
    ("\\(@<tt[bi]?>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-fixed-pitch-face t))
    ("\\(@<ttb>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-bold-face append))
    ("\\(@<tti>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-oblique-face prepend))
    ("\\(@<ins>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-underline-face append))
    ("@<br>{}"
     (0 'review-starter-br-face t))
    (,review-starter-regexp-reference
     (0 'review-starter-ref-face t))
    ("@<idx>{.*?[^\\]}"
     (0 'review-starter-idx-face t))
    ("@<hidx>{.*?[^\\]}"
     (0 'review-starter-hidx-face t))
    ("@<balloon>{.*?[^\\]}"
     (0 'review-starter-balloon-face t))
    ("\\(@<m>{\\)\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-math-face t)
     (3 'review-starter-inline-tag-face t))
    ("\\(@<m>\\$\\)\\(.*?\\)\\(\\$\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-math-face t)
     (3 'review-starter-inline-tag-face t))
    ("\\(@<m>\|\\)\\(.*?\\)\\(\|\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-math-face t)
     (3 'review-starter-inline-tag-face t))
    ("◆→[^◆]*←◆"
     (0 'review-starter-warning-face t))
    ("^#@warn.*"
     (0 'review-starter-warning-face t))
    (,review-starter-regexp-block-begin+
     (0 'review-starter-warning-face))
    (,review-starter-regexp-single-block+
     (0 'review-starter-warning-face))
    (,review-starter-regexp-inline+
     (1 'review-starter-warning-face))
    ("^ -+? .+? "
     (0 'review-starter-warning-face))
    )
  "Default expressions to highlight in Re:VIEW Starter mode.

Re:VIEW Starter拡張を含まないfont-lock-keywords.")

(defvar review-starter-font-lock-keywords-default+
  `(("^//\\(begin\\|end\\)child\n"
     (0 'review-starter-warning-face t))
    (,review-starter-regexp-block-begin+
     (0 'review-starter-block-face t))
    (,review-starter-regexp-single-block+
     (0 'review-starter-block-face t))
    (,review-starter-regexp-inline+
     (1 'review-starter-inline-tag-face t))
    ("\\(@<B>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-bold-face append))
    ("\\(@<term>\\){\\(.*?[^\\]\\)\\(}\\)"
     (1 'review-starter-inline-tag-face t)
     (2 'review-starter-keyword-face append))
    ("^ -+? .+? "
     (0 'review-starter-list-item-face t))
    ;; `font-lock-default'のkeyword-onlyがnilでも #@---
    ;; の # までしか `font-lock-comment-delimiter-face' に
    ;; ならなかったので追加.
    ("^#@---"
     (0 font-lock-comment-delimiter-face t)))
  "Default expressions to highlight in Re:VIEW Starter mode.

Re:VIEW Starter拡張も含んだfont-lock-keywords.")

(defun review-starter-syntactic-face (state)
  "Return font-lock face for characters with given STATE.
See `font-lock-syntactic-face-function' for details."
  (let ((in-comment (nth 4 state)))
    (cond
     (in-comment font-lock-comment-face)
     (t nil))))


;;;
;;; Outline support
;;;
(defvar review-starter-section-alist
  '(("=" . 0)
    ("==" . 1)
    ("===" . 2)
    ("====" . 3)
    ("=====" . 4)
    ("======" . 5))
  "Association list of Re:VIEW header.")

(defvar review-starter-outline-regexp
  "^\\(=\\{1,6\\}\\)\\(\\[.+?\\]\\)?\\({.+?[^\\]}\\)? "
  "The regexp matches the outline of Re:VIEW format.

This matches =[nonum], ==[column]{label}, and ==={label} etc.
[CAUTION] It also matches the expression ={label}, which is not
allowed in Re:VIEW format.")

(defun review-starter-outline-level ()
  "Return the outline level.

This function was originally derived from `latex-outline-level'
from `tex-mode.el'."
  (interactive)
  (if (looking-at review-starter-outline-regexp)
      (1+ (or (cdr (assoc (match-string 1) review-starter-section-alist)) -1))
    1000))

(defun review-starter-current-defun-name ()
  "Return the name of the Re:VIEW section or chapter at point, or nil.

This function was originally derived from
`tex-current-defun-name' from `tex-mode.el'."
  (save-excursion
    (when (re-search-backward review-starter-outline-regexp nil t)
      (goto-char (match-end 0))
      (buffer-substring-no-properties
       (point)
       (line-end-position)))))


;;;
;;; Imenu support
;;;
(defun review-starter-imenu-create-index ()
  "Generate an alist for imenu from a Re:VIEW buffer.

This function was originally derived from
`latex-imenu-create-index' from `tex-mode.el'."
  (let (menu)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp review-starter-outline-regexp nil t)
	    (let ((start (match-beginning 0))
	          (here (point))
              (i (cdr (assoc (buffer-substring-no-properties
			                  (match-beginning 1)
			                  (match-end 1))
			                 review-starter-section-alist))))
          (end-of-line)
	      (condition-case nil
	          (progn
		        (push
                 (cons
                  (concat (apply #'concat
					             (make-list i
					                        review-starter-imenu-indent-string))
				          (buffer-substring-no-properties
				           here (point)))
			      start)
		         menu))
	        (error nil))))
      ;; Sort in increasing buffer position order.
      (sort menu (lambda (a b) (< (cdr a) (cdr b)))))))


;;;
;;; Commands
;;;
(defun review-starter-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "review-starter-mode, version %s" review-starter-mode-version))

(defun review-starter-toggle-use-expansion ()
  "Starter拡張を使うかどうかを切り替える."
  (interactive)
  (if review-starter-use-expansion
      (progn (setq review-starter-use-expansion nil) (message "DO NOT Use Starter Expansion."))
    (setq review-starter-use-expansion t)
    (message "Use Starter Expansion."))
  (when (eq major-mode 'review-starter-mode)
    (review-starter-mode)))

(defun review-starter-block (arg)
  "Make Re:VIEW block (//blockname[...][...]{... //} pair).
With optional ARG, modify current block.

選択領域を指定したタグで囲みます.

もしRegionが選択されていたら, その領域を指定したタグで囲みます.
もしRegionが選択されていなかったら, 現在のカーソル位置をタグで囲
みます.  もしARGありで呼ばれた場合は、Regionが選択されているかに
よらず, カーソル位置より前の, 最も近いタグを変更します."
  (interactive "*P")
  (let* ((pattern (completing-read
                   (concat "Tag [" review-starter-default-block-name "]: ")
                   (append review-starter-standard-block-names
                           review-starter-block-names
                           review-starter-standard-single-line-block-names
                           review-starter-single-line-block-names
                           (when review-starter-use-expansion
                             (append review-starter-standard-block-names+
                                     review-starter-standard-single-line-block-names+)
                             ))
                   nil nil nil nil review-starter-default-block-name)))
    (unless (equal review-starter-default-block-name pattern)
      (setq review-starter-default-block-name pattern))
    (if arg
        (review-starter-modify-block pattern)
      (review-starter-insert-block pattern))))

(defun review-starter-modify-block (pattern)
  "Modify current block.

[WiP] 現在の実装だと, 現在位置の一つ前のタグをPATTERNに変更します.
将来的には`get-text-property'をつかって今blockの中にいるかを判定
し, blockにいるときはそのblockを変更し、そうでないときはエラーメッ
セージを出すようにする予定です."
  (save-excursion
    (if (re-search-backward (concat "//" ".+{"))
        (replace-match (concat "//" pattern "{"))
      (message "カーソル位置より前にタグが見つかりません.")
      )))

(defun review-starter-insert-block (name)
  "Insert NAME block around point or region."
  (let* ((single-line-block-names
          (append review-starter-standard-single-line-block-names
                  review-starter-single-line-block-names
                  (when review-starter-use-expansion
                    review-starter-standard-single-line-block-names+))))
    (cond
     ((region-active-p)
	  (save-restriction
	    (narrow-to-region (region-beginning) (region-end))
	    (goto-char (point-min))
	    (cond
         ((member name single-line-block-names)
		  (insert "//" name)
          (newline))
	     (t
		  (insert "//" name "{")
          (newline)
		  (goto-char (point-max))
		  (insert "//" "}")
          (newline)))))
     (t
	  (cond
       ((member name single-line-block-names)
	    (insert "//" name)
        (newline))
       (t
        (save-excursion
          (insert "//" name "{")
          (newline)
		  (insert "//" "}")
          (newline))
        (forward-word)
        (forward-char)
        )))
     )))

(defun review-starter-insert-child-block ()
  "Insert a child block (//beginchild //endchild pair) around point or region.

※ Re:VIEWでは箇条書きの途中に何かを含めたいときには、それを
\"//beginchild 〜//endchild\" で囲んで配置します.  多重に入れ子に
することも可能です.  Re:VIEW Starterではそもそもブロックを入れ子
にできるのでこの表記は使いません."
  (interactive)
  (cond
   ((region-active-p)
	(save-restriction
	  (narrow-to-region (region-beginning) (region-end))
	  (goto-char (point-min))
	  (insert "//beginchild")
      (newline 2)
	  (goto-char (point-max))
      (newline 2)
	  (insert "//endchild")
      (newline)))
   (t
	(insert "//beginchild")
    (newline 4)
	(insert "//endchild")
    (newline)
    (forward-line -3))))

(defun review-starter-inline-region ()
  "Insert NAME inline command around point or region."
  (interactive)
  (let* ((pattern (completing-read
                   (concat "タグ [" review-starter-default-inline-command "]: ")
                   (append review-starter-standard-inline-names
                           review-starter-inline-names
                           (when review-starter-use-expansion
                             review-starter-standard-inline-names+))
                   nil nil nil nil review-starter-default-inline-command)))
    (unless (equal review-starter-default-inline-command pattern)
      (setq review-starter-default-inline-command pattern))
    (cond
     ((region-active-p)
	  (save-restriction
	    (narrow-to-region (region-beginning) (region-end))
	    (goto-char (point-min))
	    (insert "@<" pattern ">{")
	    (goto-char (point-max))
	    (insert "}")))
	 (t
	  (insert "@<" pattern ">{}")
	  (backward-char)
	  ))
    ))

(defun review-starter-string-region (markb marke)
  "Surround point or region with MARKB and MARKE."
  (cond ((region-active-p)
	     (save-restriction
	       (narrow-to-region (region-beginning) (region-end))
	       (goto-char (point-min))
	       (insert markb)
	       (goto-char (point-max))
	       (insert marke)))
	    (t
	     (insert markb marke)
	     (backward-char))))

(defun review-starter-bold-region ()
  "選択領域を太字タグ(@<b>)で囲みます."
  (interactive)
  (review-starter-string-region "@<b>{" "}"))

(defun review-starter-keyword-region ()
  "選択領域をキーワードフォントタグ(@<kw>)で囲みます."
  (interactive)
  (review-starter-string-region "@<kw>{" "}"))

(defun review-starter-italic-region ()
  "選択領域をイタリックフォントタグ(@<i>)で囲みます."
  (interactive)
  (review-starter-string-region "@<i>{" "}"))

(defun review-starter-em-region ()
  "選択領域を強調タグ(@<em>)で囲みます."
  (interactive)
  (review-starter-string-region "@<em>{" "}"))

(defun review-starter-strong-region ()
  "選択領域を強調タグ(@<strong>)で囲みます."
  (interactive)
  (review-starter-string-region "@<strong>{" "}"))

(defun review-starter-underline-italic-region ()
  "選択領域を等幅イタリックフォントタグ(@<tti>)で囲みます."
  (interactive)
  (review-starter-string-region "@<tti>{" "}"))

(defun review-starter-underline-region ()
  "選択領域を等幅タグ(@<u>)で囲みます."
  (interactive)
  (review-starter-string-region "@<u>{" "}"))

(defun review-starter-tt-region ()
  "選択領域を等幅タグ(@<tt>)で囲みます."
  (interactive)
  (review-starter-string-region "@<tt>{" "}"))

(defun review-starter-hyperlink-region ()
  "選択領域をハイパーリンクタグ(@<href>)で囲みます."
  (interactive)
  (review-starter-string-region "@<href>{" "}"))

(defun review-starter-code-region ()
  "選択領域をコードタグ(@<code>)で囲みます."
  (interactive)
  (review-starter-string-region "@<code>{" "}"))

(defun review-starter-math-region ()
  "選択領域を数式タグ(@<m>)で囲みます.

[WiP] 選択領域に\"}\"が含まれる場合はフェンスに$$を使い. そうでな
いときは$を使うが, この使い分けはいらないかも？"
  (interactive)
  (if (region-active-p)
      (save-restriction
	    (narrow-to-region (region-beginning) (region-end))
	    (goto-char (point-min))
	    (if (string-match "}" (buffer-string))
	        (progn
              (insert "@<m>$")
              (goto-char (point-max))
              (insert "$"))
	      (progn
	        (insert "@<m>{")
	        (goto-char (point-max))
	        (insert "}"))))
    (progn
      (insert "@<m>$$")
      (backward-char)
      )
    ))

(defun review-starter-index-region ()
  "選択領域を出力付き索引化(@<idx>)で囲みます."
  (interactive)
  (review-starter-string-region "@<idx>{" "}"))

(defun review-starter-balloon-comment (pattern)
  "PATTERNを吹き出しとして挿入する."
  (interactive "s吹き出し: \n")
  (insert "@<balloon>{" pattern "}"))

(defun review-starter-kokomade ()
  "一時終了タグを挿入.

作業途中の疑似マーカーを挿入します."
  (interactive)
  (insert review-starter-comment-start "ここまで -" review-starter-role-name
          review-starter-comment-end  "\n"))

(defun review-starter-normal-comment (pattern)
  "PATTERNをコメントとして挿入.

ユーザーから編集者へのメッセージ疑似マーカーを挿入します."
  (interactive "sコメント: \n")
  (insert review-starter-comment-start pattern " -" review-starter-role-name
          review-starter-comment-end))

(defun review-starter-dtp-comment (pattern)
  "PATTERNをDTP向けコメントを挿入.

DTP担当へのメッセージ疑似マーカーを挿入します."
  (interactive "sDTP向けコメント: \nP")
  (insert review-starter-comment-start review-starter-mode-dtp
          ":" pattern " -" review-starter-role-name review-starter-comment-end))

(defun review-starter-tip-comment (pattern)
  "PATTERNを注釈コメントとして挿入.

ユーザー注釈の疑似マーカーを挿入します."
  (interactive "s注釈コメント: \n")
  (insert review-starter-comment-start review-starter-tip-name
          ":" pattern " -" review-starter-role-name review-starter-comment-end))

(defun review-starter-reference-comment ()
  "参照コメントを挿入.

参照先をあとで確認する疑似マーカーを挿入します."
  (interactive)
  (insert review-starter-comment-start "参照先確認 -"
          review-starter-role-name review-starter-comment-end))

(defun review-starter-insert-index ()
  "索引 @<hidx>{} 挿入用の関数.

領域が選択されているか、それがインラインタグのカッコ内全部,
つまり, @<tag>{XYZ}のXYZであるかに応じて以下のように動作が変わる.
なお, @<tag>{XYZ}でXYやYZのみが選択されていた場合は 3番の動作になる.

1. 領域が選択されていなかったら, ユーザーの入力を受け取りそれを索
引としてカーソル位置に挿入する.

2. 領域が選択されていて, それがインラインタグのカッコ内全てであれ
ばその内容をタグの直前に索引として挿入する.

3. それ以外の場合で、領域が選択されているときは選択領域を索引とし
て領域の直前に挿入する."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (review-starter-index-buffer (buffer-substring-no-properties start end))
           (string-after (char-to-string (char-after end)))
           (strings-before
            (concat
             (char-to-string (char-before (- start 1)))
             (char-to-string (char-before start)))))
      ;; インラインタグ内の全領域かの判定.
      ;; regionの直前の文字が ">{"で直後が "}" だったら@<.+?>{を探し,
      ;; マッチした位置の終わりがregionの始まりに一致した場合は,
      ;; @の直前をstartに変更する.
      (when (and (equal string-after "}")
                 (equal strings-before ">{")
                 (re-search-backward "@<.+?[^}]>{" nil t)
                 (= start (match-end 0)))
        (setq start (point)))
	  (save-restriction
	    (narrow-to-region start end)
        (goto-char (point-min))
	    (insert "@<hidx>{" review-starter-index-buffer "}"))))
   (t
    (let ((pattern (read-from-minibuffer "索引: ")))
      (insert "@<hidx>{" pattern "}"))))
  )

(defun review-starter-insert-header (num)
  "Level NUMの見出しを挿入します."
  (interactive "nヘッダレベル: \n")
  (insert (make-string num ?=) " "))

(defun review-starter-insert-br ()
  "強制改行タグ(@<br>{})を挿入します."
  (interactive)
  (insert "@<br>{}"))

(defun review-starter-opentag (pattern)
  "PATTERNをraw html開始タグとして挿入します."
  (interactive "sタグ: \n")
  (insert "//raw[|html|<" pattern ">]"))

(defun review-starter-closetag (pattern)
  "PATTERNをraw html終了タグとして挿入します."
  (interactive "sタグ: \n")
  (insert "//raw[|html|</" pattern ">]"))

(defconst review-starter-regexp-uri
  "\\(\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]+\\)\\|\\(\\([^-A-Za-z0-9!_.%]\\|^\\)[-A-Za-z0-9._!%]+@[A-Za-z0-9][-A-Za-z0-9._!]+[A-Za-z0-9]\\)"
  "URI選択部分正規表現.")

(defun review-starter-search-uri ()
  "前方向にURIを検索して一番始めに見つかったものをブラウザで表示する.
カーソルはそのURIに移動する.  Regionがactiveなときはその範囲の先
頭からURIを探し, それをブラウザで開く."
  (interactive)
  (let ((func
         `(lambda ()
            (if (not (re-search-forward review-starter-regexp-uri nil t))
                (message "URI not found!")
              (goto-char (match-beginning 1))
              (browse-url (match-string 1))
              )
            )))
    (cond
     ((region-active-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
	    (goto-char (point-min))
        (funcall func)
        )
      )
     (t
      (funcall func))))
  )

(defun review-starter-zenkaku-mapping (key)
  "全角文字の挿入.

半角文字のKEYを与えると,
`review-starter-hankaku-zenkaku-mapping-alist' から対応する全角文
字を持ってきて挿入する.  存在しなくても特にエラーは吐かない."
  (insert (cdr (assoc key review-starter-hankaku-zenkaku-mapping-alist))))

(defun review-starter-zenkaku-mapping-lparenthesis ()
  "全角(を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "("))

(defun review-starter-zenkaku-mapping-rparenthesis ()
  "全角)を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping ")"))

(defun review-starter-zenkaku-mapping-langle ()
  "全角[を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "["))

(defun review-starter-zenkaku-mapping-rangle ()
  "全角]を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "]"))

(defun review-starter-zenkaku-mapping-plus ()
  "全角+を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "+"))

(defun review-starter-zenkaku-mapping-minus ()
  "全角-を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "-"))

(defun review-starter-zenkaku-mapping-asterisk ()
  "全角*を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "*"))

(defun review-starter-zenkaku-mapping-slash ()
  "全角/を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "/"))

(defun review-starter-zenkaku-mapping-equal ()
  "全角=を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "="))

(defun review-starter-zenkaku-mapping-yen ()
  "全角￥を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "\\"))

(defun review-starter-zenkaku-mapping-space ()
  "全角スペースを挿入する."
  (interactive)
  (review-starter-zenkaku-mapping " "))

(defun review-starter-zenkaku-mapping-colon ()
  "全角:を挿入する."
  (interactive)
  (review-starter-zenkaku-mapping ":"))

(defun review-starter-zenkaku-mapping-lbracket ()
  "<タグを挿入する."
  (interactive)
  (review-starter-zenkaku-mapping "<"))

(defun review-starter-change-role-sub (number)
  "編集役割を変更するためのサブルーチン.
編集役割をを`review-starter-default-role-alist'のNUMBER番目に変更する."
  (let ((list (nth number review-starter-default-role-alist)))
    (setq review-starter-role-name (car list))
    ))

(defun review-starter-change-mode ()
  "編集モードと作業者名を変更します."
  (interactive)
  (let (key message element (list review-starter-default-role-alist) (sum 0))
    (while list
      (setq element (car (car list)))
      (setq sum ( + sum 1))
      (if message
          (setq message (format "%s%d.%s " message sum element))
	    (setq message (format "%d.%s " sum element))
	    )
      (setq list (cdr list))
      )
    (message (concat "編集モード: " message ":"))
    (setq key (read-char))
    (cond
     ((eq key ?1) (review-starter-change-role-sub 0))
     ((eq key ?2) (review-starter-change-role-sub 1))
     ((eq key ?3) (review-starter-change-role-sub 2))
     ((eq key ?4) (review-starter-change-role-sub 3))
     ((eq key ?5) (review-starter-change-role-sub 4))))
  (setq review-starter-tip-name (cdr (assoc review-starter-role-name
                                            review-starter-default-role-alist)))
  (message (concat "現在のモード: " review-starter-role-name))
  (setq mode-name
        (concat (if review-starter-use-expansion
                    review-starter-mode-name-string+
                  review-starter-mode-name-string)
                "/"
                review-starter-role-name)))

(defun review-starter-change-dtp ()
  "DTP担当を変更します."
  (interactive)
  (let ((sub `(lambda (number)
                "DTP担当変更サブルーチン."
                (let (list)
                  (setq list (nth number review-starter-dtp-list))
                  (setq review-starter-dtp-name list)
                  (message (concat "現在のDTP: " review-starter-dtp-name)))))
        key message element (list review-starter-dtp-list) (sum 0))
    (while list
      (setq element (car list))
      (setq sum ( + sum 1))
      (if message
          (setq message (format "%s%d.%s " message sum element))
	    (setq message (format "%d.%s " sum element))
	    )
      (setq list (cdr list))
      )
    (message (concat "DTP担当: " message ":"))
    (setq key (read-char))
    (cond
     ((eq key ?1) (funcall sub 0))
     ((eq key ?2) (funcall sub 1))
     ((eq key ?3) (funcall sub 2))
     ((eq key ?4) (funcall sub 3))
     ((eq key ?5) (funcall sub 4)))))

(defun review-starter-index-change ()
  "選択領域を索引として追記する.  索引からは()とスペースを取る."
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (region-string (buffer-substring-no-properties start end)))
	    (save-restriction
	      (narrow-to-region start end)
          (goto-char (point-min))
	      (while (re-search-forward "\(\\|\)\\| " nil t)
	        (replace-match "" nil nil))
	      (goto-char (point-max))
	      (insert "@" region-string)))
    (message "索引にする範囲を選択してください")
    ))

(defun review-starter-page-increment-region (num)
  "選択領域のページ数をNUM増減する (DTP作業用)."
  (interactive "n増減値: \n")
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (re-search-forward "^\\([0-9][0-9]*\\)\t" nil t)
            (replace-match
             (concat (number-to-string
                      (+ num (string-to-number (match-string 1))))
                     "\t"))))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (re-search-forward "^p\\.\\([0-9][0-9]*\\) " nil t)
            (replace-match
             (concat "p."
                     (number-to-string
                      (+ num (string-to-number (match-string 1))))
                     " ")))))
    (message "Region NOT ACTIVE!")))

(defun review-starter-surround-tt ()
  "カーソル位置から後続の英字記号範囲を選択して等幅化する."
  (interactive)
  (re-search-forward "[-a-zA-Z0-9_=?#$@~`%&*+|()'\\/.,:<>]+")
  (goto-char (match-end 0))
  (insert "}")
  (goto-char (match-beginning 0))
  (insert "@<tt>{")
  (goto-char (+ 7 (match-end 0))))

(defun review-starter-compile ()
  "Run rake command on the current document."
  (interactive)
  (cond
   ((file-exists-p "Rakefile")
    (review-starter-compile-exec-command)
    )
   ((file-exists-p "../Rakefile")
    (let ((current-dir default-directory))
      (cd "..")
      (review-starter-compile-exec-command)
      (cd current-dir)))
   (t
    (message "Rakefile not found!"))))

(defun review-starter-compile-exec-command ()
  "Execute rake command."
  (call-interactively 'compile))

(defun review-starter-at-heading-p (&optional _)
  "Non-nil when on a headline."
  (outline-on-heading-p t))

(defun review-starter-at-list-p (&optional _)
    "Non-nil when on a list."
    (save-excursion
      (beginning-of-line)
      (looking-at "^ \\*+ ")))

(defun review-starter-at-num-list-p (&optional _)
    "Non-nil when on a list."
    (save-excursion
      (beginning-of-line)
      (looking-at "^ [0-9]+?\\. ")))

(defun review-starter-at-list+-p (&optional _)
    "Non-nil when on a list."
    (save-excursion
      (beginning-of-line)
      (looking-at "^ -+? .+? ")))

(defun review-starter-cycle (&optional arg)
  "TAB-action and visibility cycling for Re:VIEW Starter mode.

If cursor is at heading, then call `outline-cycle' which cycles between
'hide all', 'headings only' and 'show all'.  Otherewise, call
`indent-for-tab-command' with ARG."
  (interactive "P")
  (cond
   ((review-starter-at-heading-p)
    (outline-cycle))
   (t
    (indent-for-tab-command arg))))

(defalias 'review-starter-cycle-buffer 'outline-cycle-buffer)

(defun review-starter-insert-heading ()
  "空気を読んでheadingや箇条書きを挿入する."
  (interactive)
  (cond
   ((review-starter-at-list-p)
    (beginning-of-line)
    (re-search-forward "^ \\*+? ")
    (end-of-line)
    (newline)
    (insert (match-string 0)))
   ((review-starter-at-num-list-p)
    (beginning-of-line)
    (re-search-forward "^ \\([0-9]+?\\)\\. ")
    (end-of-line)
    (newline)
    (insert (concat " "
                    (number-to-string
                     (1+ (string-to-number (match-string 1))))
                    ". ")))
   ((and review-starter-use-expansion
         (review-starter-at-list+-p))
    (beginning-of-line)
    (re-search-forward "^ -+? ")
    (end-of-line)
    (newline)
    (insert (match-string 0)))
   (t
    (outline-insert-heading))
   ))

(defalias 'review-starter-move-subtree-down 'outline-move-subtree-down)
(defalias 'review-starter-move-subtree-up 'outline-move-subtree-up)

(defun review-starter-promote (&optional n)
  "Promote the current heading higher up the tree.

If cursor is at heading or list, promote the the current heading
higher up the tree.  Otherewise call `left-word' with N"
  (interactive "^p")
  (cond
   ((review-starter-at-heading-p)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^=" nil t)
          (replace-match "")
        (message "Already Top Level!"))))
   ((review-starter-at-list-p)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^ \\*\\*" nil t)
          (replace-match " *")
        (message "Already Top Level!"))))
   ((and review-starter-use-expansion
         (review-starter-at-list+-p))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^ --" nil t)
          (replace-match " -")
        (message "Already Top Level!"))))
   (t
    (left-word n))))

(defun review-starter-demote (&optional n)
  "Demote the current heading lower down the tree.

If cursor is at heading or list, deomote the the current heading
higher up the tree.  Otherewise call `right-word' with N"
  (interactive "^p")
  (cond
   ((review-starter-at-heading-p)
    (save-excursion
      (beginning-of-line)
      (insert "=")))
   ((review-starter-at-list-p)
    (save-excursion
      (beginning-of-line)
      (forward-char)
      (insert "*")))
   ((and review-starter-use-expansion
         (review-starter-at-list+-p))
    (save-excursion
      (beginning-of-line)
      (forward-char)
      (insert "-")))
   (t
    (right-word n))))


;;;
;;; Keymap
;;;
(defvar review-starter-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Most frequently used commands
    (define-key map "\C-c\C-c" 'review-starter-compile)
    (define-key map "\C-c\C-e" 'review-starter-block)
    (define-key map "\C-c\C-i" 'review-starter-inline-region)
    ;; font
    (define-key map "\C-c\C-f\C-f" 'review-starter-inline-region)
    (define-key map "\C-c\C-fb" 'review-starter-bold-region)
    (define-key map "\C-c\C-f\C-b" 'review-starter-bold-region)
    (define-key map "\C-c\C-fk" 'review-starter-keyword-region)
    (define-key map "\C-c\C-f\C-k" 'review-starter-keyword-region)
    (define-key map "\C-c\C-fi" 'review-starter-italic-region)
    (define-key map "\C-c\C-f\C-i" 'review-starter-italic-region)
    (define-key map "\C-c\C-fe" 'review-starter-em-region)
    (define-key map "\C-c\C-f\C-e" 'review-starter-em-region)
    (define-key map "\C-c\C-fs" 'review-starter-strong-region)
    (define-key map "\C-c\C-f\C-s" 'review-starter-strong-region)
    (define-key map "\C-c\C-fu" 'review-starter-underline-region)
    (define-key map "\C-c\C-f\C-u" 'review-starter-underline-region)
    (define-key map "\C-c\C-ft" 'review-starter-tt-region)
    (define-key map "\C-c\C-f\C-t" 'review-starter-tt-region)
    (define-key map "\C-c\C-fa" 'review-starter-underline-italic-region)
    (define-key map "\C-c\C-f\C-a" 'review-starter-underline-italic-region)
    (define-key map "\C-c\C-fm" 'review-starter-math-region)
    (define-key map "\C-c\C-f\C-m" 'review-starter-math-region)
    (define-key map "\C-c\C-fc" 'review-starter-code-region)
    (define-key map "\C-c\C-f\C-c" 'review-starter-code-region)
    (define-key map "\C-c\C-fh" 'review-starter-hyperlink-region)
    (define-key map "\C-c\C-f\C-h" 'review-starter-hyperlink-region)
    ;; marker & comment
    (define-key map "\C-c\C-a" 'review-starter-normal-comment)
    (define-key map "\C-c\C-d" 'review-starter-dtp-comment)
    (define-key map "\C-c\C-k" 'review-starter-tip-comment)
    (define-key map "\C-c\C-r" 'review-starter-reference-comment)
    (define-key map "\C-c!" 'review-starter-kokomade)
    ;; role
    (define-key map "\C-c\C-t1" 'review-starter-change-mode)
    (define-key map "\C-c\C-t2" 'review-starter-change-dtp)
    ;; index
    (define-key map "\C-c\C-fn" 'review-starter-index-region)
    (define-key map "\C-c\C-f\C-n" 'review-starter-index-region)
    (define-key map "\C-c\C-w" 'review-starter-insert-index)
    (define-key map "\C-c\C-y" 'review-starter-index-change) ; 使う?
    ;; 全角文字の挿入 (Major Modeの規約違反ではある. Minor Modeにする?)
    (define-key map "\C-c8" 'review-starter-zenkaku-mapping-lparenthesis)
    (define-key map "\C-c9" 'review-starter-zenkaku-mapping-rparenthesis)
    (define-key map "\C-c\(" 'review-starter-zenkaku-mapping-lparenthesis)
    (define-key map "\C-c\)" 'review-starter-zenkaku-mapping-rparenthesis)
    (define-key map "\C-c\[" 'review-starter-zenkaku-mapping-langle)
    (define-key map "\C-c\]" 'review-starter-zenkaku-mapping-rangle)
    (define-key map "\C-c-" 'review-starter-zenkaku-mapping-minus)
    (define-key map "\C-c+" 'review-starter-zenkaku-mapping-plus)
    (define-key map "\C-c*" 'review-starter-zenkaku-mapping-asterisk)
    (define-key map "\C-c/" 'review-starter-zenkaku-mapping-slash)
    (define-key map "\C-c\\" 'review-starter-zenkaku-mapping-yen)
    (define-key map "\C-c=" 'review-starter-zenkaku-mapping-equal)
    (define-key map "\C-c " 'review-starter-zenkaku-mapping-space)
    (define-key map "\C-c:" 'review-starter-zenkaku-mapping-colon)
    ;; Misc
    (define-key map "\C-c\C-p" 'review-starter-insert-header)
    (define-key map "\C-c\C-m" 'review-starter-insert-br)
    (define-key map "\C-c<" 'review-starter-opentag)
    (define-key map "\C-c>" 'review-starter-closetag)
    (define-key map "\C-c\C-b" 'review-starter-balloon-comment)
    (define-key map "\C-c1" 'review-starter-search-uri)
    map)
  "Keymap for `revew-mode'.")


;;;
;;; Mode Definitions
;;;

;;;###autoload
(define-derived-mode review-starter-mode outline-mode
  (concat
   (if review-starter-use-expansion
       review-starter-mode-name-string+
     review-starter-mode-name-string)
   (when review-starter-display-role-name-in-mode-line
     (concat "/" review-starter-role-name)))
  "Major mode for editing Re:VIEW Starter."

  ;; Use tab indentation
  (setq-local indent-tabs-mode 1)
  (setq-local tab-width 4)

  ;; Regexp isearch should accept newline and formfeed as whitespace.
  (setq-local search-whitespace-regexp "[ \t\r\n\f]+")

  ;; Outline
  (setq-local add-log-current-defun-function #'review-starter-current-defun-name)
  (setq-local outline-heading-alist review-starter-section-alist)
  (setq-local outline-regexp review-starter-outline-regexp)
  (setq-local outline-level  #'review-starter-outline-level)

  ;; Imenu
  (setq-local imenu-create-index-function #'review-starter-imenu-create-index)

  ;; Comments
  (setq-local comment-start "#@# ")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-style 'plain)

  ;; Syntax
  (setq-local syntax-propertize-function
              (if review-starter-use-expansion
                  (syntax-propertize-rules review-starter-syntax-propertize-rules+)
                (syntax-propertize-rules review-starter-syntax-propertize-rules)))

  ;; Font-lock.
  (setq-local review-starter-font-lock-keywords
              (append review-starter-font-lock-keywords-default
                      (when review-starter-use-expansion
                        review-starter-font-lock-keywords-default+
                        )))
  (setq-local font-lock-defaults
              '((review-starter-font-lock-keywords)
    	        nil nil nil nil
                (font-lock-multiline . t)
    	        (font-lock-mark-block-function . mark-paragraph)
    	        (font-lock-syntactic-face-function . review-starter-syntactic-face)
                ))

  ;; Commands
  (setq-local compile-command review-starter-default-compile-command)

  ;; keymap
  (use-local-map review-starter-mode-map)
  (let ((map review-starter-mode-map))
    (if review-starter-use-expansion
        (define-key map "\C-c\C-o" nil)
      (define-key map "\C-c\C-o" 'review-starter-insert-child-block))
    (when review-starter-use-outline-commands
      (define-key map (kbd "<tab>") 'review-starter-cycle)
      (define-key map (kbd "<backtab>") 'review-starter-cycle-buffer)
      (define-key map (kbd "M-RET") 'review-starter-insert-heading)
      (define-key map (kbd "M-<down>") 'review-starter-move-subtree-down)
      (define-key map (kbd "M-<up>") 'review-starter-move-subtree-up)
      (define-key map (kbd "M-<left>") 'review-starter-promote)
      (define-key map (kbd "M-<right>") 'review-starter-demote))
    )

  ;; Misc
  (when review-starter-use-whitespace-mode
    (whitespace-mode 1))
  (when review-starter-use-which-function-mode
    (which-function-mode 1))

  ;; hook
  (run-hooks 'review-starter-mode-hook))


;; Associate .re files with review-starter-mode
;;;###autoload
(setq auto-mode-alist (append '(("\\.re$" . review-starter-mode)) auto-mode-alist))

(provide 'review-starter-mode)
;;; review-starter-mode.el ends here
