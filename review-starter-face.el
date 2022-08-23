;;; review-starter-face.el --- Faces for review-starter-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defgroup review-starter-faces nil
  "Faces used in Re:View Starter Mode."
  :group 'review-starter-mode
  :group 'faces)

(defface review-starter-header-face
  '((((class color) (background light)) :foreground "darkgreen")
    (((class color) (background dark)) :foreground "spring green")
    (t :weight bold))
  "Face used to highlight header."
  :group 'review-starter-faces)

(defface review-starter-header1-face
  '((((class color) (background light)) :foreground "darkgreen")
    (((class color) (background dark)) :foreground "spring green")
    (t :weight bold))
  "Face used to highlight header1."
  :group 'review-starter-faces)

(defface review-starter-header2-face
  '((((class color) (background light)) :foreground "darkgreen")
    (((class color) (background dark)) :foreground "spring green")
    (t :weight bold))
  "Face used to highlight header2."
  :group 'review-starter-faces)

(defface review-starter-header3-face
  '((((class color) (background light)) :foreground "darkgreen")
    (((class color) (background dark)) :foreground "spring green")
    (t :weight bold))
  "Face used to highlight header3."
  :group 'review-starter-faces)

(defface review-starter-header4-face
  '((((class color) (background light)) :foreground "darkgreen")
    (((class color) (background dark)) :foreground "spring green")
    (t :weight bold))
  "Face used to highlight header4."
  :group 'review-starter-faces)

(defface review-starter-header5-face
  '((((class color) (background light)) :foreground "darkgreen")
    (((class color) (background dark)) :foreground "spring green")
    (t :weight bold))
  "Face used to highlight header5."
  :group 'review-starter-faces)

(defface review-starter-column-end-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used to highlight column end."
  :group 'review-starter-faces)

(defface review-starter-list-item-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to highlight list item."
  :group 'review-starter-faces)

(defface review-starter-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to highlight comments.

今のところ使用していない."
  :group 'review-starter-faces)

(defface review-starter-warning-face
  '((t (:foreground "Red")))
  "Face used to highlight warning.")

(defface review-starter-br-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to highlight @<br>{}."
  :group 'review-starter-faces)

(defface review-starter-block-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used to highlight block."
  :group 'review-starter-faces)

(defface review-starter-inline-tag-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used to highlight inline-tag."
  :group 'review-starter-faces)

(defface review-starter-code-face
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight code region."
  :group 'review-starter-faces)

(defface review-starter-math-face
  '((t (:inherit font-lock-string-face)))
  "Face used to highlight math region."
  :group 'review-starter-faces)

(defface review-starter-keyword-face
  '((t (:inherit font-lock-keyword-face :bold t)))
  "Face used to hilight keyword.")

(defface review-starter-bold-face
  '((t (:inherit bold)))
  "Face used to highlight bold text."
  :group 'review-starter-faces)

(defface review-starter-italic-face
  '((t (:inherit italic)))
  "Face used to highlight italic."
  :group 'review-starter-faces)

(defface review-starter-oblique-face
  '((t (:slant oblique)))
  "Face used to highlight italic."
  :group 'review-starter-faces)

(defface review-starter-underline-face
  '((t (:inherit underline)))
  "Face used to highlight underline."
  :group 'review-starter-faces)

(defface review-starter-underlinebold-face
  '((t (:inherit bold :underline t)))
  "Face used to highlight underlinebold."
  :group 'review-starter-faces)

(defface review-starter-fixed-pitch-face
  '((t (:fixed-pitch t)))
  "Face used in fixed-pitch area."
  :group 'review-starter-faces)

(defface review-starter-strike-through-face
  '((t (:strike-through t)))
  "Face for strike-through text."
  :group 'review-starter-faces)

(defface review-starter-ami-face
  '((((class color) (background light)) :background "dark gray")
    (((class color) (background dark)) :background "dim gray"))
  "網掛けに対して使われるface."
  :group 'review-starter-faces)

(defface review-starter-uchar-face
  '((t (:inherit font-lock-type-face)))
  "Face used in uchar."
  :group 'review-starter-faces)

(defface review-starter-idx-face
  '((t (:bold t :foreground "SlateGrey")))
  "Face used to highlight index."
  :group 'review-starter-faces)

(defface review-starter-hidx-face
  '((((class color) (background light)) :foreground "plum4")
    (((class color) (background dark)) :foreground "plum2")
    (t :weight bold))
  "Face used to highlight index."
  :group 'review-starter-faces)

(defface review-starter-balloon-face
  '((t (:foreground "CornflowerBlue")))
  "Face used to highlight balloon."
  :group 'review-starter-faces)

(defface review-starter-ref-face
  '((((class color) (background light)) :foreground "yellow4")
    (((class color) (background dark)) :foreground "yellow2")
    (t :weight bold))
  "Face used to highlight reference."
  :group 'review-starter-faces)

(defface review-starter-bracket-face
  '((t (:bold t :foreground "DarkBlue")))
  "Face used to highlight \"<\".

今のところ使用していない."
  :group 'review-starter-faces)

(defface review-starter-fullwidth-hyphen-minus-face
  '((t (:foreground "grey90" :bkacground "red")))
  "全角ハイフン/マイナスのフェイス."
  :group 'review-starter-faces)

(defface review-starter-minus-sign-face
  '((t (:background "grey90")))
  "全角ハイフン/マイナスのフェイス."
  :group 'review-starter-faces)

(defface review-starter-hyphen-face
  '((t (:background "maroon1")))
  "全角ハイフンのフェイス."
  :group 'review-starter-faces)

(defface review-starter-figure-dash-face
  '((t (:foreground "white" :background "firebrick")))
  "Figureダッシュ(使うべきでない)のフェイス."
  :group 'review-starter-faces)

(defface review-starter-en-dash-face
  '((t (:foreground "white" :background "sienna")))
  "半角ダッシュ(使うべきでない)のフェイス."
  :group 'review-starter-faces)

(defface review-starter-em-dash-face
  '((t (:background "honeydew1")))
  "全角ダッシュのフェイス."
  :group 'review-starter-faces)

(defface review-starter-horizontal-bar-face
  '((t (:background "LightSkyBlue1.")))
  "水平バーのフェイス."
  :group 'review-starter-faces)

(defface review-starter-left-quote-face
  '((t (:foreground "medium sea green")))
  "開き二重引用符のフェイス."
  :group 'review-starter-faces)

(defface review-starter-right-quote-face
  '((t (:foreground "LightSlateBlue")))
  "閉じ二重引用符のフェイス."
  :group 'review-starter-faces)

(defface review-starter-reversed-quote-face
  '((t (:foreground "LightCyan" :background "red")))
  "開き逆二重引用符(使うべきでない)のフェイス."
  :group 'review-starter-faces)

(defface review-starter-double-prime-face
  '((t (:foreground "light steel blue" :background "red")))
  "閉じ逆二重引用符(使うべきでない)のフェイス."
  :group 'review-starter-faces)

(provide 'review-starter-face)
;;; review-starter-face.el ends here
