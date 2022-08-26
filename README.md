# review-starter-el
Major mode of Emacs for [Re:VIEW](https://reviewml.org/ja/)
and [Re:VIEW Starter](https://kauplan.org/reviewstarter/).

If `review-starter-use-expansion` is nil, this package supports Re:VIEW,
and if non-nil, this it supports Re:VIEW Starter.

It was mainly derived from [review-mode.el](https://github.com/kmuto/review-el/).

まだbeta versionなので非互換な更新がされる可能性が高いです.

## シンタックスハイライトについて
Re:VIEWとRe:VIEW Starterの差分となっているコマンドに対しては
`reveiw-starter-use-expansion` の値に応じて, 存在しないコマンドに
`review-starter-warning-face` (デフォルトは赤色)
が適応され, まちがいに気が付きやすいようになっています。

ユーザー独自のコマンドに対しては
`review-starter-block-names`
`review-starter-single-line-block-names`
`review-starter-inline-names`
を設定すればハイライトが可能です。カスタマイズ例を参照ください。

また, Re:VIEW Starterでは許されている入れ子のインライン命令をつかうと,
多少ハイライトが崩れてしまします.

## カスタマイズ例
```
(setq review-starter-use-expansion nil) ; Re:View Starter拡張を使わない.
(setq review-starter-block-names '("foo")) ; //foo{ ~ //} という新しいコマンドを追加する.
(setq review-starter-single-line-block-names '("bar")) //bar という新しいコマンドを追加する.
(setq review-starter-inline-names '("baz")) @<baz>{ ~ } という新しいコマンドを追加する.
(setq review-starter-role-name "監訳") ; コメントなどに入れる名前を「監訳」とする.
(setq review-starter-tip-name "監注") ; 注を入れる際の名称を「監注」とする.
(setq review-starter-use-whitespace-mode t) ; whitespace-modeを有効にする.
```

## 定義されているコマンドとキーバインド
まずは, コンパイル, ブロックの挿入, インラインタグの挿入の３つだけで十分便利だと思います.

- コンパイル : `C-c C-c` (`review-starter-compile`) ビルドを実行する.  初回実行時は
`review-starter-default-compile-command` (デフォルト値 "rake pdf")が呼ばれ,
2回め以降は前回実行時のコマンドが履歴に登録される.

- ブロックの挿入, 変更 :
  - `C-c C-e` (`review-starter-block`) 選択範囲をブロックで囲む.  選択されていない場合は新規に挿入する.
  - `C-u C-c C-e` (`review-starter-block`) 直前のブロックを変更する.
  - `C-c C-o` (`review-starter-insert-child-block`) 選択範囲をNest用のブロック(//beginchild ... //endchild) で囲む.  選択されていない場合は新規に挿入する.  Re:VIEW Staterでは使わないので`review-starter-use-starter-expansion`がnilのときだけキーにバンドされる.

- インラインタグの挿入 :
  - `C-c C-i` or `C-c C-f C-f` (`'review-starter-inline-region`) 選択範囲をインラインタグで囲む。選択されていない場合は新規に挿入する。

- フォント関連のインラインタグの挿入 : 選択範囲を囲む. 選択されていない場合は新規に挿入する.
  - `C-c C-f b` or `C-c C-f C-b` (`review-starter-bold-region`) 太字タグ(@\<b\>)で囲む
  - `C-c C-f k` or `C-c C-f C-k` (`review-starter-keyword-region`) キーワードタグ(@\<kw\>)で囲む
  - `C-c C-f i` or `C-c C-f C-i` (`review-starter-italic-region`) イタリックタグ(@\<i\>)で囲む
  - `C-c C-f e` or `C-c C-f C-e` (`review-starter-em-region`) 強調タグ（@\<em\>）で囲む
  - `C-c C-f s` or `C-c C-f C-s` (`review-starter-strong-region`) 強調タグ(@\<strong\>)で囲む
  - `C-c C-f u` or `C-c C-f C-u` (`review-starter-underline-region`) 下線タグ(@\<u\>)で囲む
  - `C-c C-f t` or `C-c C-f C-t` (`review-starter-tt-region`) 等幅タグ(@\<tt\>)で囲む
  - `C-c C-f a` or `C-c C-f C-a` (`review-starter-underline-italic-region`) 等幅イタリックタグ(@\<tti\>)で囲む
  - `C-c C-f c` or `C-c C-f C-c` (`review-starter-code-region`) コードタグ(@\<code\>)で囲む
  - `C-c C-f h` or `C-c C-f C-h` (`review-starter-hyperlink-region`) ハイパーリンクタグ(@\<href\>)で囲む

- 索引関連のインラインタグの挿入 :
   - `C-c C-f n` or `C-c C-f C-n` (`review-starter-index-region`) 出力付き索引化(@\<idx\>)する.
   - `C-c C-w` (`review-starter-insert-index`) 隠し索引(@\<hidx\>)を入れる.  Regionが選択されているか, それがインラインタグの中全てなのかを見て挙動が変わる。詳しくはdoc strigを参照せよ.

- Outline系の操作 :
  - `Tab` (`review-starter-cycle`) もし見出し行にいたら'hide all', 'headings only', 'show all'の順でcycleする.
  - `S-Tab` (`review-starter-cycle-buffer`) buffer全体を'hide all', 'headings only', 'show all'の順でcycleする.
  - `M-RET` (`review-starter-insert-heading`) カーソル位置と同レベルの見出しを入力する. 箇条書きの行にいたら箇条書きの次の項目を入力する.
  - `M-up` (`review-starter-move-subtree-up`) 今いる見出しレベルのツリーをと一つ前に移動する (同レベルの入れ替えのみ).
  - `M-down` (`review-starter-move-subtree-down`) 今いる見出しレベルのツリーを一つ後ろに移動する (同レベルの入れ替えのみ).
  - `M-left` (`review-starter-promote`) 見出し行や箇条書きの行にいたらそれを一つ上のレベルにする.
  - `M-right` (`review-starter-deomote`) 見出し行や箇条書きの行にいたらそれを一つ下のレベルにする.

- マーカーや作業に関するコマンド :
  - `C-c C-a` (`review-starter-normal-comment`)ユーザーから編集者へのメッセージ擬似マーカー.
  - `C-c C-k` (`review-starter-tip-comment`) ユーザー注釈の擬似マーカー.
  - `C-c C-d` (`review-starter-dtp-comment`) DTP担当へのメッセージ擬似マーカー.
  - `C-c C-r` (`review-starter-reference-comment`) 参照先をあとで確認する擬似マーカーを挿入する.
  - `C-c !`   (`review-starter-kokomade`) 作業途中の擬似マーカー.
  - `C-c C-t 1` (`review-starter-change-mode`) 作業者名の変更.
  - `C-c C-t 2` (`review-starter-change-dtp`) DTP担当の変更.

- 全角文字の挿入 :
  - `C-c (`   全角(
  - `C-c 8`   同上
  - `C-c )`   全角)
  - `C-c 9`   同上
  - `C-c [`   【
  - `C-c ]`    】
  - `C-c -`    全角ダーシ
  - `C-c +`    全角＋
  - `C-c *`    全角＊
  - `C-c /`    全角／
  - `C-c =`    全角＝
  - `C-c \`    ￥
  - `C-c SPC`  全角スペース
  - `C-c :`    全角：

- その他 :
  - `C-c C-m` (`review-starter-insert-br`) 強制改行タグの挿入.
  - `C-c C-p` (`review-starter-insert-header`) 見出し挿入.
  - `C-c C-b` (`review-starter-balloon-comment`) 吹き出しを入れる.
  - `C-c <` (`review-starter-opentag`) HTML開きタグを入れる.
  - `C-c >` (`review-starter-closetag`) HTML閉じタグを入れる.
  - `C-c 1` (`review-starter-search-uri`) 直近のURIを検索してブラウザを開く.  Regionが選択されているならその範囲の先頭からURIを探し, それをブラウザで開く.

- ほかにも下記のコマンドが定義されています. 詳しくはDocstringを参照ください.
  - `review-starter-show-version`
  - `review-starter-toggle-use-expansion`
  - `review-starter-index-change`
  - `review-starter-page-increment-region`
  - `review-starter-surround-tt`

## 今後の予定 (メモ.  時が来たらIssuesに書くかも.)
- Test codeを書く.
  - 難しそう.
- 変数や関数名の改善
  - prefixがreview-starter-は長すぎる気がする.  適切な省略形は？
  - tagよりはcmd (command)、op (operation)のほうがいいかも.
  - 末尾がregionで終わるコマンドはもう少し適切な名前がありそう.
- Syntax highlighの改善
  - インラインタグのネスト対応.
    - Font-lockの正規表現の枠組みでは難しい？
- Flycheck, Flymake
  - 今のSyntax highlightでそんなに困らないかも.
- `beginning-of-defun-function` と `end-of-defun-function` をちゃんと設定する.
  - `C-M-a`, `C-M-e`で章や節, blockの先頭と末尾に移動できるようにするのが良さそう.
- Indentation
  - 実はそんなに問題ないかも？
- Completion
  - company-modeとauto-completeのバックエンドを書く?
  - Eldoc的なものも出せると便利かも.
- org-mode.elを参考にした拡張.
  - コードブロックで `` C-c ` ``でその言語に対応したmajor-modeで編集できるようにしたい.

## ライセンス
GNU General Public License version 3 (LICENSE を参照してください)
