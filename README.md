# review-starter-el
Major mode of Emacs for [Re:VIEW](https://reviewml.org/ja/)
and [Re:VIEW Starter](https://kauplan.org/reviewstarter/).

If `review-starter-use-expansion` is nil, this package supports Re:VIEW,
and if non-nil, this it supports Re:VIEW Starter.

It was mainly derived from [review-mode.el](https://github.com/kmuto/review-el/).

## シンタックスハイライトについて
現状, 存在しないブロック命令やインライン命令に対してもハイライトが行われてしまします.
また, Re:VIEW Starterでは許されている入れ子のインライン命令をつかうと,
ハイライトが崩れてしまします.

Re:VIEWとRe:VIEW Starterの差分となっている命令の幾つかに対しては
`reveiw-starter-use-expansion` がnilかnon-nilかに応じて,
それぞれの側に存在しない命令には
`review-starter-warning-face` (デフォルトは赤色)
が適応されるようになっています.  全ての差分に対応できているわけではありません.

## Usage
まずは, コンパイル, ブロックの挿入, インラインタグの挿入の３つだけで十分便利だと思います.

- コンパイル : `C-c C-c` (`review-starter-compile`) ビルドを実行する。初回実行時は
`review-starter-default-compile-command` (デフォルト値 "rake pdf")が呼ばれ,
2回め以降は前回実行時のコマンドが履歴に登録される.

- ブロックの挿入, 変更 :
  - `C-c C-e` (`review-starter-insert-block`) 選択範囲をブロックで囲む.  選択されていない場合は新規に挿入する.
  - `C-u C-c C-e` (`review-starter-insert-block`) 直前のブロックを変更する.
  - `C-c C-o` (`review-starter-insert-child-block`) 選択範囲をNest用のブロック(//beginchild ... //endchild) で囲む.  選択されていない場合は新規に挿入する.   Re:VIEW Staterでは使わないので`review-starter-use-starter-expansion`がnilのときだけキーにバンドされる.

- インラインタグの挿入 :
  - `C-c C-i` or `C-c C-f C-f` (`'review-starter-inline-region`) 選択範囲をインラインタグで囲む。選択されていない場合は新規に挿入する。

- フォント関連のインラインタグの挿入 : 選択範囲を囲む. 選択されていない場合は新規に挿入する.
  - `C-c C-f b` or `C-c C-f C-b` (`review-starter-bold-region`) 太字タグ(@\<b\>)で囲む
  - `C-c C-f k` or `C-c C-f C-k` (`review-starter-keyword-region`) キーワードタグ(@\<kw\>)で囲む
  - `C-c C-f i` or `C-c C-f C-i` (`review-starter-italic-region`) イタリックタグ(@\<i\>)で囲む
  - `C-c C-f e` or `C-c C-f C-e` (`review-starter-em-region`) 強調タグ（@\<em\>）で囲む
  - `C-c C-f s` or `C-c C-f C-s` (`review-starter-strong-region`) 強調タグ(@\<strong\>)で囲む
  - `C-c C-f u` or `C-c C-f C-u` (`review-starter-underline-region`) 下線タグ(@\<u\>)で囲む
  - `C-c C-f t` or `C-c C-f C-t` (`review-starter-em-region`) 等幅タグ(@\<tt\>)で囲む
  - `C-c C-f a` or `C-c C-f C-a` (`review-starter-underline-italic-region`) 等幅イタリックタグ(@\<tti\>)で囲む
  - `C-c C-f c` or `C-c C-f C-c` (`review-starter-code-region`) コードタグ(@\<code\>)で囲む
  - `C-c C-f h` or `C-c C-f C-h` (`review-starter-hyperlink-region`) ハイパーリンクタグ(@\<href\>)で囲む

- 索引関連のインラインタグの挿入 :
   - `C-c C-f n` or `C-c C-f C-n` (`review-starter-index-region`) 出力付き索引化(@\<idx\>)する.
   - `C-c C-w` (`review-starter-insert-index`) 隠し索引(@\<hidx\>)を入れる.  Regionが選択されているか, それがインラインタグの中全てなのかを見て挙動が変わる。詳しくはdoc strigを参照せよ.

- マーカーや作業に関するコマンド :
  - `C-c C-a` ユーザーから編集者へのメッセージ擬似マーカー.
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
  - `C-c C-p` (`review-starter-insert-header`) 見出し挿入.
  - `C-c C-b` (`review-starter-balloon-comment`) 吹き出しを入れる.
  - `C-c <` (`review-starter-opentag`) HTML開きタグを入れる.
  - `C-c >` (`review-starter-closetag`) HTML閉じタグを入れる.
  - `C-c 1` (`review-starter-search-uri`) 直近のURIを検索してブラウザを開く.  Regionが選択されているならその範囲の先頭からURIを探し, それをブラウザで開く.


## カスタマイズの例
```
(setq review-starter-use-expansion nil) ; Re:View Starter拡張を使わない.
(setq review-starter-role-name "監訳") ; コメントなどに入れる名前を「監訳」とする.
(setq review-starter-tip-name "監注") ; 注を入れる際の名称を「監注」とする.
(setq review-starter-use-whitespace-mode t) ; whitespace-modeを有効にする.
```

## 今後の予定
- インラインタグのネスト対応
  - 正規表現では難しい？
- 存在しないブロック命令に警告色をつけたい.
  - User定義のブロック命令はつけないようにしないといけない.
- org-mode.elを参考にした拡張をしたい.
  - `M-RET` (`review-starter-meta-return`) でいい感じに箇条書きや見出しの入力できるようにしたい.
    - これを導入したら`C-c C-p`を廃止したい.
  - `M-Right`, `M-Left`で箇条書きや見出しのレベルを変更できるようにしたい.
  - コードブロックで `` C-c ` ``でその言語に対応したmajor-modeで編集できるようにしたい.

## ライセンス
GNU General Public License version 3 (LICENSE を参照してください)
