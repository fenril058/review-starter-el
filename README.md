# review-starter-el
Major mode of Emacs for Re:VIEW and  Re:VIEW Starter.

It is mainly derived from [review-mode.el](https://github.com/kmuto/review-el/).
This package also supports Re:VIEW, if `review-starter-use-expansion` is nil.

## Usage
まずは, コンパイル, ブロックの挿入, インラインタグの挿入の３つだけで十分便利だと思います.

- コンパイル : `C-c C-c` (`review-starter-compile`) ビルドを実行する。初回実行時は
`review-starter-default-compile-command` (デフォルト値 "rake pdf")が呼
ばれ2回め以降は前回実行時のコマンドが履歴に登録される.

- ブロックの挿入, 変更 :
  - `C-c C-e` (`review-starter-insert-block`) 選択範囲をブロックで囲む.  選択されていない場合は新規に挿入する.
  - `C-u C-c C-e` (`review-starter-insert-block`) 直前のブロックを変更する.
  - `C-c C-o` (`review-starter-insert-child-block`) 選択範囲をNest用のブロック(//beginchild ... //endchild) で囲む.  選択されていない場合は新規に挿入する.   Re:VIEW Staterでは使わないので`review-starter-use-starter-expansion`がnilのときだけキーにバンドされる.

- インラインタグの挿入 :
  - `C-c C-i` or `C-c C-f C-f` (`'review-starter-inline-region`) 選択範囲をインラインタグで囲む。選択されていない場合は新規に挿入する。

- フォント関連のインラインタグの挿入 :
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
   - `C-c C-w` (`review-starter-insert-index`) 隠し索引(@\<hidx\>)を入れる.  Regionが選択されているか, インラインタグの中にいるかで挙動がいい感じに変わる.

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
  - `M-RET` (`review-starter-meta-return`) 見出し挿入とか箇条書きとかの挿入 (未実装.  org-modeを参考に実装予定).
  - `C-c C-p` (`review-starter-insert-header`) 見出し挿入 (廃止予定)
  - `C-c C-b` (`review-starter-balloon-comment`) 吹き出しを入れる.
  - `C-c <` (`review-starter-opentag`) rawのHTML開きタグを入れる.
  - `C-c >` (`review-starter-closetag`) rawのHTML閉じタグを入れる.
  - `C-c 1` (`review-starter-search-uri`) 直近のURIを検索してブラウザを開く.  Regionが選択されているならその範囲の先頭からURIを探し、それをブラウザで開く.


## カスタマイズの例
```
(setq review-starter-use-expansion nil) ; Re:View Starter拡張を使わない.
(setq review-starter-role-name "著者") ; コメントなどに入れる名前を「著者」とする.
(setq review-starter-tip-name "注") ; 注を入れる際の名称を「注」とする.
(setq review-starter-use-whitespace-mode t) ; whitespace-modeを有効にする.
```

## 今後の予定
- Debug
  - ブロックとインラインタグのネスト対応 (現状Font-lockが崩れるのでそれを直す).

- Enhance
  - org-mode.elを参考にした拡張をしたい.
    - `M-RET`でいい感じに箇条書きや見出しの入力できるようにしたい.
    - `M-Right`, `M-Left`で箇条書きや見出しのレベルを変更できるようにしたい.
    - コードブロックで`C-c `でその言語に対応したmajor-modeで編集できるようにしたい.

## ライセンス
GNU General Public License version 3 (LICENSE を参照してください)
