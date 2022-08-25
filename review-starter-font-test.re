= Font-lockの動作確認用のファイル.

このファイルで @<code>{M-x review-starter-mode} を実行することで, Font-lockの動作を確認できる.

== Re:VIEW
//lead{
Re:VIEW確認用. 例文は
@<href>{https://github.com/kmuto/review/blob/master/doc/format.ja.md}
などから適当に持ってきた。
//}

#@# (review-starter-toggle-use-expansion)

=== 箇条書き

 * 箇条書き1
 ** 箇条書き2
 *** 箇条書き3

 1. 箇条書き1
 2. 箇条書き
 3. 箇条書き

 : Alpha
   alpha
 : Beta
   beta

==== 入れ子の箇条書き

 * UL1

//beginchild
#@# ここからUL1の子

 1. UL1-OL1

//beginchild
#@# ここからUL1-OL1の子

UL1-OL1-PARAGRAPH

 * UL1-OL1-UL1
 * UL1-OL1-UL2

//endchild
#@# ここまでUL1-OL1の子

 2. UL1-OL2

 : UL1-DL1
        UL1-DD1
 : UL1-DL2
        UL1-DD2

//endchild
#@# ここまでUL1の子

 * UL2

===[notoc] ソースコードブロック

//list[mainc][{キャプション\}][c]{
main(){
    printf("hello world")
	}
//}
ただし途中に"}"があってもFont-lockが崩れたりはしないため気づきにくい.

//firstlinenum[100]
//listnum[mainrust][キャプション][rust]{
fn main() {
    println!("Hello, world!");
}
//}

//emlist[キャプション][python]{
print("hello world")
//}

//emlistnum[キャプション][ruby]{
print "hello world"
//}

====[column] ソースコードなどのリスト

//source[/hello/world.rb]{
puts "hello world!" # キャプションあり
//}

//source{
puts "hello world!" # キャプションなし
//}

//source[/hello/world.rb][ruby]{
puts "hello world!" # キャプションあり、ハイライトあり
//}

//source[][ruby]{
puts "hello world!" # キャプションなし、ハイライトあり
//}

====[/column]

==== 数式ブロック

//texequation{
y = ax + b
//}

=== インライン命令

==== フォントなど

 * @<kw>{HHH}, @<kw>{キーワード, 補足}, @<kw>{信任状, credential}
 * @<bou>{傍点が付きます。}
 * @<ami>{文字に対して網がかかります。}
 * @<u>{underline}
 * @<b>{太字} : 太字にします。
 * @<i>{斜体} : イタリックにします。
 * @<strong>{STRONG} : 強調（太字）にします。
 * @<em>{EMPHASIZE} : 強調にします。
 * @<tt>{mono, 等幅} : 等幅にします。
 * @<tti>{等幅＋イタリック, mono + Italic} : にします。
 * @<ttb>{mono + bold} : 等幅＋太字にします。
 * @<code>{nodePaht.get('body.0.expression.callee')} : 等幅にします（コードの引用という性質）。
 * @<tcy>{tategaki} : 縦書きの文書において文字を縦中横にします。
 * @<ins>{insert} : 挿入箇所を明示します（デフォルトでは下線が引かれます）。
 * @<del>{uchikeshi} : 削除箇所を明示します（デフォルトでは打ち消し線が引かれます）。
 * @<ruby>{親文字,ルビ} : ルビを振ります。たとえば @<ruby>{愕然,がくぜん} のように表記します。
 * @<br>{} : 段落途中で改行します。濫用は避けたいところですが、表のセル内や箇条書き内などで必要になることもあります。
 * @<uchar>{番号} : Unicode文字を出力します。引数は16進数で指定します。


==== 参照

 * @<chap>{章ファイル名}
 * @<title>{章ファイル名}
 * @<chapref>{章ファイル名}
 * @<list>{識別子}
 * @<img>{識別子}
 * @<table>{識別子}
 * @<eq>{識別子}
 * @<hd>{ラベルまたは見出し}
 * @<column>{ラベルまたは見出し}
 * @<fn>{脚注1}

//footnote[脚注1][@<m>$f(x)$について]

==== 索引

 * @<idx>{文字列}
 * @<hidx>{文字列}

親索引文字列<<>>子索引文字列 (未対応)

==== その他

 * @<icon>{識別子}
 * @<m>{数式}, @<m>$数式$, @<m>|数式|
 * @<w>{キー}
 * @<wb>{キー}
 * @<raw>{|ビルダ|〜}
 * @<embed>{|ビルダ|〜}
 * @<balloon>{hoge}

== Re:VIEW Starter

//abstract{
Starter 拡張の確認.
//}

#@# (review-starter-toggle-use-expansion)

=== New list

 - A. First
 -- B. Second

=== New Tag

 * @<LaTeX>{}
 * @<B>{ゴシック体の太字}
 * @<term>{NEW WORD}
 * @@<nop>{}<B>{テキスト}

=== Nest Inline tag

 * @<ruby>{@<b>{foo}}
 * @<b>{@<i>{bold + italic}}
 * @<i>{@<b>{italic + bold}}
 * @<ami>{@<b>{網掛け + 太字}}
 * @<code>{foo@<b>{bar}} : code中の太字など.

現状うまくハイライトできていない例.
@<aaa>{xxx}という命令がxxxにもfaceをappend設定するケースの場合,
@<aaa>{@<bbb>{xxx}}と入れ子にすると,
最初の綴じカッコで両方の命令が閉じる判定されてしまうためおかしなことになる.
正規表現の限界かもしれない.

=== New Block & Nest Block
//vspace[6zw]

//note[][@<m>$g(x)$のノート]{

//list{
//}
//}

//info[][]{
//texequation{
\exp(i\theta) = \sin(\theta) + i\cos(\theta)
//}
//}
