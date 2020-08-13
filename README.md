# 適応型ハフマン符号ライブラリ

大学のある科目の自由課題として取り組んだものです。FGKアルゴリズムによって文章を適応型ハフマン符号化します。

Rust実行環境は入っているものとし、本リポジトリに移動後次のコマンドで文章のバイナリ圧縮、復号化を実際に試すことができます。\
バイナリファイルはバイナリエディタ等で確認することをおすすめします。

(以下テキストを用意するのにPythonを使っていますがこのライブラリに必要なわけではありません。)

```bash
$ python -c "import this" > zen.txt
$ cargo run zen.txt
    Finished dev [unoptimized + debuginfo] target(s) in 0.02s
     Running `target/debug/adap-haff zen.txt`
$ cargo run zen.txt.hufb -d
    Finished dev [unoptimized + debuginfo] target(s) in 0.02s
     Running `target/debug/adap-haff zen.txt.hufb -d`
$ ls -l zen.txt*
-rw-r--r-- 1 o1810156 y2018 857  8月 13 09:56 zen.txt
-rw-r--r-- 1 o1810156 y2018 539  8月 13 10:05 zen.txt.hufb
-rw-r--r-- 1 o1810156 y2018 857  8月 13 10:06 zen.txt.hufb.dec
```

857バイトが539バイトまで圧縮されている様子が確認できます。62.9%ほどの圧縮率です。

またアニメーションでハフマン木が構成される様子を観察することもできます。

![適応型ハフマン木アニメーション](./huffman2.gif)

## オプション

|オプション|    |    説明    |
|:-------|:---|:-----------|
|-d|--decode|デコードモード。エンコードされた.hufbファイルを復号します。|
|-v|--verbose|饒舌モード。ハフマン木を出力します。|
|-a [ms]|--animation [ms]|アニメーションモード。饒舌モードをアニメーションにします。スピードをミリ秒で設定でき、デフォルトは500msです。|
|-h|--help|ヘルプの表示|

## 使用したバージョン

```bash
$ cargo --version
cargo 1.40.0 (bc8e4c8be 2019-11-22)
$ rustc --version
rustc 1.40.0 (73528e339 2019-12-16)
```
