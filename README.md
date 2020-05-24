# log-analyze
## What's About

アクセスログを解析するプログラムです。
以下の形式のログファイルを読み込み、解析します。

```
%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"
```

それぞれの項目の意味は以下のとおりです。
- %h    リモートホスト名
- %l    クライアントの識別子
- %u    認証ユーザー名
- %t    リクエストを受信した時刻([day/month/year:hour:minute:second zone]の書式)
- %r    リクエストの最初の行
- %>s   最後のレスポンスのステータス
- %b    HTTPヘッダを除くレスポンスのバイト数。0バイトの場合は「-」と表示される
- %{Referer}i    サーバが受信したリクエストヘッダのReferer
- %{User-Agent}i サーバが受信したリクエストヘッダのUser-Agent

時間帯ごとのアクセス数、ホストごとのアクセス数を集計し、標準出力に結果を出力します。

### 例

```
10.2.3.4 - - [18/Apr/2005:00:10:47 +0900] "GET / HTTP/1.1" 200 854 "-" "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)"
10.2.3.4 - - [18/Apr/2005:00:10:47 +0900] "GET /style.css HTTP/1.1" 200 102 "http://www.geekpage.jp/" "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)"
10.2.3.4 - - [18/Apr/2005:00:10:47 +0900] "GET /img/title.png HTTP/1.1" 304 - "http://www.geekpage.jp/" "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)"
```

## Compile
GHC 8.4.4 をインストールしてあるコンピュータで以下のコマンドを打つことでコンパイルできます。

```
ghc -o log-analyze Main.hs
```

## How to Use
次の形式で実行できます。

```
$ log-analyze FILES [-d BEGIN END]
```

## 実行例

```
$ log-analyze test/small-test
Hour	 | Access
------------------------------
9	 | 3
12	 | 3



10.2.3.4 : 6
```
