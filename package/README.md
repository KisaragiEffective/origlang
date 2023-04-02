各ディレクトリはそれぞれ次の責務を持ちます。

* `origlang-ast` - AST (構文木) の構造のモデリング
* `origlang-cli` - OS上におけるツールチェインのフロントエンド
* `origlang-compiler` - レキサー、パーサー、型チェック
* `origlang-interop` - `origlang-compiler` をWebAssemblyで動かすためのグルーコード
* `origlang-interop-frontend-webserver` - `origlang-interop` のフロントエンド
* `origlang-runtime` - スコープや中間コードなど、実行時の情報の格納

より詳細な解説は各ディレクトリを参照してください。
