各ディレクトリはそれぞれ次の責務を持ちます。

* `origlang-ast` - 構文木のモデリング
* `origlang-cli` - OS上におけるツールチェインのフロントエンド
* `origlang-compiler-entrypoint` - コンパイラの入口
* `origlang-compiler-scanner-example` - 診断機構の活用例
* `origlang-diagnostics` - リンターなどに活用できる診断機構
* `origlang-interop` - WebAssemblyで動かすためのグルーコード
* `origlang-interop-frontend-webserver` - `origlang-interop` のフロントエンド
* `origlang-ir` - 中間表現の定義
* `origlang-ir-optimizer` - 中間表現の最適化機構
* `origlang-lexer` - 字句解析器
* `origlang-parser` - パーサー
* `origlang-platform` - プラットフォーム依存の情報の抽象化
* `origlang-runtime` - スコープや中間コードなど、実行時の情報の格納
* `origlang-source-span` - ソースコード上の場所の提供
* `origlang-testsuite` - テストのための内部バイナリ
* `origlang-token-stream` - 字句解析器から出力されたトークンをコレクションとして管理する
* `origlang-typecheck` - 型チェック
* `origlang-typesystem-model` - 型システムのモデル

より詳細な解説は各ディレクトリを参照してください。
