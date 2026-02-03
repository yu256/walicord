# walicord

Discord上で共有経費を追跡し、グループメンバー間の最適な債務決済を計算するDiscordボットです。

## 機能

- **経費記録**: カスタムDSL（英語/日本語対応）を使用した経費の記録
- **メンバー管理**: Discordチャンネルのトピックからメンバーを定義
- **残高計算**: 記録された全取引に基づく各メンバーの純残高の計算
- **債務決済の最適化**: 線形計画法を使用した決済取引の最小化
- **変数表示**: `!variables` コマンドで定義されたグループとそのメンバーを表示
- **複数チャンネル対応**: 複数のDiscordチャンネルで同時に動作可能
- **決済コマンド**: `!settleup` / `!確定` で特定メンバー集合の決済を即時反映
- **画像出力**: 残高表と決済プランをPNGで返信

## 技術スタック

- **Rust**: メインプログラミング言語
- **serenity**: Discord APIクライアント
- **tokio**: 非同期ランタイム
- **nom**: DSLパーサー
- **good_lp**: 混合整数線形計画問題ソルバー
- **dashmap**: 並行HashMap
- **tracing**: 構造化ログ
- **indexmap**: 順序保持HashMap
- **resvg / tiny-skia**: SVGからPNGへのレンダリング
- **dotenvy**: 環境変数の読み込み
- **thiserror**: エラーハンドリング

## プロジェクト構成

このプロジェクトは9つのクレートで構成されるRustワークスペースです:

- **walicord**: Discord APIとの連携を担当するメインのDiscordボットアプリケーション
- **walicord-application**: メッセージ処理やユースケースをまとめるアプリケーション層
- **walicord-domain**: 残高計算や決済ロジックのドメインモデル
- **walicord-parser**: DSLパーサーライブラリ
- **walicord-calc**: 債務決済最適化ライブラリ
- **walicord-infrastructure**: パーサーと最適化ソルバーのアダプタ
- **walicord-presentation**: 残高表と決済表のSVG/テキスト出力
- **walicord-i18n**: 表示メッセージのローカライズ
- **walicord-interpreter**: `.walicord`スクリプトを実行するためのコマンドラインツール

## `walicord-interpreter`

`walicord-interpreter`は、Discordを介さずに`walicord`のコアロジックをテストしたり、スクリプトとして実行したりするためのコマンドラインツールです。結果はSVGテキストとして標準出力に出力されます。

### 使い方

```sh
cargo run --package walicord-interpreter -- <file.walicord>
```

### `.walicord` ファイルフォーマット

- ファイルの1行目には、`MEMBERS`宣言を記述する必要があります。
- 2行目以降には、グループ宣言や支払い記録など、ボットが通常解釈するのと同じ構文のステートメントを記述できます。

#### 例: `example.walicord`

```
MEMBERS := A, B, C

group1 := A, B

A lent 1000 to B
C lent 500 to group1

!evaluate
```

## セットアップ

1. Rustをインストール
2. `.env`ファイルにDiscordボットトークンと対象チャンネルIDを設定:
   ```
   DISCORD_TOKEN="YOUR_DISCORD_BOT_TOKEN"
   TARGET_CHANNEL_IDS="CHANNEL_ID_1,CHANNEL_ID_2" # カンマ区切りで複数指定可能
   ```
3. `cargo run --release`で実行

### 言語切り替え（features）

デフォルトは日本語です。英語のみを使いたい場合は`en`を指定します。

```sh
cargo run --release --features en
```

日本語のみを明示する場合は`ja`を指定します。

```sh
cargo run --release --features ja
```

`walicord-interpreter`も同じfeaturesを使用できます。

```sh
cargo run --package walicord-interpreter --features en -- <file.walicord>
```

### Unix系での依存関係

PNG出力のため、Unix系環境では`fontconfig`が必要になることがあります。日本語表示を安定させたい場合は`Noto Sans CJK JP`の導入を推奨します。

## 使い方

1. Discordチャンネルのトピックにメンバーを定義:
   ```
   MEMBERS := Alice, Bob, Carol
   ```
2. チャンネルで経費を記録:
   ```
   Alice lent 100 to Bob
   Alice が Bob に 100 貸した
   ```
3. `!variables` コマンドで現在のグループとメンバーの定義を確認
4. `!evaluate` コマンドで決済プランを計算
5. `!settleup` または `!確定` コマンドで特定のメンバー集合の決済計算:
   ```
   !settleup MEMBERS - Alice
   !確定 MEMBERS - Bob
   ```

## 構文について

### MEMBERSの宣言

チャンネルのトピックで、参加メンバーを宣言する必要があります:

```
MEMBERS := name1, name2, name3
```

### 文の種類

DSLには3種類のステートメントがあります:

### 1. グループ宣言

メンバーのグループを定義します:

```
group_name := member1, member2
```

- `group_name`: グループの識別子
- `member1, member2`: グループに含まれるメンバー(カンマ区切り)

### 2. 支払い記録

支払いを記録します。4つの構文パターンがサポートされています:

#### パターン1: 日本語(貸し手主語)

```
{payer} が {payee} に {amount} 貸した
```

#### パターン2: 英語(貸し手主語)

```
{payer} lent {amount} to {payee}
```

#### パターン3: 日本語(借り手主語)

```
{payee} が {payer} から {amount} 借りた
```

#### パターン4: 英語(借り手主語)

```
{payee} borrowed {amount} from {payer}
```

### 3. コマンド

```
!variables
!evaluate
!settleup MEMBERS - Alice
```

- `!variables`: グループとメンバー定義の一覧を表示
- `!evaluate`: 現在までの記録を元に決済プランを計算
- `!settleup` / `!確定`: 指定メンバー集合の決済を即時反映し、その結果を表示

### 識別子のルール

識別子(メンバー名、グループ名)には以下の文字が使用できます:

- 英数字
- アンダースコア(`_`)
- ハイフン(`-`)
- 日本語文字(ひらがな、カタカナ、漢字)

### 金額の表記

金額は以下の形式で記述できます:

- `1000` (数値のみ)
- `¥1000` (円記号付き)
- `1000円` (「円」付き)
- `1000えん` (ひらがな)
- `1000yen` (英語)

### セット演算子

グループ宣言や支払い記録の`{payer}`や`{payee}`には、以下のセット演算子を使った複雑な式も記述できます。

- **和集合**: `A ∪ B`, `A, B`, `A，B` (カンマは全角・半角対応)
- **積集合**: `A ∩ B`
- **差集合**: `A - B`
- **括弧**: `(A ∪ B) ∩ C`

### キーワードのバリエーション

DSLは日本語と英語の両方をサポートしています:

| 意味   | 日本語             | 英語                |
|--------|--------------------|---------------------|
| 借りた | かりた、借りた     | borrowed, BORROWED  |
| から   | から               | from, FROM          |
| 貸した | 貸した、かした     | lent, LENT          |
| に     | に                 | to, TO              |

### バリデーションルール

パーサーは以下のバリデーションを実行します:

1. **未定義メンバーチェック**: 宣言されていないメンバーやグループが使用された場合、`UndefinedMember`エラーが発生します
2. **構文エラー**: パース失敗時は`SyntaxError`が発生します
