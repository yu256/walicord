# セットアップガイド

Walicordを使用するためのセットアップ手順です。

## 前提条件

- Discordアカウント
- Discordサーバー（管理者権限が必要）

## セットアップ手順

### オプション1: バイナリを直接ダウンロード（推奨）

最も簡単な方法です。

1. **ダウンロード**: [Releases](https://github.com/yu256/walicord/releases) から最新のバイナリをダウンロードしてください。
2. **設定**: `.env`ファイルを作成し、Discordボットトークンを設定します（[環境設定](#環境設定)を参照）。
3. **実行**: バイナリを実行します。

### オプション2: ソースからビルド

Rustの開発環境がある場合の手順です。

1. **Rustのインストール**: [rustup.rs](https://rustup.rs/) からRustをインストールします。
2. **クローン**: リポジトリをクローンします。
   ```sh
   git clone https://github.com/yu256/walicord.git
   cd walicord
   ```
3. **設定**: `.env`ファイルを作成し、必要な設定を行います。
4. **実行**:

   Walicord は HiGHS ソルバーを標準で使用します。言語を feature フラグで指定して実行します：

   ```sh
   # 日本語（デフォルト）
   cargo run --release --features ja

   # 英語
   cargo run --release --no-default-features --features en

   ```

## Discordボットの作成

1. [Discord Developer Portal](https://discord.com/developers/applications)にアクセスし、**New Application**をクリックして新しいアプリケーションを作成します。
2. 左メニューの**Bot**を選択し、**Add Bot**をクリックします。
3. **Privileged Gateway Intents**セクションで以下の設定を有効にします（必須）：
   - **Message Content Intent**
   - **Server Members Intent**
4. トークンをコピーして保存しておきます（後で`.env`ファイルに使用します）。

### ボットの招待

1. 左メニューの**OAuth2 > URL Generator**を選択します。
2. **Scopes**で`bot`にチェックを入れます。
3. **Bot Permissions**で以下の権限にチェックを入れます：
   - `Read Message History`
   - `Send Messages`
   - `Add Reactions`
   - `Attach Files` (画像送信に必要)
4. 生成されたURLをブラウザで開き、ボットをあなたのサーバーに招待します。

## 環境設定

プロジェクトのルートディレクトリに`.env`という名前のファイルを作成し、以下の内容を記述します：

```env
DISCORD_TOKEN="YOUR_DISCORD_BOT_TOKEN_HERE"
```

- `DISCORD_TOKEN`: Developer Portalで取得したボットのトークン

### チャンネルトピックの設定
ボットを有効にしたいチャンネルのトピックに `#walicord` を含めてください。

- 例: `会計ログ #walicord`
- 無効化したい場合はトピックから `#walicord` を削除します。

**注意事項:**
- トピックが設定できるチャンネル（テキストチャンネル）のみが対象です。スレッドやトピックのないチャンネルは有効化できません。
- `#walicord` が含まれていれば有効化されます（部分一致）。例: `#walicord-dev` でも一致します。
- ボットがチャンネル一覧の取得とメッセージ履歴の読み取り権限を持っている必要があります。
- 起動時に、参加しているサーバーの全テキストチャンネルをスキャンして `#walicord` 付きのチャンネルを自動的に有効化します。

## 言語設定

ビルド時のfeatureフラグで言語を切り替えられます（デフォルトは日本語）。

- **英語のみ**: `cargo run --release --features en`
- **日本語のみ**: `cargo run --release --features ja`

## Unix系OSでの注意点

画像生成（残高表など）のために`fontconfig`が必要です。また、日本語を正しく表示するために`Noto Sans CJK JP`などのフォントのインストールを推奨します。

```sh
# Ubuntu/Debianの例
sudo apt-get install libfontconfig1 fontconfig
sudo apt-get install fonts-noto-cjk
```
