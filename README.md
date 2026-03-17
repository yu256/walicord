# Walicord

共有経費を追跡し、グループメンバー間の最適な債務決済を計算する Discord ボットです。

## 機能概要

- **経費記録**: `1000 to @Hanako` のように自然な入力で記録
- **重み付け割り勘**: `3000 @A*2 @B*0 @C` のようにメンバーごとに異なる割合で分担
- **最適化清算**: 複数の貸し借りを相殺し、最小限の送金回数で清算
- **残高管理**: 誰が誰にいくら貸しているかを常に追跡し、画像で表示

## ドキュメント

- **[セットアップガイド](docs/setup.md)**: インストール、環境設定、ボットの作成方法
- **[ユーザーガイド](docs/usage.md)**: 経費の入力方法、清算コマンドの使い方

## 開発者向け

Rust のワークスペース構成です。

- `walicord`: エントリーポイント
- `walicord-presentation`: Discord とのインタラクション
- `walicord-application`: ユースケース
- `walicord-domain`: コアロジック
- `walicord-infrastructure`: 永続化
- `walicord-parser`: 経費入力のパーサー
- `walicord-transfer-construction`: 送金構築エンジン
- `walicord-i18n`: 多言語対応

設計ドキュメント: [割り勘計算の数学的仕様](docs/design/settlement-mathematics.md) / [送金構築ロジック](docs/design/transaction-construction.md)
