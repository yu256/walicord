# Button-First Discord Ledger UI

## 目的

経費記録の入口を slash command からボタンに寄せ、初回利用者がコマンド名を覚えなくても ledger UI を開始できるようにする。

既存の `/expense` PoC は modal / select menu / confirm button / canonical attachment append まで実装済みなので、処理本体は流用し、入口だけを button-first にする。

## 結論

Discord の制約上、任意の常設アプリ内ボタンをチャンネル UI に直接追加することはできない。ボタンは bot が投稿した message component として表示する必要がある。

そのため Walicord では、tracked channel に bot が「操作パネル」メッセージを投稿し、そこに以下のボタンを置く。

- `記録する`: 既存の `/expense` と同じ modal を開く
- `清算確認`: ledger PoC の review preview を ephemeral に表示する
- `台帳`: ledger summary を ephemeral に表示する
- `取り消し`: void 対象選択 UI を ephemeral に表示する

Slash command は管理者・上級者用の代替入口として残す。

## ユーザー体験

1. 管理者が `/panel` を実行する、または bot 起動時に tracked channel へ未設置なら投稿する。
2. bot がチャンネルに Walicord 操作パネルを投稿する。
3. ユーザーは `記録する` ボタンを押す。
4. bot は既存 `/expense` と同じ modal を開く。
5. modal 送信後は既存どおり、payer / participants / roles / MEMBERS / weights を選んで `記録する` で append する。

## Discord 制約への対応

- Button は message component なので、操作パネルメッセージが入口になる。
- Modal は command interaction だけでなく component interaction への response としても開ける。
- Button の custom id は再起動後も押されうるため、セッション依存の ID と固定 launcher ID を分ける。
- 操作パネルのボタン押下は公開メッセージを汚さないよう、以降の応答を ephemeral にする。
- 既存の draft state は process memory なので、入力中セッションは引き続き single-process runtime 前提にする。

## 実装方針

### 1. 固定 launcher component を追加する

`walicord/src/discord/ledger.rs` に固定 custom id を追加する。

```text
ledger:panel:expense
ledger:panel:review
ledger:panel:ledger
ledger:panel:void
```

既存の session custom id は `expense:new:{nonce}:{session}` のような nonce 付きなので、launcher と衝突しない。

### 2. slash command 専用の開始処理を分離する

現在の `start_expense(ctx, command)` は `CommandInteraction` に直接 modal response を返している。これを以下の形に分ける。

- `expense_modal(session_id) -> CreateModal`
- `start_expense_from_command(ctx, command)`
- `start_expense_from_component(ctx, component)`

component 版では `component.create_response(... Modal(modal))` を使う。modal の custom id は既存と同じ `EXPENSE_MODAL_PREFIX` を使うため、送信後の `complete_expense_modal` は変更しない。

### 3. パネル投稿コマンドを追加する

`/panel` を追加し、tracked channel でだけ利用可能にする。

パネルメッセージは bot message として投稿する。本文は最小限にし、重要なのは component。

初期実装では重複投稿を厳密に防がず、管理者が必要なチャンネルに置く運用でよい。次段階で message id を永続化するか、直近 bot message から custom id を検出して upsert する。

### 4. 既存 ledger PoC の処理を button から呼ぶ

`handle_component` の先頭で launcher custom id を判定する。

- `ledger:panel:expense` → `start_expense_from_component`
- `ledger:panel:review` → review command の中身を component 用に共通化して呼ぶ
- `ledger:panel:ledger` → ledger summary の中身を component 用に共通化して呼ぶ
- `ledger:panel:void` → void start の中身を component 用に共通化して呼ぶ

最初の実装単位は `記録する` だけでよい。review / ledger / void は同じ形で追える。

## 推奨する実装順

1. `記録する` launcher button と `/panel` を実装する。
2. `start_expense` を command/component 共通 modal builder に分離する。
3. `記録する` ボタンから既存 modal flow が完走するテストを追加する。
4. `清算確認` と `台帳` を component 起点にする。
5. `取り消し` を component 起点にする。
6. パネル重複投稿の upsert 戦略を決める。

## テスト観点

- 固定 launcher custom id が stale nonce 判定に巻き込まれない。
- `ledger:panel:expense` が session id を発行し、既存 `EXPENSE_MODAL_PREFIX` の modal を返す。
- modal 送信後は既存の `complete_expense_modal` に入り、draft が作られる。
- tracked channel 以外では `/panel` と launcher component が `CHANNEL_NOT_TRACKED` を返す。
- 再起動前に投稿された操作パネルの固定ボタンは、再起動後も押せる。

## 残す判断

コマンドを完全に消す必要はない。Discord ではコマンドが bot の発見性・権限管理・管理操作の入口として自然なので、`/panel` と既存 slash commands は残し、日常操作だけをボタン起点にする。
