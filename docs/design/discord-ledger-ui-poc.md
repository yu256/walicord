# Discord Ledger UI PoC

## PoC の目的

Discord からの UI 操作で ledger event をそのまま append し、ledger channel から hash chain を検証して replay できる経路を通す。技術的な綺麗さより、ユーザーから見える `/expense` → `/review` → `/settle` → `/void` → `/ledger` の振る舞いを先に成立させる。

## 実現するユーザー体験

- `/expense` で modal から金額・メモ・日付を入力する
- payer / participants / roles / built-in `MEMBERS` group / weights を順に選んで確認する
- 「記録する」で ledger thread に人間可読な台帳メッセージを append する
- `/review` で ledger thread から current balances と清算案を表示する
- `/settle` で直前 review の transfer を確定し、固定 transfer を append する
- `/void` で最近の ledger entry 候補から対象を選んで append-only で取り消す
- `/ledger` で participants / balances / void / sealed through を確認する

## Discord UI の制約

- Modal は表編集の代わりではないので、自由入力は amount / note / date のみに寄せる
- 参加者・payer・void target は select menu / button で段階的に選ぶ
- ledger thread 内の `/review` は ephemeral に preview を表示し、legacy `/review` は既存の公開挙動を保つ
- `/void` は ephemeral message 上の confirm button で進める
- 人間向け本文や embed 文言は presentation のみで、復元には使わない
- draft / preview などの interaction state は process memory に置くので、この PoC は single-process deployment を前提にする

## コマンドごとの方針

### `/expense`

- Slash command で modal を開く
- modal 送信後、ephemeral UI で payer / participants / role / built-in `MEMBERS` group / weights を選ぶ
- 入力値から直接 `ExpenseRecorded` と `LedgerEntry` を構築する
- `LedgerEntryMetadata` には `recorded_by` / `source` / `allocation_snapshot` / `effective_date` を入れる
- role / built-in `MEMBERS` group は append 時点で resolved member + weight に展開する

### `/review`

- ledger thread の canonical attachment を load する
- hash chain を検証する
- append order を検証する
- `LedgerProjector` で replay する
- current balances から settlement preview を作り、確認用に保持する

### `/settle`

- `/review` で保持した preview を ledger head binding 付きで再確認する
- confirm 時は preview で見せた transfer そのものを `NormalizedSettlementPlanRecorded` として append する
- replay 時に optimizer は再実行しない

### `/void`

- recent ledger entry を候補表示する
- confirm 後は `EntryVoided` を append する
- 元メッセージの編集 / 削除は state 変更に使わない

### `/ledger`

- projected ledger state を表示する
- participants / balances / voided entries / sealed through を確認できるようにする

## ledger channel への保存形式

- source of truth は dedicated ledger thread に append される bot message
- 表示本文は人間可読な経費 / 清算 / void / state 表示
- canonical data は message attachment (`walicord-ledger-entry.json`) に入れる
- attachment の中身は versioned Discord transport DTO と application-owned ledger envelope を持つ

## canonical data の保存場所

- machine-readable canonical data は JSON attachment に保存する
- message 本文や embed は display cache であり、decode の入力にはしない

## hash chain の扱い

- append 前に current ledger tail を load して head hash を得る
- append する entry は application-owned schema v1 + `sha256_v1` で hash を作る
- load 時は attachment から envelope を decode し、`sha256_v1` で verify / replay する

## load / replay の流れ

1. ledger thread lookup / create
2. bot-authored ledger messages から attachment DTO を load
3. DTO → `UnverifiedLedgerStoreEnvelope<MessageId>`
4. hash chain verify
5. append-order validation
6. replay
7. presentation / interaction state 更新

## 既存 Script 経路との関係

- 既存 message + DSL path はそのまま残す
- 新 slash-ledger path は legacy history を source of truth に混ぜない
- 既存 `/review` 相当の script flow は残す
- ledger PoC の `/review` は ledger thread 内で実行し、legacy `/review` を横取りしない
- PoC の ledger slash commands は ledger thread だけを source of truth として読む

## 今回の整理で固定したこと

- hash chain は speculative な v2 を持たず、deployed target である schema v1 + `sha256_v1` に統一する
- `effective_date` も schema v1 の canonical bytes に含め、Discord path だけ別 schema に逃がさない
- weight UI と canonical attachment roundtrip は回帰テストで固定し、表示変更が source of truth に影響しないようにする
- runtime 起動時に cross-process instance lock を取得し、interaction state が分散しないよう single-process を強制する
