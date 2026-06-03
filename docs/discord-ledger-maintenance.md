# Discord 台帳保守 runbook

`discord_ledger_maintenance` は Discord 台帳の保守要求を監査可能な JSON 成果物として発行します。
通常の bot プロセスを停止し、単一 writer の静止時間帯を確保した状態でのみ実行してください。

## Readiness

```sh
export WALICORD_LEDGER_MAINTENANCE_RUNBOOK="$PWD/docs/discord-ledger-maintenance.md"
export WALICORD_LEDGER_MAINTENANCE_ARTIFACT_DIR="$PWD/var/discord-ledger-maintenance"

cargo run --bin discord_ledger_maintenance -- \
  --confirm-quiesced-single-writer-window readiness
```

readiness は runbook、成果物ディレクトリ、静止時間帯の明示確認を検査します。要求発行時には bot と
同じ runtime instance lock も取得するため、bot が動作中なら fail closed します。

## 要求発行

```sh
cargo run --bin discord_ledger_maintenance -- \
  --confirm-quiesced-single-writer-window ordinary-seal LEDGER_ID
```

利用可能な操作:

- `older-than-twenty-void LEDGER_ID TARGET_ENTRY_ID ACTOR_ID REPLACEMENT_ACTOR_ID`
- `duplicate-thread-resolution LEDGER_ID KEEP_THREAD_ID RETIRED_THREAD_IDS_COMMA_SEPARATED`
- `damaged-thread-replacement RETIRED_LEDGER_ID NEW_PARENT_CHANNEL_ID`
- `ordinary-seal LEDGER_ID`
- `sealed-entry-correction LEDGER_ID TARGET_ENTRY_ID`
- `prior-adjustment-correction LEDGER_ID TARGET_ENTRY_ID`

成果物の `status` は `operator_action_required` です。現段階では canonical thread を直接変更しません。
成果物を保管し、対象 thread、verified history、置換内容を確認してから Discord 側の保守手順を実施してください。
