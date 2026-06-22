use std::{
    collections::HashMap,
    sync::Mutex,
    time::{Duration, SystemTime},
};

use walicord_domain::model::MemberId;
use walicord_ledger::LedgerId;

use crate::ports::InteractionNonce;

/// Read-view sessions expire after this long so stale paginated views do not block
/// the actor from reopening `/ledger` / `/review` with fresh data.
pub const READ_VIEW_SESSION_TTL: Duration = Duration::from_secs(15 * 60);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReadViewSessionKey {
    pub ledger_id: LedgerId,
    pub actor_id: MemberId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadViewSession<Page> {
    key: ReadViewSessionKey,
    nonce: InteractionNonce,
    pages: Vec<Page>,
    current_page: usize,
    last_touched: SystemTime,
}

impl<Page> ReadViewSession<Page> {
    pub fn new(
        key: ReadViewSessionKey,
        nonce: InteractionNonce,
        pages: Vec<Page>,
        now: SystemTime,
    ) -> Self {
        debug_assert!(
            !pages.is_empty(),
            "read view session must hold at least one page"
        );
        Self {
            key,
            nonce,
            pages,
            current_page: 0,
            last_touched: now,
        }
    }

    pub fn key(&self) -> ReadViewSessionKey {
        self.key
    }

    pub fn nonce(&self) -> InteractionNonce {
        self.nonce
    }

    pub fn page_count(&self) -> usize {
        self.pages.len()
    }

    pub fn current_index(&self) -> usize {
        self.current_page
    }

    pub fn current_page(&self) -> &Page {
        &self.pages[self.current_page]
    }

    pub fn advance(&mut self, now: SystemTime) -> bool {
        if self.current_page + 1 >= self.pages.len() {
            return false;
        }
        self.current_page += 1;
        self.last_touched = now;
        true
    }

    pub fn retreat(&mut self, now: SystemTime) -> bool {
        if self.current_page == 0 {
            return false;
        }
        self.current_page -= 1;
        self.last_touched = now;
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum ReadViewSessionAccessError {
    #[error("read view session expired")]
    Expired,
    #[error("read view session has been superseded")]
    Superseded {
        actual: InteractionNonce,
        expected: InteractionNonce,
    },
}

pub struct ReadViewSessionStore<Page> {
    by_key: Mutex<HashMap<ReadViewSessionKey, ReadViewSession<Page>>>,
}

impl<Page: Clone> Default for ReadViewSessionStore<Page> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Page> ReadViewSessionStore<Page>
where
    Page: Clone,
{
    pub fn new() -> Self {
        Self {
            by_key: Mutex::new(HashMap::new()),
        }
    }

    pub fn replace(&self, session: ReadViewSession<Page>) -> Option<ReadViewSession<Page>> {
        self.by_key
            .lock()
            .expect("ReadViewSessionStore mutex poisoned")
            .insert(session.key(), session)
    }

    pub fn peek(&self, key: ReadViewSessionKey, now: SystemTime) -> Option<Page> {
        let mut guard = self
            .by_key
            .lock()
            .expect("ReadViewSessionStore mutex poisoned");
        let session = guard.get(&key)?;
        let elapsed = now.duration_since(session.last_touched).unwrap_or_default();
        if elapsed >= READ_VIEW_SESSION_TTL {
            guard.remove(&key);
            return None;
        }
        Some(session.current_page().clone())
    }

    pub fn clear(&self, key: ReadViewSessionKey) -> Option<ReadViewSession<Page>> {
        self.by_key
            .lock()
            .expect("ReadViewSessionStore mutex poisoned")
            .remove(&key)
    }

    pub fn clear_ledger(&self, ledger_id: LedgerId) {
        self.by_key
            .lock()
            .expect("ReadViewSessionStore mutex poisoned")
            .retain(|key, _| key.ledger_id != ledger_id);
    }

    pub fn access(
        &self,
        key: ReadViewSessionKey,
        observed_nonce: InteractionNonce,
        now: SystemTime,
    ) -> Result<Option<ReadViewSession<Page>>, ReadViewSessionAccessError> {
        let mut guard = self
            .by_key
            .lock()
            .expect("ReadViewSessionStore mutex poisoned");
        let Some(session) = guard.get(&key).cloned() else {
            return Ok(None);
        };
        let elapsed = now.duration_since(session.last_touched).unwrap_or_default();
        if elapsed >= READ_VIEW_SESSION_TTL {
            guard.remove(&key);
            return Err(ReadViewSessionAccessError::Expired);
        }
        if session.nonce != observed_nonce {
            return Err(ReadViewSessionAccessError::Superseded {
                actual: observed_nonce,
                expected: session.nonce,
            });
        }
        Ok(Some(session))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key() -> ReadViewSessionKey {
        ReadViewSessionKey {
            ledger_id: walicord_ledger::test_fixtures::ledger_id(7),
            actor_id: MemberId(11),
        }
    }

    fn nonce(value: u64) -> InteractionNonce {
        InteractionNonce::new(value).expect("non-zero nonce")
    }

    #[test]
    fn advance_and_retreat_walk_pages_within_bounds() {
        let mut session =
            ReadViewSession::new(key(), nonce(1), vec!["a", "b", "c"], SystemTime::UNIX_EPOCH);
        assert_eq!(*session.current_page(), "a");
        assert!(session.advance(SystemTime::UNIX_EPOCH));
        assert_eq!(*session.current_page(), "b");
        assert!(session.advance(SystemTime::UNIX_EPOCH));
        assert_eq!(*session.current_page(), "c");
        assert!(!session.advance(SystemTime::UNIX_EPOCH));
        assert!(session.retreat(SystemTime::UNIX_EPOCH));
        assert_eq!(*session.current_page(), "b");
        assert!(session.retreat(SystemTime::UNIX_EPOCH));
        assert_eq!(*session.current_page(), "a");
        assert!(!session.retreat(SystemTime::UNIX_EPOCH));
    }

    #[test]
    fn access_returns_session_when_nonce_matches_and_within_ttl() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        store.replace(ReadViewSession::new(
            key(),
            nonce(1),
            vec!["page"],
            SystemTime::UNIX_EPOCH,
        ));
        let actual = store
            .access(key(), nonce(1), SystemTime::UNIX_EPOCH)
            .expect("access should not fail");
        assert!(actual.is_some());
    }

    #[test]
    fn access_rejects_superseded_nonce() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        store.replace(ReadViewSession::new(
            key(),
            nonce(1),
            vec!["page"],
            SystemTime::UNIX_EPOCH,
        ));
        let actual = store.access(key(), nonce(2), SystemTime::UNIX_EPOCH);
        assert!(matches!(
            actual,
            Err(ReadViewSessionAccessError::Superseded { .. })
        ));
    }

    #[test]
    fn access_expires_session_past_ttl_and_clears_it() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        store.replace(ReadViewSession::new(
            key(),
            nonce(1),
            vec!["page"],
            SystemTime::UNIX_EPOCH,
        ));
        let later = SystemTime::UNIX_EPOCH + READ_VIEW_SESSION_TTL;
        let actual = store.access(key(), nonce(1), later);
        assert!(matches!(actual, Err(ReadViewSessionAccessError::Expired)));
        let after_expiry = store.access(key(), nonce(1), SystemTime::UNIX_EPOCH);
        assert!(matches!(after_expiry, Ok(None)));
    }

    #[test]
    fn peek_returns_current_page_without_consuming_session() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        store.replace(ReadViewSession::new(
            key(),
            nonce(1),
            vec!["page"],
            SystemTime::UNIX_EPOCH,
        ));

        let peeked = store.peek(key(), SystemTime::UNIX_EPOCH);
        assert_eq!(peeked, Some("page"));

        let still_there = store
            .access(key(), nonce(1), SystemTime::UNIX_EPOCH)
            .expect("session should still exist after peek");
        assert!(still_there.is_some());
    }

    #[test]
    fn peek_returns_none_for_expired_session_and_removes_it() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        store.replace(ReadViewSession::new(
            key(),
            nonce(1),
            vec!["page"],
            SystemTime::UNIX_EPOCH,
        ));

        let later = SystemTime::UNIX_EPOCH + READ_VIEW_SESSION_TTL;
        assert_eq!(store.peek(key(), later), None);
        assert_eq!(store.clear(key()), None);
    }

    #[test]
    fn peek_returns_none_for_missing_session() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        assert_eq!(store.peek(key(), SystemTime::UNIX_EPOCH), None);
    }

    #[test]
    fn clear_ledger_keeps_sessions_for_other_ledgers() {
        let store: ReadViewSessionStore<&'static str> = ReadViewSessionStore::new();
        let retained_key = ReadViewSessionKey {
            ledger_id: walicord_ledger::test_fixtures::ledger_id(8),
            actor_id: MemberId(11),
        };
        store.replace(ReadViewSession::new(
            key(),
            nonce(1),
            vec!["removed"],
            SystemTime::UNIX_EPOCH,
        ));
        store.replace(ReadViewSession::new(
            retained_key,
            nonce(2),
            vec!["retained"],
            SystemTime::UNIX_EPOCH,
        ));

        store.clear_ledger(walicord_ledger::test_fixtures::ledger_id(7));

        assert_eq!(store.clear(key()), None);
        assert_eq!(
            store
                .clear(retained_key)
                .map(|session| session.current_page),
            Some(0)
        );
    }
}
