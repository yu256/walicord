#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("vector must not be empty")]
pub struct EmptyVecError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmptyVec<T> {
    inner: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn new(inner: Vec<T>) -> Result<Self, EmptyVecError> {
        if inner.is_empty() {
            return Err(EmptyVecError);
        }
        Ok(Self { inner })
    }

    pub fn singleton(value: T) -> Self {
        Self { inner: vec![value] }
    }

    pub fn from_first_and_rest(first: T, rest: impl IntoIterator<Item = T>) -> Self {
        let mut inner = vec![first];
        inner.extend(rest);
        Self { inner }
    }

    pub fn pair(first: T, second: T) -> Self {
        Self::from_first_and_rest(first, [second])
    }

    pub fn first(&self) -> &T {
        &self.inner[0]
    }

    pub fn into_vec(self) -> Vec<T> {
        self.inner
    }
}

impl<T> std::ops::Deref for NonEmptyVec<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.inner
    }
}

impl<T: PartialEq> PartialEq<[T]> for NonEmptyVec<T> {
    fn eq(&self, other: &[T]) -> bool {
        self.inner == other
    }
}

impl<T: PartialEq, const N: usize> PartialEq<[T; N]> for NonEmptyVec<T> {
    fn eq(&self, other: &[T; N]) -> bool {
        self.inner.as_slice() == other
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyVec<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}
