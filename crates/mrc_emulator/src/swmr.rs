use std::cell::{Cell, UnsafeCell};
use std::ops::{Deref, DerefMut};
use std::thread::ThreadId;

#[derive(Copy, Clone)]
enum State {
    Unlocked,
    Locked(ThreadId),
}

/// # Single writer; multiple readers (SWMR)
///
/// The inner value can be read from multiple thread, but only be written to from a single thread.
/// Reading and writing happens without any synchronization.  This is useful if you don't care that
/// some object is in an undefined state during reads.
pub struct Swmr<T> {
    inner: UnsafeCell<T>,
    state: Cell<State>,
}

unsafe impl<T> Send for Swmr<T> {}
unsafe impl<T> Sync for Swmr<T> {}

impl<T> Swmr<T> {
    pub fn new(t: T) -> Self {
        Self {
            inner: UnsafeCell::new(t),
            state: Cell::new(State::Unlocked),
        }
    }

    pub fn borrow(&self) -> SwmrRef<T> {
        SwmrRef { swmr: self }
    }

    pub fn borrow_mut(&self) -> SwmrRefMut<T> {
        let thread_id = std::thread::current().id();

        match self.state.get() {
            State::Unlocked => {
                self.state.set(State::Locked(thread_id));
                SwmrRefMut { swmr: self }
            }

            State::Locked(thread_id) => panic!("Already locked to thread {:?}.", thread_id),
        }
    }
}

pub struct SwmrRef<'a, T> {
    swmr: &'a Swmr<T>,
}

impl<'a, T> Deref for SwmrRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.swmr.inner.get() }
    }
}

pub struct SwmrRefMut<'a, T> {
    swmr: &'a Swmr<T>,
}

impl<'a, T> Deref for SwmrRefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.swmr.inner.get() }
    }
}

impl<'a, T> DerefMut for SwmrRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.swmr.inner.get() }
    }
}

impl<'a, T> Drop for SwmrRefMut<'a, T> {
    fn drop(&mut self) {
        // If we are dropping a mutable reference, then we MUST have been locked before.
        assert!(matches!(self.swmr.state.get(), State::Locked(_)));

        self.swmr.state.set(State::Unlocked);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn basic() {
        let swmr = Swmr::new(10);

        println!("test");
        assert_eq!(10, *swmr.borrow());
        *swmr.borrow_mut().unwrap() = 20;
        assert_eq!(20, *swmr.borrow());
    }

    #[test]
    fn can_read_from_multiple_threads() {
        let value = Arc::new(Swmr::new(10));
        let mr = &mut *value.borrow_mut().unwrap();

        let clone = value.clone();
        let handle = std::thread::spawn(move || {
            assert_eq!(10, *clone.borrow());
        });

        assert_eq!(10, *mr);

        handle.join().unwrap();
    }

    #[test]
    fn can_not_write_from_multiple_threads() {
        let value = Arc::new(Swmr::new(10));
        let mr = &mut *value.borrow_mut().unwrap();

        let clone = value.clone();
        let handle = std::thread::spawn(move || {
            assert!(clone.borrow_mut().is_err());
        });

        assert_eq!(10, *mr);

        handle.join().unwrap();
    }
}
