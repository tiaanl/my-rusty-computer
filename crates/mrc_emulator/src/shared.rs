use std::cell::UnsafeCell;

pub struct Shared<T: ?Sized> {
    inner: UnsafeCell<T>,
}

impl<T> Shared<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner: UnsafeCell::new(inner),
        }
    }

    pub fn read(&self) -> &T {
        unsafe { &*self.inner.get() }
    }

    pub fn write(&self) -> &mut T {
        unsafe { &mut *self.inner.get() }
    }
}

unsafe impl<T: ?Sized + Send> Send for Shared<T> {}
unsafe impl<T: ?Sized + Send> Sync for Shared<T> {}
