use parking_lot_core::{self, ParkResult, SpinWait, UnparkResult, UnparkToken, DEFAULT_PARK_TOKEN};

use std::mem;
use std::sync::atomic::{fence, AtomicU32, Ordering};
use std::thread;

pub struct EntryMutex<'a>(pub &'a AtomicU32);

// UnparkToken used to indicate that that the target thread should attempt to
// lock the mutex again as soon as it is unparked.
const TOKEN_NORMAL: UnparkToken = UnparkToken(0);

// UnparkToken used to indicate that the mutex is being handed off to the target
// thread directly without unlocking it.
const TOKEN_HANDOFF: UnparkToken = UnparkToken(1);

/// This bit is set in the `state` of a `RawMutex` when that mutex is locked by some thread.
const LOCKED_BIT: u32 = 0b01;
/// This bit is set in the `state` of a `RawMutex` just before parking a thread. A thread is being
/// parked if it wants to lock the mutex, but it is currently being held by some other thread.
const PARKED_BIT: u32 = 0b10;

const MUTEX_BITS: u32 = 0b11;
const WRITE_INCREMENT: u32 = 0b100;

impl<'a> EntryMutex<'a> {
    pub fn lock(&self) -> LockGuard {
        let mut spinwait = SpinWait::new();
        let mut state = self.0.load(Ordering::Relaxed);
        loop {
            // Grab the lock if it isn't locked, even if there is a queue on it
            if state & LOCKED_BIT == 0 {
                match self.0.compare_exchange_weak(
                    state,
                    state | LOCKED_BIT,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => break,
                    Err(x) => state = x,
                }
                continue;
            }

            // If there is no queue, try spinning a few times
            if state & PARKED_BIT == 0 && spinwait.spin() {
                state = self.0.load(Ordering::Relaxed);
                continue;
            }

            // Set the parked bit
            if state & PARKED_BIT == 0 {
                if let Err(x) = self.0.compare_exchange_weak(
                    state,
                    state | PARKED_BIT,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                ) {
                    state = x;
                    continue;
                }
            }

            // Park our thread until we are woken up by an unlock
            let addr = self.0 as *const _ as usize;
            let validate =
                || (self.0.load(Ordering::Relaxed) & MUTEX_BITS) == LOCKED_BIT | PARKED_BIT;
            let before_sleep = || {};
            let timed_out = |_, _| {};
            // SAFETY:
            //   * `addr` is an address we control.
            //   * `validate`/`timed_out` does not panic or call into any function of `parking_lot`.
            //   * `before_sleep` does not call `park`, nor does it panic.
            match unsafe {
                parking_lot_core::park(
                    addr,
                    validate,
                    before_sleep,
                    timed_out,
                    DEFAULT_PARK_TOKEN,
                    None,
                )
            } {
                // The thread that unparked us passed the lock on to us
                // directly without unlocking it.
                ParkResult::Unparked(TOKEN_HANDOFF) => break,

                // We were unparked normally, try acquiring the lock again
                ParkResult::Unparked(_) => (),

                // The validation function failed, try locking again
                ParkResult::Invalid => (),

                // Timeout expired
                ParkResult::TimedOut => unreachable!(),
            }

            // Loop back and try locking again
            spinwait.reset();
            state = self.0.load(Ordering::Relaxed);
        }
        LockGuard(self.0)
    }

    pub fn read<T, F: Fn() -> T>(&self, f: F) -> T {
        loop {
            let seq1 = self.0.load(Ordering::Acquire);
            if seq1 & LOCKED_BIT == LOCKED_BIT {
                // Currently writing, bail
                thread::yield_now();
                continue;
            }

            let e = f();

            fence(Ordering::Acquire);
            let seq2 = self.0.load(Ordering::Relaxed);
            if seq1 != seq2 {
                continue;
            }
            return e;
        }
    }
}

pub struct LockGuard<'a>(&'a AtomicU32);

impl<'a> LockGuard<'a> {
    #[allow(dead_code)]
    pub fn abort(self) {
        self.unlock();
        mem::forget(self);
    }

    fn unlock(&self) {
        // Unpark one thread and leave the parked bit set if there might
        // still be parked threads on this address.
        let addr = self.0 as *const _ as usize;
        let callback = |result: UnparkResult| {
            // If we are using a fair unlock then we should keep the
            // mutex locked and hand it off to the unparked thread.
            if result.unparked_threads != 0 && result.be_fair {
                // Clear the parked bit if there are no more parked
                // threads.
                if !result.have_more_threads {
                    self.0.fetch_and(!PARKED_BIT, Ordering::Relaxed);
                }
                return TOKEN_HANDOFF;
            }

            // Clear the locked bit, and the parked bit as well if there
            // are no more parked threads.
            if result.have_more_threads {
                self.0.fetch_and(!LOCKED_BIT, Ordering::Relaxed);
            } else {
                self.0
                    .fetch_and(!(LOCKED_BIT | PARKED_BIT), Ordering::Relaxed);
            }
            TOKEN_NORMAL
        };
        // SAFETY:
        //   * `addr` is an address we control.
        //   * `callback` does not panic or call into any function of `parking_lot`.
        unsafe {
            parking_lot_core::unpark_one(addr, callback);
        }
    }
}

impl<'a> Drop for LockGuard<'a> {
    fn drop(&mut self) {
        self.0.fetch_add(WRITE_INCREMENT, Ordering::Relaxed);
        fence(Ordering::Release);
        self.unlock();
    }
}
