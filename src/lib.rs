//! A fork of the [closure](https://crates.io/crates/closure) crate, with more IDE-friendly syntax.
//! 
//! # The [`closure`] Macro
//! 
//! A macro for capturing variables on a per variable basis.
//!
//! With the [`closure`] macro, it is possible to specify how each variable should be captured.
//! Variables can be either moved, referenced, mutably referenced or transformed using a method
//! (e.g. `clone`).
//! By default, variables will be captured according to what type of underlying closure is used.
//! To use `move` or "by-method" capturing, a `move` closure is needed.
//!
//! The syntax for each capture type is as follows:
//! - `move var` moves `var` into the closure
//! - `ref var` borrows `var`
//! - `ref mut var` mutably borrows `var`
//! - `$IDENT var` transforms `var` where $IDENT is any identifier for a method with receiver and no further arguments
//!
//! ## Capturing by Value
//!
//! To capture a variable by moving it into the closure,
//! use `move` (or `move mut` to create a mutable binding):
//!
//! ```
//! # use closure::closure;
//! let first = "first".to_owned();
//! let second = "second".to_owned();
//!
//! let closure = closure!([move first, move mut second] move || {
//!     // creates an immutable `first` and a mutable `second`
//!     // binding...
//!     # assert_eq!(first, "first");
//!     # second.clear();
//!     # assert_eq!(second, "");
//! });
//! ```
//!
//! ## Capturing by Reference
//!
//! To capture a variable by borrowing it in the closure,
//! use `ref` (or `ref mut` for a mutable borrow):
//!
//! ```
//! # use closure::closure;
//! let mut a = 1;
//! let b = 0;
//!
//! let mut closure = closure!([ref mut a, ref b] move || {
//!     *a = 0;
//!     assert_eq!(*a, *b);
//! });
//! # closure();
//! ```
//!
//! ## Capturing Paths
//! 
//! It is also possible to capture simple paths, such as members or methods of a `struct`:
//!
//! ```
//! # use closure::closure;
//! struct Foo {
//!     bar: i32,
//! }
//!
//! impl Foo {
//!     fn print(&self) {
//!         // here a binding `let bar = &self.bar` will be
//!         // created for the closure
//!         closure!([ref self.bar] move || println!("{}", bar))();
//!     }
//! }
//! ```
//!
//! With `move` captures, the usual rules for destructuring apply.
//!
//! ## Capturing the Result of a Method
//!
//! Capturing a variable by passing it to a method,
//! creates a binding of the same name but containing the methods return value.
//! The most common use case for this type of capture is probably calling [`clone`](Clone::clone) on a variable.
//! However, as long as the method has a receiver (`self`, `&self`, `&mut self`, &.c.), it can be used.
//! Examples include: `to_string`, `to_owned` and `into_iter`.
//!
//! ```
//! # use closure::closure;
//! let first = "first".to_string();
//! let second = "second".to_string();
//!
//! let mut closure = closure!([clone first, clone mut second] move || {
//!     // creates two bindings `first` and `second`,
//!     // the latter is mutable.
//!     println!("cloned: {}", first);
//!     second.clear();
//!     # assert_eq!(second, "");
//! });
//!
//! closure();
//! println!("the original {} and {} were not moved", first, second);
//! ```
//!
//! ## Examples
//!
//! ### Spawning a Thread
//!
//! Instead of having to write:
//!
//! ```
//! use std::thread;
//! use std::sync::{Arc, Barrier, Mutex};
//!
//! let mutex = Arc::new(Mutex::new(Vec::new()));
//! let barrier = Arc::new(Barrier::new(2));
//!
//! let vector_clone = Arc::clone(&mutex);
//! let barrier_clone = Arc::clone(&barrier);
//!
//! thread::spawn(move || {
//!     let mut vec = vector_clone.lock().unwrap();
//!     vec.push(2);
//!     vec.push(3);
//!     vec.push(4);
//!
//!     barrier_clone.wait();
//! });
//!
//! barrier.wait();
//! let mut vec = mutex.lock().unwrap();
//!
//! vec.push(1);
//! assert_eq!(*vec, &[2, 3, 4, 1]);
//! ```
//!
//! Using [`closure`] it is possible to avoid manually creating bindings for each cloned `Arc`:
//!
//! ```
//! use std::thread;
//! use std::sync::{Arc, Barrier, Mutex};
//!
//! use closure::closure;
//!
//! let mutex = Arc::new(Mutex::new(Vec::new()));
//! let barrier = Arc::new(Barrier::new(2));
//!
//! thread::spawn(closure!([clone mutex, clone barrier] move || {
//!     let mut vec = mutex.lock().unwrap();
//!     vec.push(2);
//!     vec.push(3);
//!     vec.push(4);
//!
//!     barrier.wait();
//! }));
//!
//! barrier.wait();
//! let mut vec = mutex.lock().unwrap();
//!
//! vec.push(1);
//! assert_eq!(*vec, &[2, 3, 4, 1]);
//! ```
//!
//! ### Moving Cloned Smart Pointers Into Thread Closures
//!
//! From the documentation of [`Condvar`][std::sync::Condvar]:
//!
//! ```
//! use std::sync::{Arc, Mutex, Condvar};
//! use std::thread;
//!
//! let pair = Arc::new((Mutex::new(false), Condvar::new()));
//! let pair2 = pair.clone();
//!
//! // Inside of our lock, spawn a new thread, and then wait for it to start.
//! thread::spawn(move|| {
//!     let &(ref lock, ref cvar) = &*pair2;
//!     let mut started = lock.lock().unwrap();
//!     *started = true;
//!     // We notify the condvar that the value has changed.
//!     cvar.notify_one();
//! });
//!
//! // Wait for the thread to start up.
//! let &(ref lock, ref cvar) = &*pair;
//! let mut started = lock.lock().unwrap();
//! while !*started {
//!     started = cvar.wait(started).unwrap();
//! }
//! ```
//!
//! With [`closure`], the explicit declaration of `pair2` can be avoided:
//!
//! ```
//! use std::sync::{Arc, Mutex, Condvar};
//! use std::thread;
//!
//! use closure::closure;
//!
//! let pair = Arc::new((Mutex::new(false), Condvar::new()));
//!
//! // Inside our lock, spawn a new thread, and then wait for it to start.
//! thread::spawn(closure!([clone pair] move || {
//!     let &(ref lock, ref cvar) = &*pair;
//!     let mut started = lock.lock().unwrap();
//!     *started = true;
//!     // We notify the condvar that the value has changed.
//!     cvar.notify_one();
//! }));
//!
//! // Wait for the thread to start up.
//! let &(ref lock, ref cvar) = &*pair;
//! let mut started = lock.lock().unwrap();
//! while !*started {
//!     started = cvar.wait(started).unwrap();
//! }
//! ```
//!
//! ### Using Mixed Capture Modes 
//!
//! ```
//! # use closure::closure;
//! let move_string = "this string will be moved".to_string();
//! let mut ref_string = "this string will be borrowed".to_string();
//!
//! let mut closure = closure!([move move_string, ref mut ref_string] move || {
//!     ref_string.push_str(&move_string);
//!     // `move_string` is dropped at the end of the scope
//! });
//!
//! # closure();
//! # assert_eq!(
//! #    ref_string,
//! #    concat!("this string will be borrowed", "this string will be moved")
//! # );
//! ```

/// A macro that allows specifying how variables should be captured.
///
/// See the [crate-level](crate) docs for information on syntax and examples.
#[macro_export(local_inner_macros)]
macro_rules! closure {
    ([move $($ids:ident).+ $(, $($tail:tt)*)?] $closure:expr) => {{
        let $crate::__extract_last_ident!($($ids).+) = $($ids).+;
        closure!([$($($tail)*)?] $closure)
    }};
    ([move mut $($ids:ident).+ $(, $($tail:tt)*)?] $closure:expr) => {{
        let $crate::__extract_last_ident!(mut $($ids).+) = $($ids).+;
        closure!([$($($tail)*)?] $closure)
    }};
    ([ref $($ids:ident).+ $(, $($tail:tt)*)?] $closure:expr) => {{
        let $crate::__extract_last_ident!($($ids).+) = & $($ids).+;
        closure!([$($($tail)*)?] $closure)
    }};
    ([ref mut $($ids:ident).+ $(, $($tail:tt)*)?] $closure:expr) => {{
        let $crate::__extract_last_ident!($($ids).+) = &mut $($ids).+;
        closure!([$($($tail)*)?] $closure)
    }};
    ([$fn:ident $($ids:ident).+ $(, $($tail:tt)*)?] $closure:expr) => {{
        let $crate::__extract_last_ident!($($ids).+) = $($ids).+.$fn();
        closure!([$($($tail)*)?] $closure)
    }};
    ([$fn:ident mut $($ids:ident).+ $(, $($tail:tt)*)?] $closure:expr) => {{
        let $crate::__extract_last_ident!(mut $($ids).+) = $($ids).+.$fn();
        closure!([$($($tail)*)?] $closure)
    }};
    ([] $closure:expr) => {
        $closure
    };

    // allow other types of brackets
    (($($args:tt)*) $closure:expr) => {
        closure!([$($args)*] $closure)
    };
    ({$($args:tt)*} $closure:expr) => {
        closure!([$($args)*] $closure)
    };
    
    // allow passing no capture list
    ($closure:expr) => { $closure };
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __extract_last_ident {
    ($last:ident) => { $last };
    (mut $last:ident) => { mut $last };
    ($ignore:ident.$($tail:ident).+) => { $crate::__extract_last_ident!($($tail).+) };
    (mut $ignore:ident.$($tail:ident).+) => { $crate::__extract_last_ident!(mut $($tail).+) };
}

#[cfg(test)]
mod test {
    use crate::closure;

    struct Foo {
        bar: Bar,
    }

    #[derive(PartialEq, Eq)]
    struct Bar {
        baz: i32,
    }

    impl Foo {
        fn new(baz: i32) -> Self {
            Foo { bar: Bar { baz } }
        }

        fn consume(self) -> Box<dyn Fn(i32) -> bool> {
            Box::new(closure!([move self.bar.baz] move |expected| baz == expected))
        }

        fn borrow(&self) -> Box<dyn Fn(i32) -> bool + '_> {
            Box::new(closure!([ref self.bar.baz] move |expected| *baz == expected))
        }
    }

    #[test]
    fn no_capture_one_line() {
        let closure = closure!(|| 5 * 5);
        assert_eq!(closure(), 25);
    }

    #[test]
    fn no_capture_with_arg() {
        let closure = closure!(|x| x * x);
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_arg_and_type_hint() {
        let closure = closure!(|x: usize| x * x);
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_arg_and_return_type() {
        let closure = closure!(|x: usize| -> usize { x * x });
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_return_type() {
        let closure = closure!(|| -> &str { "result" });
        assert_eq!(closure(), "result");
    }

    #[test]
    fn capture_by_move() {
        let string = "move".to_string();
        let closure = closure!([move string] move || string.len());
        assert_eq!(closure(), 4);
    }

    #[test]
    fn capture_by_ref() {
        let var = -1;
        let closure = closure!([ref var] move || *var == -1);
        assert!(closure());
    }

    #[test]
    fn capture_by_ref_mut() {
        let mut var = -1;
        closure!([ref mut var] move || *var *= -1)();
        assert_eq!(var, 1);
    }

    #[test]
    fn capture_nested_by_move() {
        let foo = Foo::new(-1);
        let closure = closure!([move foo.bar] move || bar == Bar { baz: -1 });
        assert!(closure());
    }

    #[test]
    fn capture_nested_by_ref() {
        let foo = Foo::new(-1);
        let closure = closure!([ref foo.bar] move || *bar == Bar { baz: -1 });
        assert!(closure());
    }

    #[test]
    fn capture_nested_by_ref_mut() {
        let mut foo = Foo::new(-1);
        closure!([ref mut foo.bar.baz] move |add| *baz += add)(2);
        assert_eq!(foo.bar.baz, 1);
    }

    #[test]
    fn capture_nested_with_self_by_move() {
        let foo = Foo::new(-1);
        let closure = foo.consume();
        assert!(closure(-1));
    }

    #[test]
    fn capture_nested_with_self_by_ref() {
        let foo = Foo::new(-1);
        let closure = foo.borrow();
        assert!(closure(-1));
    }

    #[test]
    fn capture_multiple_mixed() {
        let borrow = 1;
        let mut borrow_mut = 1;
        let string = "move".to_string();

        let closure = closure!([ref borrow, ref mut borrow_mut, move mut string] move || {
            assert_eq!(*borrow, 1);
            *borrow_mut -= 1;
            string.push_str("d back");
            string
        });

        assert_eq!(&closure(), "moved back");
    }

    #[test]
    fn capture_by_clone() {
        use std::rc::Rc;

        let rc = Rc::new(Foo::new(0));
        let closure = closure!([clone rc] move |expected| -> bool {
            rc.bar.baz == expected && Rc::strong_count(&rc) == 2
        });
        assert!(closure(0));
    }

    #[test]
    fn capture_by_fn_ident() {
        let string = "string";
        let closure = closure!([to_string string] || {
            let mut owned: String = string;
            owned.push_str(", now owned");
            owned
        });

        assert_eq!(closure(), "string, now owned");
    }
}
