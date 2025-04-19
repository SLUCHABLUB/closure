# `closure` - A Macro for Individually Capturing Variables

<!-- TODO: update the shields -->
[![Latest version](https://img.shields.io/crates/v/closure.svg)](https://crates.io/crates/closure)
[![Documentation](https://docs.rs/closure/badge.svg)](https://docs.rs/closure)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/oliver-giersch/closure)

> This is a fork of the [closure](https://crates.io/crates/closure) crate
> by [Oliver Giersch](https://github.com/oliver-giersch) which is no longer maintained.

This crate provides a macro which lets you write closures that can capture variables individually,
either by moving, referencing, or transforming by a method.

## Usage

Start by adding an entry to your `Cargo.toml`:

```toml
[dependencies]
closure = "0.3.0"
```

Then you can write closures like so:

```rust
use closure::closure;

let string = "move".to_string();
let x = 10;
let mut y = 20;
let rc = Rc::new(5);

let closure = closure!([move string, ref x, ref mut y, clone rc] move |arg: i32| {
    ...
});
```
