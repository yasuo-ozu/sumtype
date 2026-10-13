# `sumtype` audit

> Historical record. It predates the announce port: `sumtype::traits::*` is now `::announce_std::*`
> and `#[sumtrait]` is now `#[announce]` with `sumtype::check` (see the README).

Audit of the `sumtype` / `sumtype-macro` crates (rustc 1.90 stable, edition 2021). Every finding
below was reproduced against the actual compiler; the confirming command or diagnostic is quoted.
Most findings have since been **fixed** — see the Status column and the
[Fixes applied](#fixes-applied) section. Remaining items are documented limitations.

Baseline before the audit: `cargo build` clean, `cargo test` green (13 integration tests + 12
doctests). After: `cargo test --workspace` green (33 integration tests + 18 doctests), `cargo
clippy --workspace --all-targets` clean, `cargo doc` warning-free.

## Summary

| # | Severity | Kind | Finding | Status |
|---|----------|------|---------|--------|
| 1 | High | bug | Bare `Hash` supertrait → nonexistent `sumtype::traits::Hash` (E0433) | **Fixed** — `traits::Hash` now defined and supported |
| 2 | High | bug | Bare `Eq` supertrait → nonexistent `sumtype::traits::Eq` (E0433) | **Fixed** — rejected with clear error |
| 3 | High | bug | `PartialEq` supertrait silently dropped (dead feature) → enum has no `PartialEq` (E0277) | **Fixed** — rejected with clear error; dead plumbing removed |
| 4 | Med | bug | Non-`#[sumtrait]` supertrait path invoked as a macro → "expected macro" | **Improved** — built-in std/core traits recognized bare + qualified; only foreign plain traits remain a limitation |
| 5 | High | bug | Const generics broken: `PhantomData<>` in `__Uninhabited` (E0107) | **Fixed** — now works |
| 6 | High | bug | `for<…>` repeating the enclosing free-fn generics duplicates names (E0403) | **Fixed** — now works |
| 7 | Med | error-quality | `#[sumtype()]` (empty bounds) leaks `__Sumtype_Enum_<random>` (E0277) | **Fixed** — clear error |
| 8 | Low | bug | Nested `sumtype!(sumtype!(…))` leaves inner macro unexpanded | Documented limitation (meaningless usage) |
| 9 | Med | error-quality | `#[sumtrait]` missing `marker` **panics** instead of `abort!` | **Fixed** — clean error |
| 10 | High | bug+CI | `sumtype-macro` doctests broken **and** silently skipped by `cargo test` | **Fixed** — examples removed; CI runs `--workspace` |
| 11 | High | coverage | Generated `Read`/`Clone`/`Copy`/multi-trait dispatch never runtime-asserted | **Fixed** — `behavior_coverage.rs` |
| 12 | High | metadata | `Cargo.toml` declares `no-std::no-alloc` but the crate is unconditionally `std` | **Fixed** — category removed |
| 13 | Med | doc | 7× broken `` [`sumtype::sumtype`] `` + 1× wrong `` [`std::marker::Clone`] `` | **Fixed** — links corrected |
| 14 | Med | doc | sumtrait-safety doc contradicts the enforced rules (receiver / arity) | **Fixed** — doc corrected |
| 15 | Low | coverage | `tests/module.rs` and `tests/test_mod.rs` are byte-identical duplicates | **Fixed** — `test_mod.rs` deleted |
| 16 | Low | CI | clippy gate omits `--all-targets`; tests fail `manual_repeat_n` under `-Dwarnings` | **Fixed** — `--all-targets` added, lints resolved |
| 17 | Low | metadata | Package description advertises "closures"; no `Fn*` support exists | **Fixed** — description corrected |
| 18 | Low | soundness | `sort_impl_generics` comparator violates strict-weak-ordering | **Fixed** — rank-based total order |
| 19 | Low | design | `__SumTrait_Sealed` apparatus + `#[allow(private_bounds)]` are dead; traits not sealed | Documented (left in place) |
| 20 | Low | design | `#[macro_export]` hoists a random-named macro to crate root for every `#[sumtrait]` | Documented (architectural) |
| 21 | Low | soundness | `random()` relies on empty-hasher output; `typeref_id` truncates `u64`→`usize` on 32-bit | Documented |
| 22 | Low | error-quality | Multiple-`Self`-arg / mismatch aborts fire at the use site, not the trait definition | Documented |
| 23 | Med | bug | Generic trait method with a turbofish-only generic (e.g. a `const` param) failed to dispatch (E0284) | **Fixed** — method generics forwarded as turbofish |
| 24 | High | bug | Built-in supertraits `Debug`/`Display`/`Error`/`Iterator`/`Read` silently dropped (bare ident hit `_ => ()`); qualified std paths unrecognized | **Fixed** — exhaustive supertrait analysis; `traits::Hash` added |
| 25 | High | regression | Qualified supertrait matched by trailing `(module, Trait)` only → user trait at `crate::hash::Hash`/`mymod::fmt::Debug` mis-mapped to the std mock; bare name hijacked same-named user traits (introduced by #24) | **Fixed** — require std root (`core`/`std`/`alloc`); bare maps only `Copy`/`Clone` |
| 26 | High | bug | User-`#[sumtrait]` chaining (`Derived: Base`) generated `impl Derived` containing `Base`'s methods (E0407); supertrait never implemented | **Fixed** — child implements its own trait, not the passed-in parent path |

## Fixes applied

All in `macro/lib.rs` unless noted.

- **1 — `Hash`.** `sumtype::traits::Hash` is now defined (mock `trait Hash { fn hash<H: Hasher>(&self, &mut H); }`,
  `implement = ::core::hash::Hash`). The generated sum type forwards `hash` to the active variant
  (sound: the enum has no `Eq`/`PartialEq` to be inconsistent with). Coverage: `tests/traits_hash.rs`
  and the `Hash` cases in `tests/supertraits.rs`.
- **2/3 — `Eq`/`PartialEq` supertraits.** Rejected with a clear `abort!` at the trait definition
  (bare or qualified `core::cmp::*`), instead of a dangling path / silent drop. These need
  cross-variant codegen on the enum definition that the single-receiver dispatch cannot express
  (`PartialEq::eq(&self, other: &Self)` has two `Self` params; `Eq` requires `PartialEq`); left as
  future work. The dead `derive_traits` plumbing was removed end-to-end.
- **4/24 — exhaustive supertrait analysis.** `process_supported_supertraits` was rewritten: it
  recognizes every built-in mock trait (`Copy`, `Clone`, `Hash`, `Debug`, `Display`, `Error`,
  `Iterator`, `Read`) whether written as a bare ident or via its canonical `std`/`core` path
  (`std::fmt::Debug`), maps each to its `sumtype::traits::*` mock, and forwards any other path to
  the user `#[sumtrait]` dispatch macro. Previously bare `Debug`/`Display`/`Error`/`Iterator`/`Read`
  hit a `_ => ()` arm and were **silently dropped** (leaving the supertrait unimplemented), and
  qualified std paths failed with "expected macro". Coverage: `tests/supertraits.rs`. Only a
  foreign *plain* (non-`#[sumtrait]`) trait remains an unsupported supertrait.
- **5 — const generics.** `replaced_ty_generics` now drops const arguments from the `__Uninhabited`
  `PhantomData` list (`filter_map → None`) instead of emitting `PhantomData<>`. Regression test:
  `tests/fixed_bugs.rs::const_generics_work`.
- **6 — duplicated generics.** `merge_generic_params` and `merge_generic_args` now deduplicate by
  parameter/argument name (merging bounds), so a `for<…>` that repeats the enclosing item's own
  generics no longer produces `E0403`/`E0107`. Regression test:
  `tests/fixed_bugs.rs::for_generics_repeating_fn_generics_work`.
- **7 — empty bounds.** `inner()` aborts with
  `"`#[sumtype]` requires at least one trait bound …"` when `args.bounds` is empty.
- **9 — missing `marker`.** The `sumtrait` attribute now uses `abort!` (with the parse-error span)
  instead of `.unwrap()`, so a missing/invalid `marker` is a normal compile error, not a panic.
- **10 — macro-crate docs.** The three broken duplicate doctests on the `sumtype` proc-macro were
  replaced with a short pointer to the facade crate. CI now runs `cargo test --workspace`, which
  exercises the macro crate's doctests too.
- **12/17 — metadata.** Removed the inaccurate `no-std::no-alloc` category and the "closures" claim
  from both `Cargo.toml`s.
- **13/14 — docs.** Fixed the 7 `` [`sumtype::sumtype`] `` links to `` [`crate::sumtype`] `` and
  `` [`std::marker::Clone`] `` to `` [`std::clone::Clone`] `` (`lib.rs`); corrected the
  sumtrait-safety bullet to require a receiver and permit additional non-`Self` parameters.
- **15 — duplicate test.** Deleted `tests/test_mod.rs`.
- **16 — clippy gate.** CI clippy now uses `--workspace --all-targets`; the resulting
  `manual_repeat_n` hits in the existing test files are silenced with a documented file-level
  `#[allow]` (the `repeat().take()` form is required to match their explicit `Take<Repeat<_>>`
  return types).
- **18 — `sort_impl_generics`.** Replaced the non-transitive comparator with a rank-based
  `sort_by_key` (lifetimes < consts < types), a proper total order (`sumtrait_internal.rs`).
- **25 — supertrait std-root check** (found in the second re-audit, a regression from #24).
  `map_builtin_supertrait` now maps a *qualified* path only when it is rooted at `core`/`std`/`alloc`
  (via `is_std_rooted`) and a *bare* ident only for the prelude markers `Copy`/`Clone`; everything
  else (incl. `crate::hash::Hash`, a user `Iterator`, `crate::cmp::Eq`) is forwarded to the user
  dispatch macro and never hijacked. `unsupported_eq_supertrait` likewise rejects only the std
  `Eq`/`PartialEq`. Coverage: `tests/supertrait_collisions.rs`.
- **26 — user-sumtrait chaining** (found in the second re-audit). In `sumtrait_internal`, a user
  `#[sumtrait]` (no `implement = …`) now implements **its own** trait (`item_trait.ident`) instead
  of the passed-in `trait_path`, which for a supertrait recursion was the *parent* trait — so
  `Derived: Base` now generates `impl Base for Enum` (was E0407). The trait must be reachable by its
  bare name at the `#[sumtype]` site. Coverage: `tests/supertrait_collisions.rs`.
- **23 — generic-method dispatch.** `process_fn` (`sumtrait_internal.rs`) now forwards a method's
  type/const generics to the dispatched call as a turbofish (`<Ty as Trait>::method::<T, N>(..)`),
  so a generic supplied only via turbofish (notably a `const` parameter, which previously failed
  with `E0284`) resolves. Found in a follow-up probe; generic methods whose generics are inferable
  from the arguments or return type already worked. Coverage: `tests/generic_methods.rs` (arg
  position, by value, return-only, const, and lifetime generics).

## Remaining limitations (documented, not fixed)

- **4 — foreign plain supertrait.** Built-in std/core traits are now recognized (bare or
  qualified), but a *plain* user trait (not `#[sumtrait]`, not a built-in) used as a supertrait is
  still emitted as a macro invocation and fails with "cannot find macro". A clearer error would
  require resolving sumtrait-ness at expansion time, which the macro cannot do.
- **8 — nested `sumtype!`.** Meaningless (wrapping a wrapped value); left as a compile error.
- **Sumtrait method generic colliding with the enclosing item's generic** (`fn make<T>` + method
  `fn m<T>`) → E0403. Illegal in hand-written Rust too; sumtype does not alpha-rename. Rename either
  generic. `compile_fail` test in `tests/compile_fail.md`.
- **RPIT-returning sumtrait method** (`fn m(&self) -> impl Trait`) → the `match` dispatch produces
  incompatible opaque types per arm (E0308). Would need the TypeRef machinery to unify.
  `compile_fail` test in `tests/compile_fail.md`.
- **Const generic in an explicit `sumtype![..]` type** → a bare const ident parses as a type arg and
  is rejected by the kind-compat check. The `impl Trait` return form works (`tests/fixed_bugs.rs`).
  `compile_fail` test in `tests/compile_fail.md`.
- **User-sumtrait supertrait reached only by a qualified path** whose trait name is not in scope at
  the `#[sumtype]` site (e.g. `mymod::Foo`) — the generated `impl Foo for Enum` uses the bare name.
  Bring the trait into scope (import it / same-level). Noted in `tests/supertrait_collisions.rs`.
- **Aliased std trait** (`use std::fmt::Debug as Dbg`) used bare as a supertrait is treated as a user
  trait and fails with "cannot find macro `Dbg`". Use the canonical name/path.
- **19/20/21/22 — design/soundness nits.** Dead `__SumTrait_Sealed` apparatus; `#[macro_export]`
  hoisting a random-named macro to the crate root; `random()` relying on empty-hasher output and a
  32-bit `usize` truncation of `typeref_id`; and use-site (rather than definition-site) aborts for
  malformed sumtrait methods. These are low-severity and were left to avoid riskier refactors.

## How the findings are realized as tests

- **`tests/compile_fail.md`** — 9 `compile_fail` doctests, wired in via a `#[cfg(doctest)]` item in
  `lib.rs` so they run under `cargo test` (*Doc-tests sumtype*) yet are invisible to `cargo doc`.
  Four are *intentional rejections* (`Eq`/`PartialEq` supertraits, empty bounds, missing marker)
  that lock in the new clear errors; five are *known limitations* (foreign plain supertrait, nested
  `sumtype!`, sumtrait method generic colliding with the enclosing generic, RPIT-returning sumtrait
  method, const generic in an explicit `sumtype![..]` type). They are `compile_fail` doctests rather
  than `trybuild` fixtures because several diagnostics embed the per-compile-random
  `__Sumtype_Enum_<random>` name, which would make a pinned `.stderr` permanently flaky.
- **`tests/fixed_bugs.rs`** — positive regression tests for the const-generic (#5) and
  duplicated-generics (#6) fixes, plus const+lifetime and 3-bound (`A + B + C`) coverage.
- **`tests/generic_methods.rs`** + **`tests/generic_methods_complex.rs`** — coverage for generic
  `#[sumtrait]` methods (#23): generics in argument position, by value, return-only, const, and
  lifetime; plus complex lists (multiple params, where-clauses with associated types, HRTB,
  dependent turbofish-only return generics, mixed lifetime/type/const).
- **`tests/supertraits.rs`** + **`tests/supertrait_collisions.rs`** + **`tests/traits_hash.rs`** —
  supertrait recognition (#4, #24, #25), user-sumtrait chaining (#26), std-name collisions, and the
  new `traits::Hash` target (#1).
- **`tests/behavior_coverage.rs`** — runtime assertions for correct `Read`, `Clone`, multi-trait
  `Iterator + Clone`, and `Copy` behavior (#11); `tests/module.rs` now asserts the `mod` expansion
  at runtime.
- The remaining findings are environmental (CI, metadata, rustdoc links, dead code) and are fixed
  directly in the relevant files.

## MSRV (`rust-version = "1.56.0"`)

Both crates declare `rust-version = "1.56.0"` (the edition-2021 floor). To honor it, the macro
crate's own code was lowered off the features it had been using:

- `macro/generics.rs` — `merge_generic_params`/`merge_generic_args` no longer use associated-type
  bounds in `impl Trait` (`impl IntoIterator<Item = …, IntoIter: Clone>`, stable 1.79). They take a
  plain `impl IntoIterator<Item = …>` and order via a stable `sort_by_key` on a kind rank.
- `macro/supertrait.rs` — `Option::is_some_and` (1.70) → `map_or(false, …)`; `let … else` (1.65) →
  `if let … else`.

`cargo clippy` is now MSRV-aware (it reads `rust-version`): it stops suggesting newer idioms (e.g.
`is_some_and`) and its `incompatible_msrv` lint guards the library against future stdlib-API
regressions below 1.56.

**Caveats (verified on rustc 1.90 only — no 1.56 toolchain available here):**
- A real 1.56 build of the published library still depends on the **dependency tree** (`syn 2`,
  `darling`, `proc-macro2`, `template-quote`, `type-leak`, `derive-syn-parse`, `syn_derive`)
  supporting 1.56; that was not verifiable in this environment.
- The **test suite** still uses features newer than 1.56 (inline format args 1.58; RPIT-in-traits
  1.75 inside `compile_fail` doctests). Per the usual convention, `rust-version` governs the
  published library, not dev-time tests; CI runs on current stable. `clippy --all-targets` passes
  because `incompatible_msrv` only flags stdlib APIs, not syntax-level features in test code.
- No MSRV CI job was added (it would require a 1.56 toolchain and dependency support that can't be
  confirmed here). Recommended next step: add a `rust-version`-pinned CI job (`cargo +1.56 build`)
  to enforce the claim once dependency MSRVs are confirmed.

## Enhancements (post-audit feature requests)

These were requested after the audit and are additive (no behavior change to existing usage):

- **Crate-rename robustness.** `#[sumtrait]`'s `krate` default was a hardcoded `::sumtype`, which
  broke when a downstream crate renamed `sumtype` in its `Cargo.toml`. It now resolves the real
  crate name with `proc-macro-crate` (`sumtype_crate()` in `macro/lib.rs`); the same resolution is
  used for the generated `Downcast` impl. `proc-macro-crate` 1.3.x has `rust-version = 1.56.0`, so
  this keeps the declared MSRV. Verified end-to-end against a crate that renames `sumtype` to `st`.
- **`Downcast<To>` trait.** `sumtype::Downcast<To>` is implemented automatically for every generated
  sum type, providing `downcast_ref`/`downcast_mut` (allocation-free) and an owning
  `downcast(self) -> Result<To, Self>`. It recovers a concrete variant type at runtime via
  `core::any::TypeId`, so it is gated on `Self: 'static` (unavailable for sum types that wrap borrowed
  values). To reach it on a function's result, name the target type(s) in the return bound —
  `-> impl Trait + Downcast<To>` (one `+ Downcast<T>` per recoverable type) — which works with the
  ordinary type-less `sumtype!(expr)` form; the named `-> sumtype!()` type exposes every `To` at once.
  `To` is a trait parameter, so it also doubles as a generic bound (`E: Downcast<u32>`). Coverage:
  `tests/downcast.rs`. (Generated in `emit_items`.)
