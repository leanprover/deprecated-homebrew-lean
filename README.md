homebrew-lean
=============

Homebrew tap for [Lean theorem prover][lean]

[lean]: https://github.com/leanprover/lean


How to Install
--------------

```bash
brew tap leanprover/lean
brew install --HEAD lean
```

``lean`` is a head-only formula which does not support homebrew's
update mechanism. Please use the following trick to upgrade lean to
the latest version:

```bash
brew rm lean && brew install --HEAD lean
```
