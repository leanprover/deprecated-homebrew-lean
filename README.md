homebrew-lean [![Build Status](https://travis-ci.org/leanprover/homebrew-lean.svg?branch=master)](https://travis-ci.org/leanprover/homebrew-lean)
=============

Homebrew tap for [Lean theorem prover][lean]

[lean]: https://github.com/leanprover/lean

How to Install
--------------

```bash
brew install leanprover/lean/lean
```

To upgrade lean to the latest version, run the following commands:

```bash
brew update && brew upgrade
```

How to Edit the Formula
-----------------------

Please make changes in `lean.rb.template` file. The real formula file
`lean.rb` will be generated from `lean.rb.template` file
automatically.
