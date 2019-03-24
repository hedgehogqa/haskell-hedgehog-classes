# Changelog

`hedgehog-classes` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

0.1.2
=====

* add `semiringLaws`, `ringLaws`, `starLaws`
* fix bug in `foldableLaws` that could cause implementations of
  `foldMap` and `fold` that evaluate in weird orders to pass (rather than fail).

0.1.1
=====

* Initial (stable) hackage release.

0.0.0
=====

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/chessai/hedgehog-classes/releases
