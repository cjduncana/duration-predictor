# Duration Predictor (title subject to change)

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

Better predict how much time does it take to complete recurring activities.

### Problem

I have a terrible sense of how much time I spend on recurring activities, so I terribly distribute my time and miss commitments.

### Goal

I want a tool that can better predict how much time to allow for an activity based on previous measurements.

### Concepts

* **Activity:** Any recurring task that you want to measure and better predict in the future.
* **Measurement:** The User or the System measures the actual Duration it took to finish the Activity to make future predictions.

## Getting Started

### Prerequisites

To contribute to this project, you need the following programs:

* [The Glasgow Haskell Compiler (GHC)][haskell-ghc]
* [Cabal][haskell-cabal]

If you do not have these tools, you can follow [GHCup][haskell-ghcup] instructions to have them installed on your machine.

## Running the wizard

To start the program, type `cabal new-run duration-predictor` in the Terminal in the project root. It will ask for information about a new Activity and its first Measurement.

## Running the tests

This program is tested with the following versions:

* [base v4.14.3.0][haskell-base]
* [cabal v3.6.2.0][haskell-cabal]
* [ghc v8.10.7][haskell-ghc]
* [ghcup v0.1.17.6][haskell-ghcup]

Type `cabal new-run test` in the Terminal in the project root to run the tests.

## Generating the documentation

To generate a local copy of this program's documentation, type `cabal new-haddock`. It will create an HTML website locally to see the documentation.

## Built With

* [Haskell][haskell]: The language used
* [Polysemy][polysemy]: Higher-order, low-boilerplate free monads
* [Tasty][tasty]: Modern and extensible testing framework
* [Ormulo][ormulo]: A formatter for Haskell source code

## Versioning

We use [SemVer][semver] for versioning.

## Author

* **Christopher Duncan** - *Initial work* - [GitHub profile][cj-github]

## License

This project is licensed under the MIT License - see the [LICENSE][license] file for details

## Acknowledgments

* Ram :heart: for inspiring this project by displaying unwavering dilligence

[cj-github]: https://github.com/cjduncana "Christopher Duncan's GitHub Profile"
[haskell]: https://www.haskell.org/ "Haskell: An advanced, purely functional programming language"
[haskell-base]: https://hackage.haskell.org/package/base "Haskell Basic libraries"
[haskell-cabal]: https://cabal.readthedocs.io/en/stable/ "Cabal: package system for Haskell software"
[haskell-ghc]: https://www.haskell.org/ghc/ "The Glasgow Haskell Compiler (GHC)"
[haskell-ghcup]: https://www.haskell.org/ghcup/ "GHCup: installer for Haskell"
[license]: LICENSE "License file"
[ormulo]: https://github.com/tweag/ormolu "Ormulo: A formatter for Haskell source code"
[polysemy]: https://hackage.haskell.org/package/polysemy "Higher-order, low-boilerplate free monads"
[semver]: http://semver.org/ "Semantic Versioning"
[tasty]: https://hackage.haskell.org/package/tasty "Modern and extensible testing framework"
