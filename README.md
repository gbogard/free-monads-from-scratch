# Free monads from scratch - A way to deal with effectful programs

*Free monads from scratch* is a talk about Free monads, their motivation, their use cases for
"real-world applications", and their implementation. It is mostly based on Wouter Swierstra's paper,
*Data Types à la carte*.

## Repository structure

- `presentation.md` is the source for the slideshow, generated using Deckset
- `scala-samples` is a sbt project with several modules
- `haskell-samples` is a stack project with several packages

## Acknowledgments

This talk wouldn't have been possible without the work of these people:

- [Adam Rosien](https://twitter.com/arosien) with his article, [*What is an effect*](https://www.inner-product.com/posts/what-is-an-effect/) which provides
an excellent introduction to effects, and a nice motivation for Free monads
- [Wouter Swierstra](https://twitter.com/wouterswierstra) with his paper, [*Data types à la carte*](http://www.staff.science.uu.nl/~swier004/talks/2018-fp-ams.pdf)
which provides the reference implementation for Free monads, on which this presentation's code is mostly based.
- [Edward A. Kmett](http://comonad.com/reader/) and contributors for the [free](https://hackage.haskell.org/package/free) Haskell package
- [Cats](https://github.com/typelevel/cats) contributors for their work on the `cats-free` module, among everything else.