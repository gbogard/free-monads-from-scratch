# Free monads from scratch - A way to deal with effectful programs

*Free monads from scratch* is a talk about Free monads, their motivation, their use cases for
"real-world applications", and their implementation. It is mostly based on Wouter Swierstra's paper,
*Data Types à la carte*.

## Disclaimer

The implementations in this repository are solely meant for educational purposes. They are neither stack-safe nor very performant,
and shouldn't be used in production. If you want the awesomeness of Free monads for your application, please use one of the widely-available
implementations.

## Repository structure

- `presentation.md` is the source for the slideshow, generated using Deckset
- `scala-samples` is a sbt project with several modules:
    - The `data-types-a-la-carte` module contains a Free monad implementation loosely based on *Data types à la carte*
    - The `cats-free-monads` module contains examples of using `cats-free` to structure programs
- Similarly, `haskell-samples` is a stack project with several packages
    - The `data-types-a-la-carte` package contains a Free monad implementation loosely based on *Data types à la carte*
    - The `free-package` package contains examples of using `free` to structure programs

### Acknowledgments

This talk wouldn't have been possible without the work of these amazing people:

- [Cats](https://github.com/typelevel/cats) contributors for their work on the `cats-free` module, among everything else.
- [James Haydon](https://www.linkedin.com/in/james-haydon-b2651066/) with his article [Free monads for cheap interpreters](https://www.tweag.io/blog/2018-02-05-free-monads/)
- [Edward A. Kmett](http://comonad.com/reader/) and contributors for the [free](https://hackage.haskell.org/package/free) Haskell package
- [Adam Rosien](https://twitter.com/arosien) with his article, [*What is an effect*](https://www.inner-product.com/posts/what-is-an-effect/) which provides
an excellent introduction to effects, and a nice motivation for Free monads
- [Wouter Swierstra](https://twitter.com/wouterswierstra) with his paper, [*Data types à la carte*](http://www.staff.science.uu.nl/~swier004/talks/2018-fp-ams.pdf)
which provides the reference implementation for Free monads, on which this presentation's code is mostly based.

### Get in touch

- E-mail: hey [at] guillaumebogard [dot] dev 
- Homepage: [guillaumebogard.dev](https://guillaumebogard.dev) 
- Twitter: [@bogardguillaume](https://twitter.com/bogardguillaume)

