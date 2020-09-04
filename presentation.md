# [fit] Free monads from scratch
## [fit] A way to deal with effectful programs
## <br />
## [fit] Guillaume Bogard - guillaumebogard.dev

---

^ Before we dig in, I will introduce myself very quickly:

Bonjour ! 👋

My name is Guillaume Bogard. I'm a Scala Developer @Linkvalue.

I love functional programming, roller-coasters, and Age of Empires.

You can follow me on Twitter @bogardguillaume and on guillaumebogard.dev

---

## Agenda

1. Effects and the substitution model of evaluation
2. Free monads, interpreters, and their benefits
3. Implementing Free monads in Scala and Haskell
4. Free monads *in the real world*

---

^ Alright, let met start this talk with a question:

## What is functional programming about?

^ I'm sure some of you, if not most, already know what functional programming is: programming with mathematical functions, also referred to as "pure functions". 
And I'm guessing that some of you are already convinced of their benefits: more stability, more productivity, lower maintenance costs ...
But why do pure functions matter ? What is it that makes the functional programmer often more productive, and functional programs more stable than let's say imperative programs?

---

## What is the *root benefit* of functional programming?

^ I'm interested in answering this question because it will motivate the use of free monads later on.
For the purpose of this talk I am going to assert that all the benefits of functional programming – the productivity increase, the enhanced stability of programs, and their comparatively
low maintenance cost, all come from one root benefit, one fundamental aspect of functional programming.

---

^ To introduce this principle, a quote from John Hughes, professor in the department of Computing Science at the Chalmers University of Technology and author of QuickCheck, the property-based testing lib for Haskell.

Functional programs contain no assignment statements, so variables, once given a value, never change. More generally, functional programs contain no side-effects at all. A function call can have no effect other than to compute its result. 

This eliminates a major source of bugs, and also makes the order of execution irrelevant - since no side-effect can change the value of an expression, it can be evaluated at any time. This relieves the programmer of the burden of prescribing the ﬂow of control.

– *Why Functional Programming Matters*, John Hughes

^ I'm particularly interested in the second part of the quote, about the order of execution being irrelevant. 
This dramatically reduces the cognitive load required to write and understand programs.

---

### Functional programming is expression-oriented
## Evaluating a program is just evaluating expressions recursively until there are no more sub-expressions

---

## Expressions are equivalent if they have the same normal form\*

#### But there's a catch, more on that in a minute

---

## How substitution works

---

## Why substitution matters

---

### But, remember, there's a catch

---

## Expressions are equivalent if they have the same normal form\*
### \*Assuming the program is *referentially transparent*

---

## When substitution breaks


---

## Common practices that break substitution

---

## How do build *anything* useful without breaking substitution ?

---

## Turning *actions* into *effects*

Turning imperative side-effects into mere *descriptions*, and deferring their execution until the *end of the world*, allows us to freely substitute these descriptions.

We use pure data structures to describe the intended behavior, without running any externally visible effect until we have to.

 
The type system will help track the nature of the effects, in addition to the type of values our programs produce.

---

# *It's just data!*™
### – every Clojure developer ever

---

## Example: replacing Exceptions with Either

---

## Why effects matter

---

## Are effects and monads the same thing?

---

## Turning arbitrary instructions into effects

---

## Why IO isn't always ideal

---

## Can we do better ?

---

# II
# Free monads and their interpreters

---

## What is a Free monad?

---

## A Free monad for accounts management

---

## Benefits of free monads

---

# III
# Implementing Free monads

---

# IV
# Free monads in the real world

---


TODO: 3 benefits to referential transparency: - reduced cognitive load, programs are inspectable and testable, and programs can be refactored with confidence

IO only reduces the cognitive load and increases the ability to refactor, but does not allow for program inspection without evaluating side effects, hence
the need for reification.

TODO later: free monads allow programs to be optimized before execution, for example, dat afetching programs could be optimized ahead of time to avoid N+1 queries




