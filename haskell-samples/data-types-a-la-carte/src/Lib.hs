{-# LANGUAGE DeriveFunctor, TypeOperators #-}

module Lib where

import Control.Monad (ap)

newtype UserId = UserId String
  deriving (Eq, Show)

newtype SubId = SubId String

newtype User = User UserId
  deriving (Eq, Show)

newtype Subscription = Subscription SubId

-- We start by defining a polymorphic Expr type, whose type parameter
-- corresponds to the type of expressions happening in the subtree

data Expr f = In (f (Expr f))

-- Then we could define a data types for expressions consisting only of integers
-- We don't actually use the type parameter because this expressions accepts no sub-expressions

data Add e = Add e e

type AddExpr = Expr Add

data Val e = Val Int

type ValExpr = Expr Val

-- We can combine these types using their coproduct

data (f :+: g) e = Inl (f e) | Inr (g e)

type ValOrAddExpr = Expr (Val :+: Add)

-- We can build programs using expressions of type Val :+: Add
-- 10 + 2 + 5
program :: ValOrAddExpr
program =
  In
    ( Inr
        ( Add
            (In (Inl (Val 10)))
            ( In
                ( Inr
                    ( Add
                        (In (Inl (Val 2)))
                        (In (Inl (Val 5)))
                    )
                )
            )
        )
    )

-- To evaluate our program, we must observe that not only Val and Add are functors, but also that
-- the coproduct of 2 functors is itself a functor. This allows us to fold over values of type ValOrdExpr
-- to get a value.
instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add a b) = Add (f a) (f b)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl term) = Inl (fmap f term)
  fmap f (Inr term) = Inr (fmap f term)

-- Given a functor F[A] and a function F[A], we can fold over F[A] to get a value like so

evalVal :: Val t -> Int
evalVal (Val number) = number

evalAdd :: Add Int -> Int
evalAdd (Add a b) = a + b

evalValOrAdd :: (Val :+: Add) Int -> Int
evalValOrAdd (Inl t) = evalVal t
evalValOrAdd (Inr t) = evalAdd t

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In term) = f (fmap (foldExpr f) term)

result :: Int
result = foldExpr evalValOrAdd program

-- Now that we have shown how functors can be combined using coproducts
-- we can generalize this approach to free monads

data Free f a
  = Pure a
  | Impure (f (Free f a))

-- When f is Functor, Free f is a monad, as illustrated by these instances

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure x) = Impure (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
  pure x = Pure x
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (Pure x) >>= f = f x
  (Impure x) >>= f = Impure (fmap (>>= f) x)

-- Then we can define a simple dsl

data UserStoreDsl next
  = GetUser UserId (User -> next)
  | Subscribe User next
  deriving (Functor)

type UserStore = Free UserStoreDsl

-- Along with smart constructors

getUser :: UserId -> UserStore User
getUser id = Impure (GetUser id Pure)

subscribe :: User -> UserStore ()
subscribe user = Impure (Subscribe user (Pure ()))

-- And we use them to build a program
freeProgram :: UserStore User
freeProgram = do
  user <- getUser (UserId "123")
  subscribe user
  return user

-- Finally, given a transformation from F to IO,
-- we define a fold over Free[F, A] that turns it into IO[A]

class Functor f => Exec f where
  exec :: f a -> IO a

instance Exec UserStoreDsl where
  exec (GetUser id next) = do
    let user = User id
    return (next user)
  exec (Subscribe user next) = do
    putStrLn ("User" <> show user <> " has subscribed") >> return next

execAlgebra :: Exec f => Free f a -> IO a
execAlgebra (Pure x) = return x
execAlgebra (Impure x) = exec x >>= execAlgebra

-- Finally we run the program
freeResult :: IO User
freeResult = execAlgebra freeProgram
