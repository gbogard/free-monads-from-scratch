{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Free (Free, liftF)

newtype UserId = UserId String

newtype SubId = SubId String

newtype User = User UserId

newtype Subscription = Subscription SubId

-- We start by defining the DSL

data UserStoreDsl next
  = GetUser UserId (User -> next)
  | GetSubscription
      UserId
      (Maybe Subscription -> next)
  | DeleteSubscription SubId next
  | Subscribe User (Subscription -> next)
  deriving (Functor)

type UserStore = Free UserStoreDsl

-- Then smart constructors

getUser :: UserId -> UserStore User
getUser uid = liftF (GetUser uid id)

getSubscription :: UserId -> UserStore (Maybe Subscription)
getSubscription uid = liftF (GetSubscription uid id)

deleteSubscription :: SubId -> UserStore ()
deleteSubscription subId =
  liftF (DeleteSubscription subId ())

subscribe :: User -> UserStore Subscription
subscribe u = liftF (Subscribe u id)

-- Then we build a program using our DSL

updateSubscription :: UserId -> UserStore ()
updateSubscription userId = do
  user <- getUser userId
  oldSub <- getSubscription userId
  case oldSub of
    Just (Subscription id) ->
      deleteSubscription id
    Nothing -> pure ()
  subscribe user
  return ()

-- Then we write an interpreter for our DSL

type f ~> g = forall x. f x -> g x

userStoreInterpreter :: UserStoreDsl ~> IO
userStoreInterpreter (GetUser id next) = do
  -- Fetch user from database
  let user = User (UserId "123")
  pure (next user)
userStoreInterpreter
  (GetSubscription (UserId "123") next) = do
    let sub = Subscription (SubId "1")
    pure (next (Just sub))
userStoreInterpreter (GetSubscription _ next) =
  pure (next Nothing)
userStoreInterpreter (DeleteSubscription _ next) =
  pure next
userStoreInterpreter (Subscribe _ next) = do
  let sub = Subscription (SubId "new-sub")
  pure (next sub)
