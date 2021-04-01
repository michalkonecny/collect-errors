{-# LANGUAGE ConstraintKinds #-}
module Control.CollectErrors.Type where

import Prelude

import Control.Applicative ( Applicative(liftA2), liftA )

import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Maybe (fromJust)

import Test.QuickCheck ( Arbitrary(arbitrary) )

import Text.Printf ( printf )

{-|
  A wrapper around values which can accommodate a list of
  (potential) errors that have (maybe) occurred during the computation
  of a value.  A value may be missing, leaving only the error(s).

  Such error collection allows one to write expressions with partial
  functions (ie functions that fail for some inputs) instead of
  branching after each application of such function.
  Dealing with the errors can be moved outside the expression.
  If the error data contain enough information, their list can be used
  to trace the source of the errors.
-}
data CollectErrors es v =
  CollectErrors
    { getMaybeValue :: Maybe v
    , getErrors :: es }

class CanTestErrorsCertain es where
  hasCertainError :: es -> Bool

instance (CanTestErrorsCertain es) => CanTestErrorsCertain (CollectErrors es v) where
  hasCertainError (CollectErrors _ es) = hasCertainError es

instance (CanTestErrorsCertain es) => CanTestErrorsCertain [es] where
  hasCertainError = or . map hasCertainError

class CanTestErrorsPresent es where
  hasError :: es -> Bool

instance (CanTestErrorsPresent es) => CanTestErrorsPresent (CollectErrors es v) where
  hasError (CollectErrors _ es) = hasError es

instance CanTestErrorsPresent [es] where
  hasError = not . null

type CanBeErrors es = (Monoid es, Eq es, Show es, CanTestErrorsCertain es, CanTestErrorsPresent es)

instance (Show v, CanBeErrors es) => (Show (CollectErrors es v)) where
  show (CollectErrors mv es) =
    case mv of
      Just v | es == mempty -> show v
      Just v -> printf "%s{%s}" (show v) (show es)
      Nothing -> printf "{%s}" (show es)

noValue :: es -> CollectErrors es v
noValue es = CollectErrors Nothing es

prependErrors :: (Monoid es) => es -> CollectErrors es v -> CollectErrors es v
prependErrors es1 (CollectErrors mv es2) = CollectErrors mv (es1 <> es2)

{-| A safe way to get a value out of the CollectErrors wrapper. -}
getValueIfNoError ::
  (CanBeErrors es)
  =>
  CollectErrors es v -> Either es v
getValueIfNoError (CollectErrors mv es) =
  case mv of
    Just v | es == mempty -> Right v
    _ -> Left es

filterValuesWithoutError ::
  (CanBeErrors es)
  =>
  [CollectErrors es v] -> [v]
filterValuesWithoutError [] = []
filterValuesWithoutError (vCE : rest) =
  either (const restDone) (: restDone) (getValueIfNoError vCE)
  where
  restDone = filterValuesWithoutError rest

-- functor instances:

instance Functor (CollectErrors es) where
  fmap f (CollectErrors mv es) =
    CollectErrors (fmap f mv) es

instance (Monoid es) => Applicative (CollectErrors es) where
  pure v = CollectErrors (Just v) mempty
  (CollectErrors (Just a) ae) <*> (CollectErrors (Just b) be) =
    CollectErrors (Just (a b)) (ae <> be)
  (CollectErrors _ ae) <*> (CollectErrors _ be) =
    CollectErrors Nothing (ae <> be)

lift :: (Monoid es) => (a -> b) -> (CollectErrors es a) -> (CollectErrors es b)
lift = liftA

lift2 :: (Monoid es) => (a -> b -> c) -> (CollectErrors es a) -> (CollectErrors es b) -> (CollectErrors es c)
lift2 = liftA2

lift1T :: (Monoid es) => (a -> b -> c) -> (CollectErrors es a) -> b -> (CollectErrors es c)
lift1T fn (CollectErrors (Just a) ae) b = CollectErrors (Just (fn a b)) ae
lift1T fn (CollectErrors _ ae) _ = CollectErrors Nothing ae

liftT1 :: (Monoid es) => (a -> b -> c) -> a -> (CollectErrors es b) -> (CollectErrors es c)
liftT1 fn a (CollectErrors (Just b) be) = CollectErrors (Just (fn a b)) be
liftT1 fn _ (CollectErrors _ be) = CollectErrors Nothing be

instance (Monoid es) => Monad (CollectErrors es) where
  ae >>= f =
    case ae of
      CollectErrors (Just a) es1 ->
        let (CollectErrors mv es2) = f a in
          CollectErrors mv (es1 <> es2)
      CollectErrors _ es ->
        CollectErrors Nothing es

instance (Arbitrary t, Monoid es) => Arbitrary (CollectErrors es t) where
  arbitrary = (\v -> CollectErrors (Just v) mempty) <$> arbitrary

