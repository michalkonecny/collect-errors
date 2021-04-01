module Control.CollectErrors
(
-- * Monad for collecting errors in expressions
  CollectErrors(..)
, CanBeErrors
, CanTestErrorsCertain(..)
, CanTestErrorsPresent(..)
-- ** Utilities
, noValue
, prependErrors
, toEither
, withErrorOrValue
, filterValuesWithoutError
, lift
, lift2
, lift1T
, liftT1
)
where

import Prelude

import Text.Printf ( printf )

import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Maybe (fromJust)

import Test.QuickCheck ( Arbitrary(arbitrary) )

import Control.CollectErrors.Type
import Control.CollectErrors.PreludeInstances ()
