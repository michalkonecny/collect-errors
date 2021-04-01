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
, getValueIfNoError
, filterValuesWithoutError
, lift
, lift2
)
where

import Prelude

import Text.Printf ( printf )

import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Maybe (fromJust)

import Test.QuickCheck ( Arbitrary(arbitrary) )

import Control.CollectErrors.Type
import Control.CollectErrors.PreludeInstances ()
