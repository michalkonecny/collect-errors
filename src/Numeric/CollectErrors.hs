module Numeric.CollectErrors
(
  -- * Type of numeric errors
  NumError(..), ErrorCertaintyLevel(..), NumErrors, CN, cn
  -- * Utilities
, noValueNumErrorCertain, noValueNumErrorPotential
  -- ** Applicable general collect-error utilities
, noValue
, prependErrors
, prependErrorCertain
, prependErrorPotential
, CanTestErrorsCertain(..)
, CanTestErrorsPresent(..)
, toEither
, withErrorOrValue
, filterValuesWithoutError
, lift
, liftCN
, liftPair
, lift2
, lift2CN
, lift1T
, liftT1
, lift2pair
, lift1Tpair
, liftT1pair
)
where

import Control.CollectErrors
import Numeric.CollectErrors.Type
import Numeric.CollectErrors.PreludeInstances ()
