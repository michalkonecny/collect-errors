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
, toEither
, withErrorOrValue
, filterValuesWithoutError
, lift
, lift2
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
