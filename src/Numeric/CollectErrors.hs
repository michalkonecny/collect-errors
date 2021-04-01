module Numeric.CollectErrors
(
  -- * Type of numeric errors
  NumError(..), ErrorCertaintyLevel(..), NumErrors, CN, cn
  -- * Utilities
, noValueNumErrorCertain, noValueNumErrorPotential
  -- ** Applicable general collect-error utilities
, noValue
, prependErrors
, getValueIfNoError
, filterValuesWithoutError
, lift
, lift2
, lift1T
, liftT1
)
where

import Control.CollectErrors
import Numeric.CollectErrors.Type
import Numeric.CollectErrors.PreludeInstances ()
