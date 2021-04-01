module Numeric.CollectErrors
(
  -- * Type of numeric errors
  NumError(..), ErrorCertaintyLevel(..), NumErrors, CN, cn
  -- * Utilities
, noValueNumErrorCertain, noValueNumErrorPotential
  -- ** Applicable general collect-error utilities
, hasCertainError
, hasError
, noValue
, getMaybeValue, getErrors, prependErrors
)
where

import Control.CollectErrors
import Numeric.CollectErrors.Type
import Numeric.CollectErrors.PreludeInstances ()
