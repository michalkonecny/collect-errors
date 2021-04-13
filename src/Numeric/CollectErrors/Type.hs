{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Numeric.CollectErrors.Type 

where

import Control.CollectErrors
    ( CanTestErrorsCertain(..), CollectErrors, noValue, prependErrors, liftCE, lift2CE )

cn :: v -> CN v
cn = pure

type CN = CollectErrors NumErrors
type NumErrors = [NumErrorLevel]
type NumErrorLevel = (NumError, ErrorCertaintyLevel)

data NumError =
    DivByZero | OutOfDomain String | NumError String
    deriving (Eq)

instance Show NumError where
  show DivByZero = "division by 0"
  show (OutOfDomain s) = "out of domain: " ++ s
  show (NumError s) = "numeric error: " ++ s

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
  deriving (Eq)

instance Show ErrorCertaintyLevel where
  show ErrorCertain = "ERROR"
  show ErrorPotential = "POTENTIAL ERROR"

instance CanTestErrorsCertain NumErrorLevel where
  hasCertainError = (== ErrorCertain) . snd

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertain :: NumError -> CN v
noValueNumErrorCertain e = noValue [(e, ErrorCertain)]

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotential :: NumError -> CN v
noValueNumErrorPotential e = noValue [(e, ErrorPotential)]

prependErrorCertain :: NumError -> CN t -> CN t
prependErrorCertain e = prependErrors [ (e, ErrorCertain) ] 
  
prependErrorPotential :: NumError -> CN t -> CN t
prependErrorPotential e = prependErrors [ (e, ErrorPotential) ] 

liftCN  :: (a -> (CN c)) -> (CN a) -> (CN c)
liftCN = liftCE

lift2CN  :: (a -> b -> (CN c)) -> (CN a) -> (CN b) -> (CN c)
lift2CN = lift2CE