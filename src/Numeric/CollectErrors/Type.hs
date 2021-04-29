{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Numeric.CollectErrors.Type 

where

import qualified Data.Set as Set

import Control.CollectErrors
    ( CanTestErrorsCertain(..), CollectErrors, noValue, prependErrors, liftCE, lift2CE, lift1TCE, liftT1CE, unCollectErrors )

cn :: v -> CN v
cn = pure

unCN :: CN p -> p
unCN = unCollectErrors

type CN = CollectErrors NumErrors
type NumErrors = Set.Set NumErrorLevel
type NumErrorLevel = (NumError, ErrorCertaintyLevel)

data NumError =
    DivByZero | OutOfDomain String | NumError String
    deriving (Eq, Ord)

instance Show NumError where
  show DivByZero = "division by 0"
  show (OutOfDomain s) = "out of domain: " ++ s
  show (NumError s) = "numeric error: " ++ s

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
  deriving (Eq, Ord)

instance Show ErrorCertaintyLevel where
  show ErrorCertain = "ERROR"
  show ErrorPotential = "POTENTIAL ERROR"

instance CanTestErrorsCertain NumErrorLevel where
  hasCertainError = (== ErrorCertain) . snd

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertain :: NumError -> CN v
noValueNumErrorCertain e = noValue $ Set.singleton (e, ErrorCertain)

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotential :: NumError -> CN v
noValueNumErrorPotential e = noValue $ Set.singleton (e, ErrorPotential)

prependErrorCertain :: NumError -> CN t -> CN t
prependErrorCertain e = prependErrors $ Set.singleton (e, ErrorCertain)
  
prependErrorPotential :: NumError -> CN t -> CN t
prependErrorPotential e = prependErrors $ Set.singleton (e, ErrorPotential)

liftCN  :: (a -> (CN c)) -> (CN a) -> (CN c)
liftCN = liftCE

lift2CN  :: (a -> b -> (CN c)) -> (CN a) -> (CN b) -> (CN c)
lift2CN = lift2CE

lift1TCN  :: (a -> b -> (CN c)) -> (CN a) -> b -> (CN c)
lift1TCN = lift1TCE
liftT1CN  :: (a -> b -> (CN c)) -> a -> (CN b) -> (CN c)
liftT1CN = liftT1CE
