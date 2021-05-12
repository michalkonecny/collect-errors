{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
module Numeric.CollectErrors.Type 

where

import GHC.Generics
import Control.DeepSeq

import qualified Data.List as List
import qualified Data.Set as Set

import Control.CollectErrors
    ( CanTestErrorsCertain(..), CollectErrors, noValue, removeValue, prependErrors, liftCE, lift2CE, lift1TCE, liftT1CE, unCollectErrors, CanTestErrorsPresent, CanTakeErrors  )

cn :: v -> CN v
cn = pure

unCN :: CN p -> p
unCN = unCollectErrors

type CN = CollectErrors NumErrors
newtype NumErrors = NumErrors (Set.Set NumErrorLevel)
  deriving (Eq, Semigroup, Monoid, CanTestErrorsCertain, CanTestErrorsPresent, Generic, NFData)
type NumErrorLevel = (NumError, ErrorCertaintyLevel)

instance Show NumErrors where
  show (NumErrors set) =
    "{" <> (List.intercalate "; " $ map showEL $ Set.toList set)  <>  "}"
    where
    showEL (e,l) =
      show l <> ": " <> show e

data NumError =
    DivByZero | OutOfDomain String | NumError String
    deriving (Eq, Ord, Generic)

instance NFData NumError

instance Show NumError where
  show DivByZero = "division by 0"
  show (OutOfDomain s) = "out of domain: " ++ s
  show (NumError s) = "numeric error: " ++ s

data ErrorCertaintyLevel =
  ErrorCertain | ErrorPotential
  deriving (Eq, Ord, Generic)

instance NFData ErrorCertaintyLevel

instance Show ErrorCertaintyLevel where
  show ErrorCertain = "ERROR"
  show ErrorPotential = "POTENTIAL ERROR"

instance CanTestErrorsCertain NumErrorLevel where
  hasCertainError = (== ErrorCertain) . snd

{-| Construct an empty wrapper indicating that given error has certainly occurred. -}
noValueNumErrorCertain :: NumError -> CN v
noValueNumErrorCertain e = noValue $ NumErrors $ Set.singleton (e, ErrorCertain)

{-| Construct an empty wrapper indicating that given error may have occurred. -}
noValueNumErrorPotential :: NumError -> CN v
noValueNumErrorPotential e = noValue $ NumErrors $ Set.singleton (e, ErrorPotential)

removeValueErrorCertain :: CN t -> NumError -> CN t
removeValueErrorCertain v e = 
  removeValue v $ NumErrors $ Set.singleton (e, ErrorCertain)

removeValueErrorPotential :: CN t -> NumError -> CN t
removeValueErrorPotential v e = 
  removeValue v $ NumErrors $ Set.singleton (e, ErrorPotential)

prependErrorCertain :: NumError -> CN t -> CN t
prependErrorCertain e = prependErrors $ NumErrors $ Set.singleton (e, ErrorCertain)
  
prependErrorPotential :: NumError -> CN t -> CN t
prependErrorPotential e = prependErrors $ NumErrors $ Set.singleton (e, ErrorPotential)

liftCN  :: (a -> (CN c)) -> (CN a) -> (CN c)
liftCN = liftCE

lift2CN  :: (a -> b -> (CN c)) -> (CN a) -> (CN b) -> (CN c)
lift2CN = lift2CE

lift1TCN  :: (a -> b -> (CN c)) -> (CN a) -> b -> (CN c)
lift1TCN = lift1TCE
liftT1CN  :: (a -> b -> (CN c)) -> a -> (CN b) -> (CN c)
liftT1CN = liftT1CE

type CanTakeCNErrors = CanTakeErrors NumErrors
