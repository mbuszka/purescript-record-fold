module Catamorphism where

import Prelude

import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.Record (get, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Algebra (name :: Symbol) (lbl :: Symbol) input res' res | name -> res where
  algebra :: (SProxy name) -> (SProxy lbl) -> input -> res' -> res

class Zero (name :: Symbol) res | name -> res where
  algZero :: SProxy name -> res

class Cata (name :: Symbol) (list :: RowList) (row :: # Type) res | name -> res where
  cata :: (SProxy name) -> (RLProxy list) -> Record row -> res

instance cataRowCons
  ::
  ( Algebra name lbl input res' res
  , IsSymbol lbl
  , RowCons lbl input rest row
  , Cata name tail row res'
  ) => Cata name (Cons lbl input tail) row res where
  cata name _ record =
    let
      key = SProxy :: SProxy lbl
      tail = RLProxy :: RLProxy tail
      res = cata name tail record
    in algebra name key (get key record) res

instance cataRowNil
  :: Zero name res => Cata name Nil r res where
  cata name _ _ = algZero name

instance algebraLen :: Algebra "length" lbl a Int Int where
  algebra _ _ _ c = c + 1

instance zeroLen :: Zero "length" Int where
  algZero _ = 0

recordLen
  :: forall row list
   . RowToList row list
  => Cata "length" list row Int
  => Record row -> Int
recordLen rec = cata (SProxy :: SProxy "length") (RLProxy :: RLProxy list) rec

type Res = Array (Tuple String String)

instance algebraShow ::
  ( Show a
  , IsSymbol lbl
  ) => Algebra "show" lbl a (Array (Tuple String String)) (Array (Tuple String String)) where
  algebra _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

instance zeroShow :: Zero "show" (Array (Tuple String String)) where
  algZero _ = []

recordShow
  :: forall row list
   . RowToList row list
  => Cata "show" list row Res
  => Record row -> Res
recordShow rec = cata (SProxy :: SProxy "show") (RLProxy :: RLProxy list) rec

instance algebraMapJust ::
  ( IsSymbol lbl
  , RowCons lbl (Maybe a) tail row
  , RowLacks lbl tail
  ) => Algebra "mapJust" lbl a (Record tail) (Record row) where
  algebra _ lbl val acc = insert lbl (Just val) acc

instance zeroMapJust :: Zero "mapJust" (Record ()) where
  algZero _ = {}

recordMapJust
  :: forall row list res
   . RowToList row list
  => Cata "mapJust" list row res
  => Record row -> res
recordMapJust r = cata (SProxy :: SProxy "mapJust") (RLProxy :: RLProxy list) r
