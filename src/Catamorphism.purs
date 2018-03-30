module Catamorphism where

import Prelude

import Data.Array (cons)
import Data.Record (delete, get)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Type.Row (class ListToRow, class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Algebra (name :: Symbol) (input :: Type) (res :: Type) | name -> res where
  algebra :: forall sym. IsSymbol sym => (SProxy name) -> (SProxy sym) -> input -> res -> res

class Cata (name :: Symbol) (list :: RowList) (row :: # Type) res | name -> res where
  cata :: (SProxy name) -> (RLProxy list) -> res -> Record row -> res

instance cataRowCons
  ::
  ( Algebra name input res
  , IsSymbol lbl
  , RowCons lbl input rest row
  , Cata name tail row res
  ) => Cata name (Cons lbl input tail) row res where
  cata name _ zero record =
    let
      key = SProxy :: SProxy lbl
      tail = RLProxy :: RLProxy tail
      res = cata name tail zero record
    in algebra name key (get key record) res

instance cataRowNil
  :: Cata name Nil r res where
  cata _ _ zero _ = zero

instance algebraLen :: Algebra "length" a Int where
  algebra _ _ _ c = c + 1

recordLen :: forall row list. RowToList row list => Cata "length" list row Int => Record row -> Int
recordLen rec = cata (SProxy :: SProxy "length") (RLProxy :: RLProxy list) 0 rec

type Res = Array (Tuple String String)

instance algebraShow :: Show a => Algebra "show" a (Array (Tuple String String)) where
  algebra _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

recordShow :: forall row list. RowToList row list => Cata "show" list row Res => Record row -> Res
recordShow rec = cata (SProxy :: SProxy "show") (RLProxy :: RLProxy list) [] rec
