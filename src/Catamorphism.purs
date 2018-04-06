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

class Cata (name :: Symbol) (list :: RowList) (row :: # Type) z res | name -> res z where
  cata :: (SProxy name) -> (RLProxy list) -> z -> Record row -> res

instance cataRowCons
  ::
  ( Algebra name lbl input res' res
  , IsSymbol lbl
  , RowCons lbl input rest row
  , Cata name tail row z res'
  ) => Cata name (Cons lbl input tail) row z res where
  cata name _ z record =
    let
      key = SProxy :: SProxy lbl
      tail = RLProxy :: RLProxy tail
      res = cata name tail z record
    in algebra name key (get key record) res

instance cataRowNil
  :: Cata name Nil r z z where
  cata name _ z _ = z

instance algebraLen :: Algebra "length" lbl a Int Int where
  algebra _ _ _ c = c + 1

recordLen
  :: forall row list
   . RowToList row list
  => Cata "length" list row Int Int
  => Record row -> Int
recordLen rec = cata (SProxy :: SProxy "length") (RLProxy :: RLProxy list) 0 rec

type Res = Array (Tuple String String)

instance algebraShow ::
  ( Show a
  , IsSymbol lbl
  ) => Algebra "show" lbl a (Array (Tuple String String)) (Array (Tuple String String)) where
  algebra _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

recordShow
  :: forall row list
   . RowToList row list
  => Cata "show" list row Res Res
  => Record row -> Res
recordShow rec = cata (SProxy :: SProxy "show") (RLProxy :: RLProxy list) [] rec

{- This algebra allows us to insert each value into functorial container -}
instance algebraMap ::
  ( IsSymbol lbl
  , RowCons lbl (f a) tail row
  , RowLacks lbl tail
  , Functor f
  ) => Algebra "map" lbl a (Tuple (f Unit) (Record tail)) (Tuple (f Unit) (Record row)) where
  algebra _ lbl val (Tuple c acc) = Tuple c $ insert lbl (map (const val) c) acc

recordMap
  :: forall f a row list res
   . RowToList row list
  => Functor f
  => Cata "map" list row (Tuple (f Unit) {}) (Tuple (f Unit) res)
  => f a -> Record row -> res
recordMap proxy r =
  let
    name = SProxy :: SProxy "map"
    list = RLProxy :: RLProxy list
    acc = Tuple (map (const unit) proxy) {}
    Tuple _ res = cata name list acc r
  in res

recordMapJust
  :: forall row list res
   . RowToList row list
  => Cata "map" list row (Tuple (Maybe Unit) {}) (Tuple (Maybe Unit) res)
  => Record row -> res
recordMapJust = recordMap (Just 1)

instance algebraApply ::
  ( IsSymbol lbl
  , RowCons lbl b tail row
  , RowLacks lbl tail
  ) => Algebra "apply" lbl (a -> b) (Tuple a (Record tail)) (Tuple a (Record row)) where
  algebra _ lbl f (Tuple c acc) = Tuple c $ insert lbl (f c) acc

recordApplyTo
  :: forall a row list res
   . RowToList row list
  => Cata "apply" list row (Tuple a {}) (Tuple a res)
  => a -> Record row -> res
recordApplyTo v r =
  let
    name = SProxy :: SProxy "apply"
    list = RLProxy :: RLProxy list
    acc = Tuple v {}
    Tuple _ res = cata name list acc r
  in res

instance algebraCollect ::
  ( IsSymbol lbl
  , RowCons lbl a tail row
  , RowLacks lbl tail
  , Apply f
  ) => Algebra "collect" lbl (f a) (f (Record tail)) (f (Record row)) where
  algebra _ lbl a acc = (insert lbl) <$> a <*> acc

recordCollect
  :: forall f row list res
   . RowToList row list
  => Applicative f
  => Cata "collect" list row (f {}) (f res)
  => Record row -> f res
recordCollect r =
  let
    name = SProxy :: SProxy "collect"
    list = RLProxy :: RLProxy list
    acc = pure {}
    res = cata name list acc r
  in res
