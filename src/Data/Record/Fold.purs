module Data.Record.Fold where

import Prelude

import Data.Array (cons)
import Data.Record (get)
import Data.Record.Builder (Builder, build, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Step stepper (lbl :: Symbol) val step | val -> step where
  step :: stepper -> (SProxy lbl) -> val -> step

class Fold stepper (list :: RowList) (row :: # Type) fold | list -> fold where
  fold :: stepper -> (RLProxy list) -> Record row -> fold

instance foldRowCons
  ::
  ( Step stepper lbl val (step a b)
  , IsSymbol lbl
  , RowCons lbl val rest row
  , Semigroupoid step
  , Fold stepper tail row (step b c)
  ) => Fold stepper (Cons lbl val tail) row (step a c) where
  fold name _ record =
    let
      key = SProxy :: SProxy lbl
      tail = RLProxy :: RLProxy tail
      res = fold name tail record
    in step name key (get key record) >>> res

instance foldRowNil
  :: 
  ( Category step
  ) => Fold name Nil r (step a a) where
  fold name _ _ = id


data LenS = LenS

instance lenStep :: Step LenS lbl a (Int -> Int) where
  step _ _ _ c = c + 1

length
  :: forall row list
   . RowToList row list
  => Fold LenS list row (Int -> Int)
  => Record row -> Int
length rec = fold LenS (RLProxy :: RLProxy list) rec $ 0


type Res = Array (Tuple String String)
data ShowS = ShowS

instance showStep ::
  ( Show a
  , IsSymbol lbl
  ) => Step ShowS lbl a ((Array (Tuple String String)) -> (Array (Tuple String String))) where
  step _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

rShow
  :: forall row list
   . RowToList row list
  => Fold ShowS list row (Res -> Res)
  => Record row -> Res
rShow rec = fold ShowS (RLProxy :: RLProxy list) rec $ []


data MapS (f :: Type -> Type) = MapS (forall a. a -> f a)

instance mapStep ::
  ( IsSymbol lbl
  , RowCons lbl (f a) tail row
  , RowLacks lbl tail
  ) => Step (MapS f) lbl a (Builder (Record tail) (Record row)) where
  step (MapS f) lbl val = insert lbl (f val)

rMap
  :: forall f row list res
   . RowToList row list
  => Fold (MapS f) list row (Builder {} (Record res))
  => (forall a. a -> f a) -> Record row -> Record res
rMap f r =
  let
    list = RLProxy :: RLProxy list
    builder = fold (MapS f) list r
    res = build builder {}
  in res


data ApplyS a = ApplyS a

instance applyStep ::
  ( IsSymbol lbl
  , RowCons lbl b tail row
  , RowLacks lbl tail
  ) => Step (ApplyS a) lbl (a -> b) (Builder (Record tail) (Record row)) where
  step (ApplyS c) lbl f = insert lbl (f c)

applyTo
  :: forall a row list res
   . RowToList row list
  => Fold (ApplyS a) list row (Builder {} (Record res))
  => a -> Record row -> Record res
applyTo v r =
  let
    list = RLProxy :: RLProxy list
    res = build (fold (ApplyS v) list r) {}
  in res


data CollectS = CollectS
newtype BuilderWrapper f a b = BW (f (Builder a b))

instance semigroupoidBuilderWrapper :: Apply f => Semigroupoid (BuilderWrapper f) where
  compose (BW g) (BW f) = BW $ compose <$> g <*> f

instance categoryBuilderWrapper :: Applicative f => Category (BuilderWrapper f) where
  id = BW $ pure id
  

instance collectStep ::
  ( IsSymbol lbl
  , RowCons lbl a tail row
  , RowLacks lbl tail
  , Apply f
  ) => Step CollectS lbl (f a) (BuilderWrapper f (Record tail) (Record row)) where
  step _ lbl a = BW $ insert lbl <$> a

collect
  :: forall f row list res
   . RowToList row list
  => Applicative f
  => Fold CollectS list row (BuilderWrapper f {} (Record res))
  => Record row -> f (Record res)
collect r =
  let
    list = RLProxy :: RLProxy list
    BW builder = fold CollectS list r
    res = build <$> builder <*> pure {}
  in res
