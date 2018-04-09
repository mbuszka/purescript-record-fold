module Data.Record.Fold where

import Prelude

import Data.Array (cons)
import Data.Record (get, insert)
import Data.Record.Builder (Builder, build)
import Data.Record.Builder as B
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
  :: (Category step) => Fold name Nil r (step a a) where
  fold name _ _ = id

data LenStep = LenStep

instance lenStep :: Step LenStep lbl a (Int -> Int) where
  step _ _ _ c = c + 1

recordLen
  :: forall row list
   . RowToList row list
  => Fold LenStep list row (Int -> Int)
  => Record row -> Int
recordLen rec = fold LenStep (RLProxy :: RLProxy list) rec $ 0

type Res = Array (Tuple String String)

data ShowStep = ShowStep

instance stepShow ::
  ( Show a
  , IsSymbol lbl
  ) => Step ShowStep lbl a ((Array (Tuple String String)) -> (Array (Tuple String String))) where
  step _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

recordShow
  :: forall row list
   . RowToList row list
  => Fold ShowStep list row (Res -> Res)
  => Record row -> Res
recordShow rec = fold ShowStep (RLProxy :: RLProxy list) rec $ []

data MapStep (f :: Type -> Type) = MapStep (forall a. a -> f a)

instance stepMap ::
  ( IsSymbol lbl
  , RowCons lbl (f a) tail row
  , RowLacks lbl tail
  ) => Step (MapStep f) lbl a (Builder (Record tail) (Record row)) where
  step (MapStep f) lbl val = B.insert lbl (f val)

recordMap
  :: forall f row list res
   . RowToList row list
  => Fold (MapStep f) list row (Builder {} (Record res))
  => (forall a. a -> f a) -> Record row -> Record res
recordMap f r =
  let
    list = RLProxy :: RLProxy list
    builder :: Builder {} (Record res)
    builder = fold (MapStep f) list r
    res :: Record res
    res = build builder {}
  in res

-- TODO Change to record builders

data ApplyStep a = ApplyStep a

instance stepApply ::
  ( IsSymbol lbl
  , RowCons lbl b tail row
  , RowLacks lbl tail
  ) => Step (ApplyStep a) lbl (a -> b) ((Record tail) -> (Record row)) where
  step (ApplyStep c) lbl f acc = insert lbl (f c) acc

recordApplyTo
  :: forall a row list res
   . RowToList row list
  => Fold (ApplyStep a) list row ({} -> res)
  => a -> Record row -> res
recordApplyTo v r =
  let
    list = RLProxy :: RLProxy list
    res = fold (ApplyStep v) list r {}
  in res

data CollectStep = CollectStep

instance stepCollect ::
  ( IsSymbol lbl
  , RowCons lbl a tail row
  , RowLacks lbl tail
  , Apply f
  ) => Step CollectStep lbl (f a) (f (Record tail) -> (f (Record row))) where
  step _ lbl a acc = (insert lbl) <$> a <*> acc

recordCollect
  :: forall f row list res
   . RowToList row list
  => Applicative f
  => Fold CollectStep list row (f {} -> f res)
  => Record row -> f res
recordCollect r =
  let
    list = RLProxy :: RLProxy list
    acc = pure {}
    res = fold CollectStep list r $ acc
  in res
