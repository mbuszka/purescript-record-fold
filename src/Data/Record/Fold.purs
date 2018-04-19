module Data.Record.Fold 
  ( class Step
  , class Fold
  , ApplyS
  , applyTo
  , AppCat
  , CollectS
  , collect
  , EqS
  , fold
  , FoldMapS
  , length
  , MapS
  , rEq
  , rMap
  , rShow
  , ShowS
  , step
  ) where

import Prelude

import Data.Array (cons)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
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


newtype FoldMapS m = FoldMapS (forall a. a -> m)

instance foldMapStep :: (Semigroup m) => Step (FoldMapS m) lbl a (m -> m) where
  step (FoldMapS f) _ a m = f a <> m

rFoldMap
   ::  forall m row list
  . Monoid m
  => Fold (FoldMapS m) list row (m -> m)
  => RowToList row list
  => (forall a. a -> m) -> Record row -> m
rFoldMap f r = fold (FoldMapS f) (RLProxy  ::  RLProxy list) r $ mempty

length
  :: forall row list
  . Fold (FoldMapS (Additive Int)) list row ((Additive Int) -> (Additive Int))
  => RowToList row list
  => (Record row) -> Int
length = unwrap <<< rFoldMap (const $ Additive 1)


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

newtype AppCat app cat a b = AppCat (app (cat a b))

instance semigroupoidAppCat :: (Semigroupoid cat, Applicative app) => Semigroupoid (AppCat app cat) where
  compose (AppCat a1) (AppCat a2) = AppCat $ (<<<) <$> a1 <*> a2

instance categoryAppCat :: (Category cat, Applicative app) => Category (AppCat app cat) where
  id = AppCat (pure id)

instance collectStep ::
  ( IsSymbol lbl
  , RowCons lbl a tail row
  , RowLacks lbl tail
  , Apply f
  ) => Step CollectS lbl (f a) (AppCat f Builder (Record tail) (Record row)) where
  step _ lbl a = AppCat $ insert lbl <$> a

collect
  :: forall f row list res
   . RowToList row list
  => Applicative f
  => Fold CollectS list row (AppCat f Builder {} (Record res))
  => Record row -> f (Record res)
collect r =
  let
    list = RLProxy :: RLProxy list
    AppCat builder = fold CollectS list r
    res = build <$> builder <*> pure {}
  in res

data EqS = EqS

instance eqStep ::
  ( RowCons lbl a r' r
  , IsSymbol lbl
  , Eq a
  ) => Step EqS lbl a (AppCat ((->) (Record r)) (->) Boolean Boolean) where
  step _ lbl val = AppCat \other res -> res && get lbl other == val

rEq
  :: forall row list
   . RowToList row list
  => Fold EqS list row (AppCat ((->) (Record row)) (->) Boolean Boolean)
  => Record row -> Record row -> Boolean
rEq r1 r2 =
  let
    list = RLProxy :: RLProxy list
    AppCat run = fold EqS list r1
  in
    run r2 true

