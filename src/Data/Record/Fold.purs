module Data.Record.Fold
  ( class Fold
  , class RFold
  , class Step
  , AppCat
  , ApplyS
  , applyTo
  , collect
  , CollectS
  , EqS
  , fold
  , LenS
  , length
  , MapS
  , rEq
  , rFold
  , rMap
  , rShow
  , ShowS
  , step
  ) where

import Prelude

import Data.Array (cons)
import Record (get)
import Record.Builder (Builder, build, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Lacks, class Cons)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Step stepper (lbl :: Symbol) val step | val -> step where
  step :: stepper -> (SProxy lbl) -> val -> step

class RFold stepper (list :: RowList) (row :: # Type) fold | list -> fold where
  rFold :: stepper -> (RLProxy list) -> Record row -> fold

instance rFoldRowCons
  ::
  ( Step stepper lbl val (step a b)
  , IsSymbol lbl
  , Cons lbl val rest row
  , Semigroupoid step
  , RFold stepper tail row (step b c)
  ) => RFold stepper (Cons lbl val tail) row (step a c) where
  rFold name _ record =
    let
      key = SProxy :: SProxy lbl
      tail = RLProxy :: RLProxy tail
      res = rFold name tail record
    in step name key (get key record) >>> res

instance rFoldRowNil
  ::
  ( Category step
  ) => RFold name Nil r (step a a) where
  rFold name _ _ = identity

class Fold stepper a fold | stepper a -> fold where
  fold :: stepper -> a -> fold

instance foldRecord
  ::
  ( RowToList row list
  , RFold stepper list row fold
  ) => Fold stepper (Record row) fold where
  fold stepper = rFold stepper (RLProxy :: RLProxy list)

data LenS = LenS

instance lenStep :: Step LenS lbl a (Int -> Int) where
  step _ _ _ c = c + 1

length
  :: forall r
   . Fold LenS r (Int -> Int)
  => r -> Int
length rec = fold LenS rec $ 0


type Res = Array (Tuple String String)
data ShowS = ShowS

instance showStep ::
  ( Show a
  , IsSymbol lbl
  ) => Step ShowS lbl a ((Array (Tuple String String)) -> (Array (Tuple String String))) where
  step _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

rShow
  :: forall r
   . Fold ShowS r (Res -> Res)
  => r -> Res
rShow rec = fold ShowS rec $ []


data MapS (f :: Type -> Type) = MapS (forall a. a -> f a)

instance mapStep ::
  ( IsSymbol lbl
  , Cons lbl (f a) tail row
  , Lacks lbl tail
  ) => Step (MapS f) lbl a (Builder (Record tail) (Record row)) where
  step (MapS f) lbl val = insert lbl (f val)

rMap
  :: forall f r res
   . Fold (MapS f) r (Builder {} (Record res))
  => (forall a. a -> f a) -> r -> Record res
rMap f r =
  let
    builder = fold (MapS f) r
    res = build builder {}
  in res


data ApplyS a = ApplyS a

instance applyStep ::
  ( IsSymbol lbl
  , Cons lbl b tail row
  , Lacks lbl tail
  ) => Step (ApplyS a) lbl (a -> b) (Builder (Record tail) (Record row)) where
  step (ApplyS c) lbl f = insert lbl (f c)

applyTo
  :: forall a row list res
   . RowToList row list
  => RFold (ApplyS a) list row (Builder {} (Record res))
  => a -> Record row -> Record res
applyTo v r =
  let
    list = RLProxy :: RLProxy list
    res = build (rFold (ApplyS v) list r) {}
  in res


data CollectS = CollectS

newtype AppCat app cat a b = AppCat (app (cat a b))

instance semigroupoidAppCat :: (Semigroupoid cat, Applicative app) => Semigroupoid (AppCat app cat) where
  compose (AppCat a1) (AppCat a2) = AppCat $ (<<<) <$> a1 <*> a2

instance categoryAppCat :: (Category cat, Applicative app) => Category (AppCat app cat) where
  identity = AppCat (pure identity)

instance collectStep ::
  ( IsSymbol lbl
  , Cons lbl a tail row
  , Lacks lbl tail
  , Apply f
  ) => Step CollectS lbl (f a) (AppCat f Builder (Record tail) (Record row)) where
  step _ lbl a = AppCat $ insert lbl <$> a

collect
  :: forall f r res
   . Applicative f
  => Fold CollectS r (AppCat f Builder {} (Record res))
  => r -> f (Record res)
collect r =
  let
    AppCat builder = fold CollectS r
    res = build <$> builder <*> pure {}
  in res

data EqS = EqS

instance eqStep ::
  ( Cons lbl a r' r
  , IsSymbol lbl
  , Eq a
  ) => Step EqS lbl a (AppCat ((->) (Record r)) (->) Boolean Boolean) where
  step _ lbl val = AppCat \other res -> res && get lbl other == val

rEq
  :: forall r
   . Fold EqS r (AppCat ((->) r) (->) Boolean Boolean)
  => r -> r -> Boolean
rEq r1 r2 =
  let
    AppCat run = fold EqS r1
  in
    run r2 true

