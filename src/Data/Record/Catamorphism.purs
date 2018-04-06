module Data.Record.Catamorphism where

import Prelude

import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.Record (get, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Algebra alg (lbl :: Symbol) val res | val -> res where
  algebra :: alg -> (SProxy lbl) -> val -> res

class Cata alg (list :: RowList) (row :: # Type) res | list -> res where
  cata :: alg -> (RLProxy list) -> Record row -> res

instance cataRowCons
  ::
  ( Algebra alg lbl val (fun a b)
  , IsSymbol lbl
  , RowCons lbl val rest row
  , Semigroupoid fun
  , Cata alg tail row (fun b c)
  ) => Cata alg (Cons lbl val tail) row (fun a c) where
  cata name _ record =
    let
      key = SProxy :: SProxy lbl
      tail = RLProxy :: RLProxy tail
      res = cata name tail record
    in algebra name key (get key record) >>> res

instance cataRowNil
  :: (Category fun) => Cata name Nil r (fun a a) where
  cata name _ _ = id

data LenAlgebra = LenAlgebra

instance lenAlgebra :: Algebra LenAlgebra lbl a (Int -> Int) where
  algebra _ _ _ z = z + 1

-- instance algebraLen :: Algebra "length" lbl a (Int -> Int) where
--   algebra _ _ _ z = z + 1

recordLen
  :: forall row list
   . RowToList row list
  => Cata LenAlgebra list row (Int -> Int)
  => Record row -> Int
recordLen rec = cata LenAlgebra (RLProxy :: RLProxy list) rec $ 0

type Res = Array (Tuple String String)

data ShowAlgebra = ShowAlgebra

instance algebraShow ::
  ( Show a
  , IsSymbol lbl
  ) => Algebra ShowAlgebra lbl a ((Array (Tuple String String)) -> (Array (Tuple String String))) where
  algebra _ sym val acc = cons (Tuple (reflectSymbol sym) (show val)) acc

recordShow
  :: forall row list
   . RowToList row list
  => Cata ShowAlgebra list row (Res -> Res)
  => Record row -> Res
recordShow rec = cata ShowAlgebra (RLProxy :: RLProxy list) rec $ []

-- {- This algebra allows us to insert each value into functorial container -}
data MapAlgebra (f :: Type -> Type) = MapAlgebra (forall a. a -> f a)

instance algebraMap ::
  ( IsSymbol lbl
  , RowCons lbl (f a) tail row
  , RowLacks lbl tail
  ) => Algebra (MapAlgebra f) lbl a ((Record tail) -> (Record row)) where
  algebra (MapAlgebra f) lbl val acc = insert lbl (f val) acc

recordMap
  :: forall f row list res
   . RowToList row list
  => Cata (MapAlgebra f) list row ({} -> res)
  => (forall a. a -> f a) -> Record row -> res
recordMap f r =
  let
    list = RLProxy :: RLProxy list
    res = cata (MapAlgebra f) list r $ {}
  in res

-- recordMapJust
--   :: forall row list res
--    . RowToList row list
--   => Cata "map" list row (Tuple (Maybe Unit) {} -> Tuple (Maybe Unit) res)
--   => Record row -> res
-- recordMapJust = recordMap (Just 1)

data ApplyAlgebra a = ApplyAlgebra a

instance algebraApply ::
  ( IsSymbol lbl
  , RowCons lbl b tail row
  , RowLacks lbl tail
  ) => Algebra (ApplyAlgebra a) lbl (a -> b) ((Record tail) -> (Record row)) where
  algebra (ApplyAlgebra c) lbl f acc = insert lbl (f c) acc

recordApplyTo
  :: forall a row list res
   . RowToList row list
  => Cata (ApplyAlgebra a) list row ({} -> res)
  => a -> Record row -> res
recordApplyTo v r =
  let
    list = RLProxy :: RLProxy list
    res = cata (ApplyAlgebra v) list r {}
  in res

-- instance algebraCollect ::
--   ( IsSymbol lbl
--   , RowCons lbl a tail row
--   , RowLacks lbl tail
--   , Apply f
--   ) => Algebra "collect" lbl (f a) (f (Record tail) -> (f (Record row))) where
--   algebra _ lbl a acc = (insert lbl) <$> a <*> acc

-- recordCollect
--   :: forall f row list res
--    . RowToList row list
--   => Applicative f
--   => Cata "collect" list row (f {} -> f res)
--   => Record row -> f res
-- recordCollect r =
--   let
--     name = SProxy :: SProxy "collect"
--     list = RLProxy :: RLProxy list
--     acc = pure {}
--     res = cata name list r $ acc
--   in res
