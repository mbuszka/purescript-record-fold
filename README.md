# purescript-record-fold

This package provides typeclasses for creating typed, generic folds over records.

As an example it also provides few utility functions:
  - `applyTo` applies record of functions to a value
  - `collect` collect record of `Applicative` to an `Applicative` containing record of values
  - `length` computes amount of fields in record
  - `rMap` maps type constructor over record
  - `rShow` transforms record with `Show`-able fields into `Array (Tuple String String)` (types are subject to change)

## Defining new folds
In order to define a new fold, first define `Stepper` datatype which will be a proxy determining
which instance of `Step` should be used by `Fold`. The `Stepper` must implement `Step` class.
Please note that the result of `step` function must be a `Category` for `Fold` to work.
