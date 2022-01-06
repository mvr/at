-- I don't want to import a whole package just for this
module Control.Category.Constrained where

import Data.Kind (Type)
import GHC.Exts (Constraint)
import Prelude hiding (id, (.))
import qualified Prelude

class Category (cat :: k -> k -> Type) where
  type Object cat (o :: k) :: Constraint
  type Object cat o = ()
  id :: Object cat a => cat a a
  (.) ::
    (Object cat a, Object cat b, Object cat c) =>
    cat b c ->
    cat a b ->
    cat a c

infixr 9 .

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)
