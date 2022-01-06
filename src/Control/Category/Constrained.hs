-- I don't want to import a whole package just for this.

-- Tried to keep it minimal, no crazy tricks
module Control.Category.Constrained where

import Data.Kind (Type)
import GHC.Exts (Constraint)
import Prelude hiding (Functor, Monad, id, (.))
import qualified Prelude

infixr 9 .

class Category (cat :: k -> k -> Type) where
  type Object cat (o :: k) :: Constraint
  type Object cat o = ()

  id :: Object cat a => cat a a

  (.) ::
    (Object cat a, Object cat b, Object cat c) =>
    cat b c ->
    cat a b ->
    cat a c

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)

-- newtype With (cat :: Type -> Type -> Type) (c :: Type -> Constraint) (a :: Type) (b :: Type) = With (cat a b)

-- instance (Category cat) => Category (With cat c) where
--   type Object (With cat c) o = (Object cat o, c o)
--   id = With id
--   (With f) . (With g) = With (f . g)

class Functor (dom :: k -> k -> Type) (cod :: k -> k -> Type) f where
  -- For convenience, to avoid extra newtypes
  type CodObj f (o :: k) :: Constraint
  type CodObj f o = ()

  fmap :: (Object dom a, Object dom b, Object cod (f a), Object cod (f a), CodObj f b) => dom a b -> cod (f a) (f b)

-- I don't care to factor this through Applicative. Only makes sense
-- for Cartesian categories anyway.
class (Functor cat cat m) => Monad (cat :: k -> k -> Type) m where
  return :: (Object cat a) => cat a (m a)
  join :: (Object cat a, CodObj m a) => cat (m (m a)) (m a)
