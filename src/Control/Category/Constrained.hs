-- I don't want to import a whole package just for this.

-- Tried to keep it minimal, no crazy tricks
module Control.Category.Constrained where

import qualified Control.Monad
import Data.Kind (Type)
import GHC.Exts (Constraint)
import Prelude hiding (Functor, Monad, fmap, id, (.), (<$>))
import qualified Prelude

infixr 9 .

class Semigroupoid (cat :: Type -> Type -> Type) where
  type Object cat (o :: Type) :: Constraint
  type Object cat o = ()

  (.) ::
    (Object cat a, Object cat b, Object cat c) =>
    cat b c ->
    cat a b ->
    cat a c

class (Semigroupoid cat) => Category (cat :: Type -> Type -> Type) where
  id :: Object cat a => cat a a

instance Semigroupoid (->) where
  (.) = (Prelude..)

instance Category (->) where
  id = Prelude.id

-- newtype With (cat :: Type -> Type -> Type) (c :: Type -> Constraint) (a :: Type) (b :: Type) = With (cat a b)

-- instance (Category cat) => Category (With cat c) where
--   type Object (With cat c) o = (Object cat o, c o)
--   id = With id
--   (With f) . (With g) = With (f . g)

class Functor dom cod f where
  -- For convenience, to avoid some unpleasant newtypes in practice.
  -- This would really be equivalent to something like
  -- `Functor dom (cod `With` CodObj f) f`
  type CodObj f (o :: Type) :: Constraint
  type CodObj f o = ()

  fmap :: (Object dom a, Object dom b, Object cod (f a), Object cod (f a), CodObj f b) => dom a b -> cod (f a) (f b)
  default fmap :: (dom ~ (->), cod ~ (->), Prelude.Functor f) => dom a b -> cod (f a) (f b)
  fmap = Prelude.fmap

cfmap ::
  ( Object dom a,
    Object dom b,
    Object cod (f a),
    CodObj f b,
    Functor dom cod f
  ) =>
  dom a b ->
  cod (f a) (f b)
cfmap = fmap

-- (<$>) :: (Functor dom cod f, Object dom a, Object dom b, Object cod (f a), Object cod (f a), CodObj f b) => dom a b -> cod (f a) (f b)
-- (<$>) = fmap
-- infixl 4 <$>

-- I don't care to factor this through Applicative. Only makes sense
-- for Cartesian categories anyway.
class (Functor cat cat m) => Monad (cat :: Type -> Type -> Type) m where
  return :: (Object cat a) => cat a (m a)
  join :: (Object cat a, CodObj m a) => cat (m (m a)) (m a)

  default return :: (cat ~ (->), Prelude.Monad m) => cat a (m a)
  return = Prelude.return
  default join :: (cat ~ (->), Prelude.Monad m) => cat (m (m a)) (m a)
  join = Control.Monad.join

(>>=) :: (Monad (->) m, CodObj m b, CodObj m (m b)) => m a -> (a -> m b) -> m b
a >>= f = join (fmap f a)
