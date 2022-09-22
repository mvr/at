{-# LANGUAGE RebindableSyntax #-}
-- I don't want to import a whole package just for this.

-- Tried to keep it minimal, no crazy tricks
module Control.Category.Constrained where

import qualified Control.Monad (join)
import Data.Kind (Type)
import GHC.Exts (Constraint)
import Prelude hiding (Functor, Monad, Traversable, fmap, id, (.), (<$>), (>>=), (>>), return)
import qualified Prelude

infixr 9 .

class Semigroupoid (cat :: i -> i -> Type) where
  type Object cat (o :: i) :: Constraint
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

newtype Sub (con :: i -> Constraint) (c :: i -> i -> Type) a b = Sub {incl :: c a b}

instance Semigroupoid c => Semigroupoid (Sub con c) where
  type Object (Sub con c) o = (Object c o, con o)
  (Sub g) . (Sub f) = Sub (g . f)

instance Category c => Category (Sub con c) where
  id = Sub id

class Functor (dom :: i -> i -> Type) (cod :: j -> j -> Type) (f :: i -> j) where
  fmap :: (Object dom a, Object dom b) => dom a b -> cod (f a) (f b)

(<$>) :: (Functor dom cod f, Object dom a, Object dom b) => dom a b -> cod (f a) (f b)
(<$>) = fmap
infixl 4 <$>

newtype Wrapped (f :: Type -> Type) a = Wrapped { unwrap :: f a }

instance (Prelude.Functor f) => Functor (->) (->) (Wrapped f) where
  fmap f (Wrapped a) = Wrapped (Prelude.fmap f a)

deriving via (Wrapped []) instance Functor (->) (->) []

-- I don't care to factor this through Applicative. Only makes sense
-- for Cartesian categories anyway.
class Functor cat cat m => Monad cat m where
  return :: Object cat a => cat a (m a)
  join :: Object cat a => cat (m (m a)) (m a)

  default return :: (cat ~ (->), Prelude.Monad m) => cat a (m a)
  return = Prelude.return

  default join :: (cat ~ (->), Prelude.Monad m) => cat (m (m a)) (m a)
  join = Control.Monad.join

instance (Prelude.Monad m) => Monad (->) (Wrapped m) where
  return a = Wrapped (Prelude.return a)
  join (Wrapped mma) = Wrapped $ Control.Monad.join (Prelude.fmap unwrap mma)

deriving via (Wrapped []) instance Monad (->) []
(>>=) :: (Monad (->) m) => m a -> (a -> m b) -> m b
a >>= f = join (fmap f a)
