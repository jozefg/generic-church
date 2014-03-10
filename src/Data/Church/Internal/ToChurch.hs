{-# LANGUAGE TypeFamilies,          TypeOperators,     UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables                                            #-}
module Data.Church.Internal.ToChurch where
import Data.Proxy
import GHC.Generics

import Data.Church.Internal.TF

-- | Eliminate a product type
type family ChurchProd v c
type instance ChurchProd (K1 a t p)    c = t -> c
type instance ChurchProd (U1 p)        c = c
type instance ChurchProd ((:*:) l r p) c = ChurchProd (l p) (ChurchProd (r p) c)
type instance ChurchProd (ListTerm p)  c = c


type family ToListProd v rest
type instance ToListProd ((:*:) l r' p) r = ToListProd (l p) (ToListProd (r' p) r)
type instance ToListProd (K1 a t p)     r = (K1 a t     :*: WithoutParam r) p
type instance ToListProd (U1 p)         r = U1 p

class GListProd a r where
  toListProd :: a -> r -> ToListProd a r
instance (WithoutParam r) p ~ r => GListProd (U1 p) r where
  toListProd = const
instance (WithoutParam r) p ~ r => GListProd (K1 a t p) r where
  toListProd = (:*:)
instance (GListProd (l p) (ToListProd (r' p) r), GListProd (r' p) r) => GListProd ((:*:) l r' p) r where
  toListProd (l :*: r) rest = toListProd l (toListProd r rest)
class GChurchProd a where
  prod :: Proxy r -> a -> ChurchProd a r -> r -- Proxy needed for GChurchSum
instance GChurchProd (U1 p) where
  prod _ _ f = f
instance GChurchProd (K1 a t p) where
  prod _ (K1 r) f = f r
instance GChurchProd (r p) => GChurchProd ((:*:) (K1 a t) r p) where
  prod p (K1 l :*: r) f = prod p r (f l)
instance GChurchProd (ListTerm p) where
  prod _ _ f = f

type family ToList v rest
type instance ToList ((:+:) l r' p) r = ToList (l p) (ToList (r' p) r)
type instance ToList (K1 a t p)     r = (K1 a t     :+: WithoutParam r) p
type instance ToList ((:*:) l r' p) r = ((l :*: r') :+: WithoutParam r) p
type instance ToList (U1 p)         r = (U1         :+: WithoutParam r) p

class GList a r where
  toList :: Maybe a -> r -> ToList a r
instance (WithoutParam r) p ~ r => GList (U1 p) r where
  toList Nothing  r = R1 r
  toList (Just a) _ = L1 a
instance (WithoutParam r) p ~ r => GList (K1 a t p) r where
  toList Nothing  r = R1 r
  toList (Just a) _ = L1 a
instance (WithoutParam r) p ~ r => GList ((l :*: r') p) r where
  toList Nothing  r = R1 r
  toList (Just a) _ = L1 a
instance (GList (l p) (ToList (r' p) r),
          GList (r' p) r) => GList ((l :+: r') p) r where
  toList (Just sum@(L1 l)) r = toList (Just l) (toList (rNot sum) r)
    where rNot :: forall l r p. (l :+: r) p -> Maybe (r p)
          rNot _ = Nothing
  toList (Just sum@(R1 r')) r = toList (lNot sum) (toList (Just r') r)
    where lNot :: forall l r p. (l :+: r) p -> Maybe (l p)
          lNot _ = Nothing
  toList m r = toList (lNot m) (toList (rNot m) r)
    where lNot :: forall l r p. Maybe ((:+:) l r p) -> Maybe (l p)
          lNot _ = Nothing
          rNot :: forall l r p. Maybe ((:+:) l r p) -> Maybe (r p)
          rNot _ = Nothing


type family ChurchSum v c
type instance ChurchSum ((:+:) l r p) c = ChurchProd (ToListProd (l p) (ListTerm ())) c -> ChurchSum (r p) c
type instance ChurchSum (ListTerm p) c  = c

class Swallow a where
  swallow :: Proxy a -> c -> ChurchSum a c
instance Swallow (ListTerm p) where
  swallow _ c = c
instance Swallow (r p) => Swallow ((:+:) l r p) where
  swallow p c = \_ -> swallow (right p) c
    where right :: forall l r p. Proxy ((:+:) l r p) -> Proxy (r p)
          right _ = Proxy

class GChurchSum a r where
  elim :: Proxy r -> a -> ChurchSum a r -- Proxy because type inference is stubborn
instance (GListProd (l p) (ListTerm ()), GChurchProd (ToListProd (l p) (ListTerm ())),
          GChurchSum (r' p) r, Swallow (r' p)) =>
         GChurchSum ((:+:) l r' p) r where
  elim p sum@(L1 l) = \f -> swallow (right sum) (prod p (toListProd l (ListTerm :: ListTerm ())) f)
    where right :: forall l r p. (:+:) l r p -> Proxy (r p)
          right _ = Proxy
  elim p (R1 r) = \_ -> elim p r
instance GChurchSum (ListTerm p) r where
  elim _ _ = error "Malformed generic instance"

from' :: Generic a => a -> Rep a ()
from' = from
