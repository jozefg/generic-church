{-# LANGUAGE TypeFamilies, TypeOperators,        FlexibleContexts #-}
{-# LANGUAGE DataKinds,    UndecidableInstances, PolyKinds        #-}
module Data.Church.Internal.TF where
import GHC.Generics
import Data.Proxy

-- | This is isomorphic to @()@ with a more interesting
-- kind. It's used to annotate the end of pseudolists formed
-- by combining @:*:@ and @:+:@'s.
data ListTerm p = ListTerm

-- | 'GHC.Generic' annotates types with an extra type parameter
-- this type family removes that extra parameter and returns
-- the constructor of kind @* -> *@
type family WithoutParam v :: * -> *
type instance WithoutParam ((:+:) l r p) = l :+: r
type instance WithoutParam ((:*:) l r p) = l :*: r
type instance WithoutParam (U1 p)        = U1
type instance WithoutParam (M1 a b f p)  = M1 a b f
type instance WithoutParam (K1 a t p)    = K1 a t
type instance WithoutParam (ListTerm p)  = ListTerm


-- | Remove the meta information (@M1@ constructors) from
-- a type.
type family StripMeta v
type instance StripMeta (M1 a b f p)  = StripMeta (f p)
type instance StripMeta (K1 a t p)    = K1 a t p
type instance StripMeta ((:+:) l r p) =
  (:+:) (WithoutParam (StripMeta (l p))) (WithoutParam (StripMeta (r p))) p
type instance StripMeta ((:*:) l r p) =
  (:*:) (WithoutParam (StripMeta (l p))) (WithoutParam (StripMeta (r p))) p
type instance StripMeta (U1 p)        = U1 p

class GStripMeta a where
  stripMeta :: a -> StripMeta a
instance GStripMeta (f p) => GStripMeta (M1 a b f p) where
  stripMeta (M1 f) = stripMeta f
instance GStripMeta (K1 a t p) where
  stripMeta = id
instance GStripMeta (U1 p) where
  stripMeta = id
instance (GStripMeta (l p), GStripMeta (r p),
          (WithoutParam (StripMeta (l p))) p ~ StripMeta (l p),
          (WithoutParam (StripMeta (r p))) p ~ StripMeta (r p)) =>
         GStripMeta ((:*:) l r p) where
  stripMeta (l :*: r) = stripMeta l :*: stripMeta r
instance (GStripMeta (l p), GStripMeta (r p),
          (WithoutParam (StripMeta (l p))) p ~ StripMeta (l p),
          (WithoutParam (StripMeta (r p))) p ~ StripMeta (r p)) =>
         GStripMeta ((:+:) l r p) where
  stripMeta (L1 l) = L1 $ stripMeta l
  stripMeta (R1 r) = R1 $ stripMeta r

-- | Strip away the extra information that annotates
-- leaves in GHC.Generics
type family StripK v
type instance StripK (K1 a t p) = t

type family Head (xs :: [k]) :: k
type instance Head (x ': xs) = x
type family Tail (xs :: [k]) :: [k]
type instance Tail (x ': xs) = xs

pHead :: Proxy xs -> Proxy (Head xs)
pHead = reproxy
pTail :: Proxy xs -> Proxy (Tail xs)
pTail = reproxy

-- | Append for type level lists
type family Append (xs :: [k]) (ys :: [k]) :: [k]
type instance Append '[] ys = ys
type instance Append (x ': xs) ys = x ': Append xs ys

-- | Reverse for type level lists                      
type family Reverse (xs :: [k]) :: [k]
type instance Reverse '[] = '[]
type instance Reverse (x ': xs) = Append (Reverse xs) (x ': '[])
