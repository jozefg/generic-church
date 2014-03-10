{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, UndecidableInstances #-}
module Data.Church.Internal.TF where
import GHC.Generics

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
