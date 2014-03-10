{-# LANGUAGE TypeFamilies,          TypeOperators,     UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables,   PolyKinds,         DataKinds            #-}
module Data.Church.Internal.FromChurch where
import Data.Proxy
import GHC.Generics
import Data.Church.Internal.TF

data Traverse a = Meta a a a | InL a a | InR a a | Term a

type family PathArg (t :: [Traverse *])
type instance PathArg (Term a     ': '[] ) = a
type instance PathArg (Meta a b p ': rest) = PathArg rest
type instance PathArg (InR l p    ': rest) = PathArg rest
type instance PathArg (InL r p    ': rest) = PathArg rest

-- | Construct a product type where all the fields are
-- _|_
class GEmpty a where
  empty :: a
instance GEmpty (U1 p) where
  empty = U1
instance GEmpty (K1 a t p) where
  empty = K1 (error "generic-church: Error! The impossible has happened.")
instance GEmpty (f p) => GEmpty (M1 a b f p) where
  empty = M1 empty
instance (GEmpty (l p), GEmpty (r p)) => GEmpty ((:*:) l r p) where
  empty = empty :*: empty

type family MakeProdPaths v (m :: [Traverse *]) (r :: [ [Traverse *] ]) :: [[Traverse *]]
type instance MakeProdPaths (K1 a t p) s all    = Reverse (Term (K1 a t p) ': s) ': all
type instance MakeProdPaths (M1 a b f p) s all  = MakeProdPaths (f p) (Meta a b p ': s) all
type instance MakeProdPaths ((:*:) l r p) s all =
  Append (MakeProdPaths (l p) (InL (r p) p ': s) '[])
          (Append (MakeProdPaths (r p) (InR (l p) p ': s) '[]) all)

class GUpdate (path :: [Traverse *]) a where
  update :: Proxy path -> a -> PathArg path -> a
instance GUpdate (Term (K1 a t p) ': '[]) (K1 a t p) where
  update _ _ a = a
instance GUpdate rest (l p) => GUpdate (InL (r p) p ': rest) ((:*:) l r p) where
  update p (l :*: r) a = update (pTail p) l a :*: r
instance GUpdate rest (r p) => GUpdate (InR (l p) p ': rest) ((:*:) l r p) where
  update p (l :*: r) a = l :*: update (pTail p) r a 
instance GUpdate rest (f p) => GUpdate (Meta a b p ': rest) (M1 a b f p) where
  update p (M1 f) a = M1 (update (pTail p) f a)

type family Fill (paths :: [[Traverse *]]) r
type instance Fill (x ': xs) r = StripK (PathArg x) -> Fill xs r
type instance Fill '[] r = r

class GFill (paths :: [[Traverse *]]) a where
  fill :: Proxy paths -> (a -> r) -> a -> Fill paths r
instance GFill '[] a where
  fill _ f a = f a
instance (PathArg x ~ K1 m t p, StripK (PathArg x) ~ t, GUpdate x a, GFill xs a) =>
         GFill (x ': xs) a where
  fill p f a = \x -> fill (pTail p) f $ update (pHead p) a (K1 x)

type family MakePaths v (m :: [Traverse *]) (r :: [ [Traverse *] ]) :: [[Traverse *]]
type instance MakePaths ((:+:) l r p) s all =
  Append (MakePaths (l p) (InL (r p) p ': s) '[])
          (Append (MakePaths (r p) (InR (l p) p ': s) '[]) all)
type instance MakePaths (M1 a b f p) s all  =
  MakePaths (f p) (Meta a b p ': s) all
type instance MakePaths (K1 a t p) s all    =  Reverse (Term (K1 a t p) ': s)    ': all
type instance MakePaths (U1 p) s all        =  Reverse (Term (U1 p) ': s)        ': all
type instance MakePaths ((:*:) l r p) s all =  Reverse (Term ((:*:) l r p) ': s) ': all

type family ReconstructPath (t :: [Traverse *])
type instance ReconstructPath (InL r p  ': rest) =
  (WithoutParam (ReconstructPath rest) :+: WithoutParam r) p
type instance ReconstructPath (InR l p  ': rest) =
  (WithoutParam l :+: WithoutParam (ReconstructPath rest)) p
type instance ReconstructPath (Meta a b p ': rest) = M1 a b (WithoutParam (ReconstructPath rest)) p
type instance ReconstructPath (Term a     ': '[])  = a

class GPath (p :: [Traverse *]) where
  path :: Proxy p -> PathArg p -> ReconstructPath p
instance GPath (Term a ': '[]) where
  path _ = id
instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
         => GPath (InR r p ': rest) where
  path p a = R1 $ path (pTail p) a
instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
         => GPath (InL l p ': rest) where
  path p a = L1 $ path (pTail p) a
instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
         => GPath (Meta a b p ': rest) where
  path p a = M1 $ path (pTail p) a

class GBuild (paths :: [[Traverse *]])f r where
  build :: Proxy paths -> f -> r

-- | Unit case. This represents constructors with no arguments
instance (ReconstructPath x ~ r, GPath x, GBuild xs f' r, PathArg x ~ U1 p)
         => GBuild (x ': xs) (r -> f') r where
  build p f = build (pTail p) $ f (path (pHead p) U1)
instance ((f -> g) ~ Fill (MakeProdPaths (PathArg x) '[] '[]) r,
          ReconstructPath x ~ r, GEmpty (PathArg x), GBuild xs f' r,
          (GFill (MakeProdPaths (PathArg x) '[] '[]) (PathArg x)),
          GPath x)
         => GBuild (x ': xs) ((f -> g) -> f') r where
  build p f = build (pTail p) $ f (fill (prod p) (path (pHead p)) empty)
    where
          prod :: forall xs. Proxy xs -> Proxy (MakeProdPaths (PathArg (Head xs)) '[] '[])
          prod _ = Proxy
instance GBuild '[] r r where
  build _ f = f
