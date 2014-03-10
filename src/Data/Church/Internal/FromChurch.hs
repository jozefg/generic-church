{-# LANGUAGE TypeFamilies,          TypeOperators,     UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables,   RankNTypes,        DataKinds            #-}
{-# LANGUAGE PolyKinds #-}
module Data.Church.Internal.FromChurch where
import Data.Proxy
import GHC.Generics
import Data.Church.Internal.TF


type family Head (xs :: [k]) :: k
type instance Head (x ': xs) = x
type family Tail (xs :: [k]) :: [k]
type instance Tail (x ': xs) = xs

-- | Append for type level lists
type family Append (xs :: [k]) (ys :: [k]) :: [k]
type instance Append '[] ys = ys
type instance Append (x ': xs) ys = x ': Append xs ys

-- | Reverse for type level lists                      
type family Reverse (xs :: [k]) :: [k]
type instance Reverse '[] = '[]
type instance Reverse (x ': xs) = Append (Reverse xs) (x ': '[])

data Traverse a = Meta a a a | InL a a | InR a a | Term a

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
  update p (l :*: r) a = update (next p) l a :*: r
    where next :: forall xs. Proxy xs -> Proxy (Tail xs)
          next _ = Proxy
instance GUpdate rest (r p) => GUpdate (InR (l p) p ': rest) ((:*:) l r p) where
  update p (l :*: r) a = l :*: update (next p) r a 
    where next :: forall xs. Proxy xs -> Proxy (Tail xs)
          next _ = Proxy
instance GUpdate rest (f p) => GUpdate (Meta a b p ': rest) (M1 a b f p) where
  update p (M1 f) a = M1 (update (next p) f a)
    where next :: forall xs. Proxy xs -> Proxy (Tail xs)
          next _ = Proxy

type family Fill (paths :: [[Traverse *]]) r
type instance Fill (x ': xs) r = StripK (PathArg x) -> Fill xs r
type instance Fill '[] r = r

class GFill (paths :: [[Traverse *]]) a where
  fill :: Proxy paths -> (a -> r) -> a -> Fill paths r
instance GFill '[] a where
  fill _ f a = f a
instance (PathArg x ~ K1 m t p, StripK (PathArg x) ~ t, GUpdate x a, GFill xs a) =>
         GFill (x ': xs) a where
  fill p f a = \x -> fill (next p) f $ update (path p) a (K1 x)
    where next :: forall xs. Proxy xs -> Proxy (Tail xs)
          next _ = Proxy
          path :: forall xs. Proxy xs -> Proxy (Head xs)
          path _ = Proxy

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

type family PathArg (t :: [Traverse *])
type instance PathArg (Term a     ': '[] ) = a
type instance PathArg (Meta a b p ': rest) = PathArg rest
type instance PathArg (InR l p    ': rest) = PathArg rest
type instance PathArg (InL r p    ': rest) = PathArg rest

class GPath (p :: [Traverse *]) where
  path :: Proxy p -> PathArg p -> ReconstructPath p
instance GPath (Term a ': '[]) where
  path _ = id
instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
         => GPath (InR r p ': rest) where
  path p a = R1 (path (right p) a)
    where right :: forall a rest. Proxy (a ': rest) -> Proxy rest
          right _ = Proxy
instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
         => GPath (InL l p ': rest) where
  path p a = L1 (path (left p) a)
    where left :: forall a rest. Proxy (a ': rest) -> Proxy rest
          left _ = Proxy

instance ((WithoutParam (ReconstructPath rest)) p ~ ReconstructPath rest, GPath rest)
         => GPath (Meta a b p ': rest) where
  path p a = M1 (path (next p) a)
    where next :: forall a rest. Proxy (a ': rest) -> Proxy rest
          next _ = Proxy

class GBuild (paths :: [[Traverse *]])f r where
  build :: Proxy paths -> f -> r
-- Unit case. 
instance (ReconstructPath x ~ r, GPath x, GBuild xs f' r, PathArg x ~ U1 p)
         => GBuild (x ': xs) (r -> f') r where
  build p f = build (rest p) $ f (path (next p) U1)
    where next :: forall xs. Proxy xs -> Proxy (Head xs)
          next _ = Proxy
          rest :: forall xs. Proxy xs -> Proxy (Tail xs)
          rest _ = Proxy
instance ((f -> g) ~ Fill (MakeProdPaths (PathArg x) '[] '[]) r,
          ReconstructPath x ~ r, GEmpty (PathArg x), GBuild xs f' r,
          (GFill (MakeProdPaths (PathArg x) '[] '[]) (PathArg x)),
          GPath x)
         => GBuild (x ': xs) ((f -> g) -> f') r where
  build p f = build (rest p) $ f (fill (prod p) (path (next p)) empty)
    where next :: forall xs. Proxy xs -> Proxy (Head xs)
          next _ = Proxy
          rest :: forall xs. Proxy xs -> Proxy (Tail xs)
          rest _ = Proxy
          prod :: forall xs. Proxy xs -> Proxy (MakeProdPaths (PathArg (Head xs)) '[] '[])
          prod _ = Proxy
instance GBuild '[] r r where
  build _ f = f
