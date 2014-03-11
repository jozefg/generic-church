{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Church (toChurch, fromChurch, Church) where

import Data.Church.Internal.ToChurch
import Data.Church.Internal.FromChurch
import Data.Church.Internal.TF
import Data.Proxy
import GHC.Generics

-- | This is the central type for this package. Unfortunately, it's
-- built around type families so it's not so easy to read. A helpful
-- algorithm for figuring out what the 'Church' of a type @Foo@ is,
-- 
--  1. For each constructor, write out it's type signature
--  2. Replace the @Foo@ at the end of each signature with @c@
--  3. Join these type signatures together with arrows @(a -> b -> c) -> c -> ...@
--  4. Append a final @ -> c@ to the end of this
type Church t c = ChurchSum (ToList (StripMeta (Rep t ())) (ListTerm ())) c


proxyOf :: a -> Proxy a
proxyOf = Proxy

class ChurchRep a where
  toChurch :: a -> Church a r
  fromChurch :: Church a (Rep a ()) -> a
  default toChurch :: (Generic a, GStripMeta (Rep a ()),
                         GList (StripMeta (Rep a ())) (ListTerm ()),
                         GChurchSum (ToList (StripMeta (Rep a ())) (ListTerm ())) r) =>
            a -> Church a r
  toChurch = result
    where result = elim p . flip toList (ListTerm :: ListTerm ()) . Just . stripMeta . from'
          proxy
          p :: Proxy (Church a r) -> Proxy r
          p = reproxy
          from' :: Generic a => a -> Rep a ()
          from' = from


-- | Reify a type to it's church equivalent.
-- The constraints look pretty scary but can mostly be ignored.
-- The only one the user should think about is @Generic a@. Everything
-- else Just Work for types created with @deriving Generic@.
-- In other words, read this type signature as @
_toChurch :: forall a r. (Generic a, GStripMeta (Rep a ()),
                         GList (StripMeta (Rep a ())) (ListTerm ()),
                         GChurchSum (ToList (StripMeta (Rep a ())) (ListTerm ())) r) =>
            a -> Church a r
_toChurch = elim p . flip toList (ListTerm :: ListTerm ()) . Just . stripMeta . from'
  where p = Proxy :: Proxy r
        from' :: Generic a => a -> Rep a ()
        from' = from

-- | Create a value from it's church representation. Again, the constraints should be mostly ignored.
-- This method may require an explicit signature.
_fromChurch :: forall a. (Generic a, (GBuild (MakePaths (Rep a ()) '[] '[]) (Church a (Rep a ())) (Rep a ()))) =>
              Church a (Rep a ()) -> a
_fromChurch c = to $ (build p c :: Rep a ())
  where p = Proxy :: Proxy (MakePaths (Rep a ()) '[] '[])
