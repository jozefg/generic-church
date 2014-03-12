{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DataKinds #-}
{-# LANGUAGE DefaultSignatures                                #-}
module Data.Church (Church, ChurchRep(..)) where

import Data.Church.Internal.ToChurch
import Data.Church.Internal.FromChurch
import Data.Church.Internal.TF
import Data.Proxy
import GHC.Generics

-- | This is the central type for this package. Unfortunately, it's
-- built around type families so it's not so easy to read. A helpful
-- algorithm for figuring out what the 'Church' of a type @Foo@ is,
-- 
--  1. For each constructor, write out its type signature
--  2. Replace the @Foo@ at the end of each signature with @c@
--  3. Join these type signatures together with arrows @(a -> b -> c) -> c -> ...@
--  4. Append a final @ -> c@ to the end of this
type Church t c = ChurchSum (ToList (StripMeta (Rep t ())) (ListTerm ())) c

class ChurchRep a where
  -- | Reify a type to its church representation
  toChurch :: forall r. ChurchRep a => a -> Church a r
  toChurch = toChurchHelper (Proxy :: Proxy r)

  -- | Reify a type to it's church equivalent. This is a hack
  -- to avoid a bug in GHC 7.6 where -XDefaultSignatures and
  -- -XScopedTypeVariables together will crash. A non-class
  -- member function 
  toChurchHelper :: Proxy r -> a -> Church a r
  default
    toChurchHelper :: (Generic a, GStripMeta (Rep a ()),
                       GList (StripMeta (Rep a ())) (ListTerm ()),
                       GChurchSum (ToList (StripMeta (Rep a ())) (ListTerm ())) r) =>
                  Proxy r -> a -> Church a r
  toChurchHelper p = elim p
                     . flip toList (ListTerm :: ListTerm ())
                     . Just
                     . stripMeta . from'
    where from' :: Generic a => a -> Rep a ()
          from' = from

  -- | Create a value from its church representation.
  -- This method may require an explicit signature.
  fromChurch :: Church a (Rep a ()) -> a
  default
    fromChurch :: (Generic a,
                   (GBuild (MakePaths (Rep a ()) '[] '[])
                    (Church a (Rep a ()))
                    (Rep a ()))) =>
              Church a (Rep a ()) -> a
  fromChurch c = to (build p c :: Rep a ())
    where p = Proxy :: Proxy (MakePaths (Rep a ()) '[] '[])

-- And now a ton of instances
instance ChurchRep Bool	 
instance ChurchRep Ordering
instance ChurchRep [a]
instance ChurchRep ()
instance ChurchRep ((,) a b)	 
instance ChurchRep ((,,) a b c)	 
instance ChurchRep ((,,,) a b c d)	 
instance ChurchRep ((,,,,) a b c d e)	 
instance ChurchRep ((,,,,,) a b c d e f)	 
instance ChurchRep ((,,,,,,) a b c d e f g)	 
instance ChurchRep (Maybe a)
instance ChurchRep (Either a b)
