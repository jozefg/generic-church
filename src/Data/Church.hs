{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Data.Church (toChurch, fromChurch, Church) where

import Data.Church.Internal.ToChurch
import Data.Church.Internal.FromChurch
import Data.Church.Internal.TF
import Data.Proxy
import GHC.Generics

type Church t c = ChurchSum (ToList (StripMeta (Rep t ())) (ListTerm ())) c
toChurch :: forall a r. 
            (Generic a, GStripMeta (Rep a ()),
             GList (StripMeta (Rep a ())) (ListTerm ()),
             GChurchSum (ToList (StripMeta (Rep a ())) (ListTerm ())) r) =>
            a -> Church a r
toChurch = elim p . flip toList (ListTerm :: ListTerm ()) . Just . stripMeta . from'
  where p = Proxy :: Proxy r

fromChurch :: forall a. (Generic a, (GBuild (MakePaths (Rep a ()) '[] '[]) (Church a (Rep a ())) (Rep a ())))
              => Church a (Rep a ()) -> a
fromChurch c = to $ (build p c :: Rep a ())
  where p :: Proxy (MakePaths (Rep a ()) '[] '[])
        p = Proxy
