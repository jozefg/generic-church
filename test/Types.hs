{-# LANGUAGE DeriveGeneric #-}
module Types where
import Data.Church
import GHC.Generics

data S3 a = S3_1 a | S3_2 a | S3_3 a
              deriving(Generic, Show, Eq)
data S4 a = S4_1 a | S4_2 a | S4_3 a | S4_4 a
                deriving(Generic, Show, Eq)
data AFew = C1 | C2 Int Char | C3 Bool Bool | C4
          deriving(Generic, Show, Eq)



instance ChurchRep AFew
instance ChurchRep (S3 a)
instance ChurchRep (S4 a)


