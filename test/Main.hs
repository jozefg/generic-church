module Main where
import Data.Monoid
import Test.Framework
import ToChurch
import FromChurch



main :: IO ()
main = defaultMainWithOpts [toChurchTests, fromChurchTests] mempty
