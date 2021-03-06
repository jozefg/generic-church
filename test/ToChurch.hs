module ToChurch where
import Test.HUnit hiding(Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Church
import Types

sums :: Test
sums = testGroup "Sum Types" [testCase "Either: Left" eitherL
                             ,testCase "Either: Right" eitherR
                             ,testCase "S3: S3_1" s3_1
                             ,testCase "S3: S3_2" s3_2
                             ,testCase "S3: S3_3" s3_3
                             ,testCase "S4: S4_1" s4_1
                             ,testCase "S4: S4_2" s4_2
                             ,testCase "S4: S4_3" s4_3
                             ,testCase "S4: S4_4" s4_4]
  where eitherL = True @?= toChurch (Left True) id not
        eitherR = True @?= toChurch (Right True) not id
        s3_1    = 'a'  @?= toChurch (S3_1 'a') id (const 'b') (const 'b')
        s3_2    = 'a'  @?= toChurch (S3_2 'a') (const 'b') id (const 'b')
        s3_3    = 'a'  @?= toChurch (S3_3 'a') (const 'b') (const 'b') id
        s4_1    = 'a'  @?= toChurch (S4_1 'a') id (const 'b') (const 'b') (const 'b')
        s4_2    = 'a'  @?= toChurch (S4_2 'a') (const 'b') id (const 'b') (const 'b')
        s4_3    = 'a'  @?= toChurch (S4_3 'a') (const 'b') (const 'b') id (const 'b')
        s4_4    = 'a'  @?= toChurch (S4_4 'a') (const 'b') (const 'b') (const 'b') id 

prods :: Test
prods = testGroup "Product Types" [testCase "(a, b)" prod2
                                  ,testCase "(a, b, c)" prod3
                                  ,testCase "(a, b, c, d)" prod4]
  where prod2 = 3  @?= toChurch (1, 2)      (\a b     -> a + b         :: Int)
        prod3 = 6  @?= toChurch (1, 2, 3)   (\a b c   -> a + b + c     :: Int)
        prod4 = 10 @?= toChurch (1, 2, 3, 4)(\a b c d -> a + b + c + d :: Int)

afew :: Test
afew = testGroup "Mixed, AFew" [testCase "AFew: C1" c1
                               ,testCase "AFew: C2" c2
                               ,testCase "AFew: C3" c3
                               ,testCase "AFew: C4" c4]
  where c1 = True  @?= toChurch C1 True (\ _ _ -> False) (\ _ _ -> False) False
        c2 = True  @?= toChurch (C2 1 'a') False (\ _ _ -> True) (\ _ _ -> False) False
        c3 = True  @?= toChurch (C3 True True) False (\ _ _ -> False) (&&) False
        c4 = True  @?= toChurch C4 False (\ _ _ -> True) (\_ _ -> False) True

toChurchTests = testGroup "toChurch" [sums, prods, afew]
