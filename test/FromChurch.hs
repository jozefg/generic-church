module FromChurch where
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
  where eitherL = Left True  @?= (fromChurch (\l _ -> l True) :: Either Bool ())
        eitherR = Right True @?= (fromChurch (\_ r -> r True) :: Either () Bool)
        s3_1    = (S3_1 'a') @?= fromChurch (\ f _ _ -> f 'a')
        s3_2    = (S3_2 'a') @?= fromChurch (\ _ f _ -> f 'a')
        s3_3    = (S3_3 'a') @?= fromChurch (\ _ _ f -> f 'a')
        s4_1    = (S4_1 'a') @?= fromChurch (\ f _ _ _ -> f 'a') 
        s4_2    = (S4_2 'a') @?= fromChurch (\ _ f _ _ -> f 'a')
        s4_3    = (S4_3 'a') @?= fromChurch (\ _ _ f _ -> f 'a')
        s4_4    = (S4_4 'a') @?= fromChurch (\ _ _ _ f -> f 'a')

prods :: Test
prods = testGroup "Product Types" [testCase "(a, b)" prod2
                                  ,testCase "(a, b, c)" prod3
                                  ,testCase "(a, b, c, d)" prod4]
  where prod2 = (1, 2)       @?= (fromChurch (\f -> f 1 2)     :: (Int, Int))
        prod3 = (1, 2, 3)    @?= (fromChurch (\f -> f 1 2 3)   :: (Int, Int, Int))
        prod4 = (1, 2, 3, 4) @?= (fromChurch (\f -> f 1 2 3 4) :: (Int, Int, Int, Int))

afew :: Test
afew = testGroup "Mixed, AFew" [testCase "AFew: C1" c1
                               ,testCase "AFew: C2" c2
                               ,testCase "AFew: C3" c3
                               ,testCase "AFew: C4" c4]
  where c1 = C1  @?= fromChurch (\ c _ _ _ -> c)
        c2 = C2 1 'a'  @?= fromChurch (\ _ f _ _ -> f 1 'a')
        c3 = C3 True True @?= fromChurch (\ _ _ f _ -> f True True)
        c4 = C4  @?= fromChurch (\ _ _ _ c -> c)

fromChurchTests :: Test
fromChurchTests = testGroup "fromChurch" [sums, prods, afew]
