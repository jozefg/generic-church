A package to reify types equipped with a generics instance back and forth between
church representations.

As an example.

    > let x = Just True
    > toChurch x 0 (const 1)
      1
    > fromChurch (\nothing just -> just True) :: Maybe Bool
      Just True
