CREATE FUNCTION mk_array(int) RETURNS int[] IMMUTABLE AS
$$
    import PGutils (PGm, Array (..))
    import Data.Int (Int32)

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = fxs : chunksOf n sxs
       where (fxs, sxs) = splitAt n xs

    mkList :: Int32 -> [Maybe Int32]
    mkList upper = [if x `mod` 3 == 0 then Nothing else Just x | x <- [0 .. upper-1]]

    mk_array :: Maybe Int32 -> PGm (Maybe (Array (Maybe Int32)))
    mk_array m = case m of
        Nothing -> return Nothing
        Just 0 -> return (Just ArrayEmpty)
        Just 1 -> return (Just (Array1D  20                                                                                                (mkList    2)))
        Just 2 -> return (Just (Array2D (20, 21)                 (                                                            (chunksOf 2) (mkList    6))))
        Just 3 -> return (Just (Array3D (20, 21, 22)             (                                             (chunksOf 3) $ (chunksOf 2) (mkList   24))))
        Just 4 -> return (Just (Array4D (20, 21, 22, 23)         (                              (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList  120))))
        Just 5 -> return (Just (Array5D (20, 21, 22, 23, 24)     (               (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList  720))))
        Just 6 -> return (Just (Array6D (20, 21, 22, 23, 24, 25) ((chunksOf 6) $ (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList 5040))))
$$
LANGUAGE plhaskell;
