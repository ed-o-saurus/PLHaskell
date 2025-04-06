CREATE FUNCTION query_array_insert() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, Array(Array6D), QueryResults(InsertResults), QueryParam(QueryParamArray, QueryParamInt4))
    import Data.Int (Int32)

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = fxs : chunksOf n sxs
       where (fxs, sxs) = splitAt n xs

    mkList :: Int32 -> [Maybe Int32]
    mkList upper = [if x `mod` 3 == 0 then Nothing else Just x | x <- [0 .. upper-1]]

    mk_array :: Maybe (Array QueryParam)
    mk_array = Just (Array6D (20, 21, 22, 23, 24, 25) ((chunksOf 6) $ (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (map QueryParamInt4 (mkList 5040))))

    query_array_insert :: PGm ()
    query_array_insert = do
        InsertResults _processed <- query "INSERT INTO query_arrays(a) VALUES ($1)" [QueryParamArray (Nothing, "int4") mk_array]
        return ()
$$
LANGUAGE plhaskell;
