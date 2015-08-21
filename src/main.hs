import qualified Arguments as Args
import qualified T411
import qualified Data.Map.Strict as Map
import qualified Types.Torrent as Torrent


main :: IO ()
main = Args.process mainDispatch


mainDispatch :: Args.FunctionsMap
mainDispatch = [ ("search", actionSearch),
                 ("categories", actionGetCategories),
                 ("terms", actionGetTerms),
                 ("download", actionDownload)
               ]


actionGetCategories :: Args.Function
actionGetCategories (username:password:_) = T411.withConnection username password $ \t411 -> do
    categories <- T411.getCategories t411
    case categories of
        Left error -> putStrLn error
        Right categories -> putStrLn . show $ categories
actionGetCategories _ = putStrLn "Provide a username and password"


actionGetTerms :: Args.Function
actionGetTerms (username:password:_) = T411.withConnection username password $ \t411 -> do
    categories <- T411.getCategories t411
    case categories of
        Left error -> putStrLn error
        Right terms -> putStrLn . show $ terms
actionGetTerms _ = putStrLn "Provide a username and password"


actionSearch :: Args.Function
actionSearch (username:password:query:offset:limit:_) = T411.withConnection username password $ \t411 -> do
    torrents <- T411.search query (read offset) (read limit) t411
    case torrents of
        Left error -> putStrLn error
        Right r -> 
            let rangeLength = Torrent.limit r
                offset = Torrent.offset r
                range = map ((+) offset) $ [0, rangeLength]
            in do
                putStrLn $ "total torrents: " ++ (show $ Torrent.total r)
                           ++ " | seeing range: " ++ (show range)
                putStrLn . unlines . map (show) $ (Torrent.torrents r)
actionSearch _ = putStrLn "Provide a username, password, search term, an offset and a torrent number"


actionDownload :: Args.Function
actionDownload (username:password:torrentId:location:_) = T411.withConnection username password $ \t411 -> do
    result <- T411.download (read torrentId) location t411
    case result of
        Left error -> putStrLn error
        Right message -> putStrLn message
actionDownload _ = putStrLn "Provide a username, password, torrent id and a folder to download to"

