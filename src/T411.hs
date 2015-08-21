module T411 (
withConnection,
getCategories,
search,
download
) where

import Network.Curl
import qualified Types.Category as Category
import qualified Types.Torrent as Torrent
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as C


type Username = String
type Password = String
type Token = String
type RequestURL = String
data RequestMethod = Get | Post
type RequestFields = [String]

data Connection = Connection Curl Token


baseURL = "https://api.t411.me/"
authURL = baseURL ++ "auth"
categoriesURL = baseURL ++ "categories/tree"
termsURL = baseURL ++ "terms/tree"
searchURL = baseURL ++ "torrents/search"
downloadURL = baseURL ++ "torrents/download"


loginFields :: String -> String -> CurlOption
loginFields user password = CurlPostFields [ "username=" ++ user, "password=" ++ password ]


tokenHeader :: String -> CurlOption
tokenHeader token = CurlHttpHeaders [ "Authorization: " ++ token ]


makeRawRequest :: Curl -> RequestURL -> RequestMethod -> [CurlOption] -> IO (Either String String)
makeRawRequest curl url method options =
    let makeMethod Get = method_GET
        makeMethod Post = method_POST
    in do
        r <- do_curl_ curl url (options ++ makeMethod method) :: IO CurlResponse
        if respCurlCode r /= CurlOK
            then return . Left $ "Curl error n°" ++ (show $ respStatus r) ++ ": " ++ (show $ respCurlCode r)
            else return . Right . respBody $ r


getToken :: Curl -> String -> String -> IO (Either String Token)
getToken curl user password = do
    body <- makeRawRequest curl authURL Post [loginFields user password]
    case body of
        Left error -> return $ Left error
        Right body -> case ((Json.decode (C.pack body) :: Maybe (Map.Map String Token)) >>= Map.lookup "token") of
            Nothing -> return $ Left "No token in response"
            Just token -> return $ Right token


withConnection :: Username -> Password -> (Connection -> IO ()) -> IO ()
withConnection username password action = withCurlDo $ do
    curl <- initialize
    maybeToken <- getToken curl username password
    case maybeToken of
        Left error -> putStrLn error
        Right token -> action (Connection curl token)


makeRequest :: Connection -> RequestURL -> RequestMethod -> [CurlOption] -> IO (Either String String)
makeRequest (Connection curl token) url method options =
    makeRawRequest curl url method (tokenHeader token : options)


getCategories :: Connection -> IO (Either String [Category.Category])
getCategories t411 = do
    body <- makeRequest t411 categoriesURL Get []
    return $ body >>= Category.fromJson


search :: String -> Int -> Int -> Connection -> IO (Either String Torrent.QueryResult)
search query offset limit t411 = 
    let queryString = "offset=" ++ (show offset) ++ "&limit=" ++ (show limit)
    in do
        body <- makeRequest t411 (searchURL ++ "/" ++ query ++ "?" ++ queryString) Get []
        return $ body >>= Torrent.fromJson


download :: Int -> String -> Connection -> IO (Either String String)
download torrentId folder t411 =
    let torrentFile = folder ++ "/" ++ (show torrentId) ++ ".torrent"
    in do
        body <- makeRequest t411 (downloadURL ++ "/" ++ (show torrentId)) Get []
        case body of
            Left error -> return $ Left error
            Right torrent -> do
                writeFile torrentFile torrent
                return $ Right ("Saving torrent to file " ++ torrentFile)

