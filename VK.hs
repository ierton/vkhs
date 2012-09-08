{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ((.), id, catch)

import Data.List
import Data.String
import Data.Monoid
import Data.Either
import Data.Label
import qualified Data.Label.Maybe as LM
import qualified Data.Label.Abstract as LA
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import System.Exit

import Control.Applicative
import Control.Concurrent
import Control.Category
import Control.Exception
import Control.Failure
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State as S

import qualified Data.ByteString.Char8 as BS
import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import Network.Curlhs.Core

import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Protocol.Uri.Query
import Network.Protocol.Cookie as C

import Text.HTML.TagSoup
import Text.Printf
import System.IO

import Debug
import Forms
import Test

-- Test applications: 
--
-- pirocheck
-- ID 3115622
-- Key IOmaB10C1v7RoMiM6Lnu
--
-- pirofetch
-- ID 3082266
-- Key <lost>

data Env = Env [(String,String)]

mkEnv :: Env
mkEnv = Env
    [ ("email","ierton@gmail.com")
    , ("pass","toavo13")
    ]

vk_start_action :: Action
vk_start_action = OpenUrl start_url mempty where
    start_url = (\f -> f $ toUri "http://oauth.vk.com/authorize") 
        $ set query $ bw params
            -- [ ("client_id",     "3082266") -- pirofetch
            [ ("client_id",     "3115622")
            , ("scope",         "wall,group")
            , ("redirect_uri",  "http://oauth.vk.com/blank.html")
            , ("display",       "wap")
            , ("response_type", "token")
            ]

type Url = String
type Email = String
type Uid = String
type Password = String
type Body = String

type AccessToken = (String,String,String)

data ExitReason = Error String | Success AccessToken
    deriving(Show)

instance Error ExitReason where
    noMsg = Error ""
    strMsg a = Error a

type VK a = ReaderT Env (ErrorT ExitReason IO) a

failVK :: String -> VK a
failVK s =  lift $ ErrorT (return $ Left (Error s))

grapAccessToken :: AccessToken -> VK a
grapAccessToken a = lift $ ErrorT (return $ Left (Success a))

runVK :: Env -> VK a -> IO (Either ExitReason a)
runVK e vk = runErrorT (runReaderT vk e)

liftVK :: IO (Either String a) -> VK a
liftVK m = liftIO m >>= \e -> do
    case e of
        Left str -> failVK str
        Right x -> return x

type Page = (Http Response, Body)

data Action = OpenUrl Uri Cookies | SendForm Form Cookies
    deriving (Show)

actionUri :: Action -> Uri
actionUri (OpenUrl u _) = u
actionUri (SendForm f _) = toUri . action $ f

data Decision = Decision Action (Either String Page)
    deriving (Show)

data S = S [Decision] Cookies

type VKS a = S.StateT S (ErrorT String IO) a

runVKS :: VKS a -> S -> IO (Either String (a,S))
runVKS vk st = runErrorT (S.runStateT vk st)

curl_verbose = True
curl_useragent = BS.pack "Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20120702 Firefox/12.0"

vk_get :: Uri -> Cookies -> VK Page
vk_get u c = liftVK $ do
    buff <- newIORef BS.empty
    bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
        let 
            memwrite n = atomicModifyIORef buff
                (\o -> (BS.append o n, CURL_WRITEFUNC_OK))
            c' = (BS.pack $ bw cookie $ map toShort $ bw gather c )
            u' = (BS.pack $ showUri u)
        in
        curl_easy_setopt curl $
            (case BS.null c' of
                True -> []
                False -> [CURLOPT_COOKIE c'])
            ++
            (case BS.null u' of
                True -> []
                False -> [CURLOPT_URL u'])
            ++
            [ CURLOPT_VERBOSE       curl_verbose
            , CURLOPT_HEADER        True
            , CURLOPT_SSL_VERIFYPEER False
            , CURLOPT_WRITEFUNCTION (Just $ memwrite)
            , CURLOPT_USERAGENT     curl_useragent
            ];
        curl_easy_perform curl;
        b <- readIORef buff ;
        return (parseResponse $ BS.unpack b) ;
    } `catch`
        (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))

vk_post :: Form -> Cookies -> VK Page
vk_post f c = liftVK $ do
    buff <- newIORef BS.empty
    bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
        let 
            memwrite n = atomicModifyIORef buff
                (\o -> (BS.append o n, CURL_WRITEFUNC_OK))
            c' = (BS.pack $ bw cookie $ map toShort $ bw gather c )
            p' = (BS.pack $ bw params $ M.toList $ inputs f)
            u' = (BS.pack $ action f)
        in
        curl_easy_setopt curl $
            (case BS.null c' of
                True -> []
                False -> [CURLOPT_COOKIE c'])
            ++
            (case BS.null u' of
                True -> []
                False -> [CURLOPT_URL u'])
            ++
            [ CURLOPT_VERBOSE       curl_verbose
            , CURLOPT_HEADER        True
            , CURLOPT_SSL_VERIFYPEER False
            , CURLOPT_WRITEFUNCTION (Just $ memwrite)
            , CURLOPT_POST          True
            , CURLOPT_COPYPOSTFIELDS p'
            , CURLOPT_USERAGENT     curl_useragent
            ];
        curl_easy_perform curl;
        b <- readIORef buff ;
        return (parseResponse $ BS.unpack b) ;
    } `catch`
        (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))

-- Splits parameters into 3 categories:
-- 1)without a value, 2)filled from user dictionary, 3)with default values
split_inputs :: [(String,String)]
             -> M.Map String String
             -> (M.Map String (), M.Map String String, M.Map String String)
split_inputs d m =
    let (b,g) = M.mapEitherWithKey (match_field d) m
    in (b, M.map (either id id) g, snd (M.mapEither id g))
    where
        match_field d k a
            | not (null a) = maybe ((Right . Left) a)  (Right . Right) u
            | otherwise    = maybe (Left ())           (Right . Right) u
            where u = lookup k d

vk_fill_form :: Form -> VK Form
vk_fill_form f = ask >>= \(Env d) -> do
    let (bad,good,user) = split_inputs d (inputs f)
    when (not $ M.null bad) (fail $ "Unmatched form parameters: " ++ (show bad))
    return f { inputs = good }

vk_move :: Action -> VK (Page, Cookies)
vk_move (OpenUrl u c) = do
    (h,b) <- (vk_get u c)
    return ((h,b),c`mappend`(get setCookies h))
vk_move (SendForm f c) = do
    f' <- vk_fill_form f
    (h,b) <- (vk_post f' c)
    return ((h,b),c`mappend`(get setCookies h))

uri_fragment :: Http Response -> Maybe AccessToken
uri_fragment = get location >=> pure . get fragment >=> pure . fw (keyValues "&" "=") >=> \f -> do
    (\a b c -> (a,b,c)) <$> lookup "access_token" f <*> lookup "user_id" f <*> lookup "expires_in" f

vk_analyze :: (Page,Cookies) -> VK Action
vk_analyze ((h,b),c)
    | isJust a       = grapAccessToken (fromJust a)
    | isJust l       = return $ OpenUrl (fromJust l) c
    | (not . null) f = return $ SendForm (head f) c
    | otherwise      = fail "HTML processing failure (new design of VK login dialog?)"
    where
        l = get location h
        f = parseTags >>> gatherForms $ b
        a = uri_fragment h

vk_dump_page :: Int -> Uri -> Page -> IO ()
vk_dump_page n u (h,b) = 
    let name = printf "%02d-%s.html" n (showAuthority (get authority u))
    in bracket (openFile name WriteMode) (hClose) $ \f -> do
        hPutStrLn f b

vk_travel :: IO (Either ExitReason ())
vk_travel = runVK mkEnv $ loop 0 vk_start_action where 
    print = liftIO . putStrLn
    loop n act = do
        print "====================="
        print $ show act
        print "====================="
        ans@(p,c) <- vk_move act
        liftIO $ vk_dump_page n (actionUri act) p
        act' <- vk_analyze ans
        liftIO (threadDelay $ fromInteger 1000*1000)
        print "Hit a char to continue"
        liftIO $ getChar
        loop (n+1) act'

        

