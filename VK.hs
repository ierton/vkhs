{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding ((.), id, catch)

import Data.List
import Data.String
import Data.Monoid
import Data.Either
import Data.Label
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Category
import Control.Exception
import Control.Failure
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
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

type Url = String
type Email = String
type Uid = String
type Password = String
type Body = String

data Verbosity = Normal | Trace | Debug
    deriving(Enum,Eq,Ord,Show)

data Env = Env
    { verbose :: Verbosity
    -- ^ Verbosity level
    , useragent :: String
    -- ^ User agent identifier. Has an affordable default.
    , formdata :: [(String,String)]
    -- ^ A dictionary used for filling forms
    , clientId :: ClientId
    -- ^ Application ID provided by vk.com
    , delay_ms :: Int
    -- ^ Delay in milliseconds
    }
    deriving (Show)

-- | Creates VK initialisation data
mkEnv :: ClientId -> Email -> Password -> Env
mkEnv cid email pwd = Env
    Normal
    "Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20120702 Firefox/12.0"
    [ ("email",email) , ("pass",pwd) ]
    cid
    500

type ClientId = String

vk_start_action :: ClientId -> Action
vk_start_action cid = OpenUrl start_url mempty where
    start_url = (\f -> f $ toUri "http://oauth.vk.com/authorize") 
        $ set query $ bw params
            [ ("client_id",     cid)
            , ("scope",         "wall,group")
            , ("redirect_uri",  "http://oauth.vk.com/blank.html")
            , ("display",       "wap")
            , ("response_type", "token")
            ]

type FilledForm = Form

type AccessToken = (String,String,String)

type VK a = ReaderT Env (ErrorT String IO) a

runVK :: Env -> VK a -> IO (Either String a)
runVK e vk = runErrorT (runReaderT vk e)

liftVK :: IO (Either String a) -> VK a
liftVK m = liftIO m >>= either fail return

dbgVK :: Verbosity -> IO () -> VK ()
dbgVK v act = ask >>= \e -> do
        if v >= (verbose e) then liftIO $ act
                            else return ()

debugVK v s = dbgVK v (hPutStrLn stderr s)
printVK v s = dbgVK v (hPutStrLn stdout s)

when_debug = dbgVK Debug
when_trace = dbgVK Trace

type Page = (Http Response, Body)

-- | Browser action
data Action = OpenUrl Uri Cookies | SendForm Form Cookies
    deriving (Show)

-- | Url assotiated with Action
actionUri :: Action -> Uri
actionUri (OpenUrl u _) = u
actionUri (SendForm f _) = toUri . action $ f

-- | Generic request sender
vk_curl :: Writer [CURLoption] () -> VK Page
vk_curl w = do
    let askE x = ask >>= return . x
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    liftVK $ do
        buff <- newIORef BS.empty
        bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
            let 
                memwrite n = atomicModifyIORef buff
                    (\o -> (BS.append o n, CURL_WRITEFUNC_OK))
            in
            curl_easy_setopt curl $
                [ CURLOPT_HEADER         True
                , CURLOPT_WRITEFUNCTION (Just $ memwrite)
                , CURLOPT_SSL_VERIFYPEER False
                , CURLOPT_USERAGENT $ BS.pack a
                , CURLOPT_VERBOSE (v == Debug )
                ] ++ (execWriter w);
            curl_easy_perform curl;
            threadDelay (1000 * d); -- delay in microseconds
            b <- readIORef buff ;
            return (parseResponse $ BS.unpack b) ;
        } `catch`
            (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))

-- | Send a get-request to the server
vk_get :: Uri -> Cookies -> VK Page
vk_get u c = let
    c' = (BS.pack $ bw cookie $ map toShort $ bw gather c )
    u' = (BS.pack $ showUri u)
    in vk_curl $ do
        when ((not . BS.null) c') $ do
            tell [CURLOPT_COOKIE c']
        when ((not . BS.null) u') $ do
            tell [CURLOPT_URL u']

-- | Send a form to the server.
vk_post :: FilledForm -> Cookies -> VK Page
vk_post f c = let
    c' = (BS.pack $ bw cookie $ map toShort $ bw gather c )
    p' = (BS.pack $ bw params $ M.toList $ inputs f)
    u' = (BS.pack $ action f)
    in vk_curl $ do
        when ((not . BS.null) c') $ do
            tell [CURLOPT_COOKIE c']
        when ((not . BS.null) u') $ do
            tell [CURLOPT_URL u']
        tell [CURLOPT_POST True]
        tell [CURLOPT_COPYPOSTFIELDS p']
    
-- | Splits parameters into 3 categories:
-- 1)without a value, 2)filled from user dictionary, 3)with default values
split_inputs :: [(String,String)]
             -- ^ User dictionary
             -> M.Map String String
             -- ^ Form fields with default values (default is valid if non-zero)
             -> (M.Map String (), M.Map String String, M.Map String String)
split_inputs d m =
    let (b,g) = M.mapEitherWithKey (match_field d) m
    in (b, M.map (either id id) g, snd (M.mapEither id g))
    where
        match_field d k a
            | not (null a) = maybe ((Right . Left) a)  (Right . Right) u
            | otherwise    = maybe (Left ())           (Right . Right) u
            where u = lookup k d

vk_fill_form :: Form -> VK FilledForm
vk_fill_form f = ask >>= \e -> do
    let (bad,good,user) = split_inputs (formdata e) (inputs f)
    when (not $ M.null bad) (fail $ "Unmatched form parameters: " ++ (show bad))
    return f { inputs = good }

-- | Executes an action, returns Web-server's answer, adjusts cookies
-- Cookie management algorithm is very primitive: just merging
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

-- | Suggests new action
vk_analyze :: (Page,Cookies) -> VK (Either AccessToken Action)
vk_analyze ((h,b),c)
    | isJust a       = return $ Left (fromJust a)
    | isJust l       = return $ Right $ OpenUrl (fromJust l) c
    | (not . null) f = return $ Right $ SendForm (head f) c
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
        hPutStrLn stderr $ "dumped: name " ++ (name) ++ " size " ++ (show $ length b)

vk_login :: Env -> IO (Either String AccessToken)
vk_login e =  runVK e $ loop 0 (vk_start_action $ clientId e) where 
    loop n act = do
        when_trace $ printf "VK => %02d %s" n (show act)
        ans@(p,c) <- vk_move act
        when_debug $ vk_dump_page n (actionUri act) p
        a <- vk_analyze ans
        case a of
            Right act' -> do
                liftIO getChar
                loop (n+1) act'
            Left at -> do
                return at
    
        
-- vk_test_login = vk_travel (mkEnv "3115622")

