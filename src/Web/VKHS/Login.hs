{-# LANGUAGE FlexibleContexts #-}

module Web.VKHS.Login
    ( login
    , Env(..)
    , env
    , AccessRight(..)
    , Verbosity(..)
    ) where

import Prelude hiding ((.), id, catch)

import Control.Applicative
import Control.Category
import Control.Exception
import Control.Failure
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Monad.State as S

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.String
import Data.Char
import Data.Monoid
import Data.Either
import Data.Label
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable

import Network.Curlhs.Core
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Protocol.Uri.Query
import Network.Protocol.Cookie as C
import Network.Shpider.Forms

import Text.HTML.TagSoup
import Text.Printf
import System.IO

import Web.VKHS.Types
import Web.VKHS.Curl

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

toarg :: [AccessRight] -> String
toarg = intercalate "," . map (map toLower . show)

-- | Gathers login information into Env data set. 
env :: String 
    -- ^ Client ID (provided by Application registration form)
    -> String
    -- ^ User email, able to authenticate the user
    -> String
    -- ^ User password
    -> [AccessRight]
    -- ^ Rights to request
    -> Env
env cid email pwd ar = Env
    Normal
    "Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20120702 Firefox/12.0"
    [ ("email",email) , ("pass",pwd) ]
    cid
    500
    ar

vk_start_action :: ClientId -> [AccessRight] -> Action
vk_start_action cid ac = OpenUrl start_url mempty where
    start_url = (\f -> f $ toUri "http://oauth.vk.com/authorize") 
        $ set query $ bw params
            [ ("client_id",     cid)
            , ("scope",         toarg ac)
            , ("redirect_uri",  "http://oauth.vk.com/blank.html")
            , ("display",       "wap")
            , ("response_type", "token")
            ]

type FilledForm = Form

type VK a = ReaderT Env (ErrorT String IO) a

runVK :: Env -> VK a -> IO (Either String a)
runVK e vk = runErrorT (runReaderT vk e)

liftVK :: IO (Either String a) -> VK a
liftVK m = liftIO m >>= either fail return

dbgVK :: Verbosity -> IO () -> VK ()
dbgVK v act = ask >>= \e -> do
        if v > (verbose e) then return ()
                            else liftIO $ act

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

liftEIO act = (liftIO act) >>= either fail return

-- | Send a get-request to the server
vk_get :: Uri -> Cookies -> VK Page
vk_get u c = let
    c' = (BS.pack $ bw cookie $ map toShort $ bw gather c )
    u' = (BS.pack $ showUri u)
    in do
        e <- ask
        s <- liftEIO $ vk_curl e $ do
            when ((not . BS.null) c') $ do
                tell [CURLOPT_COOKIE c']
            when ((not . BS.null) u') $ do
                tell [CURLOPT_URL u']
        liftVK (return $ parseResponse s)

-- | Send a form to the server.
vk_post :: FilledForm -> Cookies -> VK Page
vk_post f c = let
    c' = (BS.pack $ bw cookie $ map toShort $ bw gather c )
    p' = (BS.pack $ bw params $ M.toList $ inputs f)
    u' = (BS.pack $ action f)
    in do
        e <- ask
        s <- liftEIO $ vk_curl e $ do
            when ((not . BS.null) c') $ do
                tell [CURLOPT_COOKIE c']
            when ((not . BS.null) u') $ do
                tell [CURLOPT_URL u']
            tell [CURLOPT_POST True]
            tell [CURLOPT_COPYPOSTFIELDS p']
        liftVK (return $ parseResponse s)
    
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

-- | Executes login procedure. AccessToken is returned on success
login :: Env -> IO (Either String AccessToken)
login e =  runVK e $ loop 0 (vk_start_action (clientId e) (ac_rights e)) where 
    loop n act = do
        when_trace $ printf "VK => %02d %s" n (show act)
        ans@(p,c) <- vk_move act
        when_debug $ vk_dump_page n (actionUri act) p
        a <- vk_analyze ans
        case a of
            Right act' -> do
                loop (n+1) act'
            Left at -> do
                return at

