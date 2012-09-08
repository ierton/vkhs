module Web.VKHS.Types where

-- | AccessToken is a authentication data, required by all VK API
-- functions. It is a tuple of access_token, user_id, expires_in fields,
-- returned by login procedure.
type AccessToken = (String,String,String)

-- See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
-- (in Russian) for more details

-- | Access rigth to request from VK.
-- See API docs http://vk.com/developers.php?oid=-1&p=Права_доступа_приложений (in
-- Russian) for details
data AccessRight
    = Notify  -- Пользователь разрешил отправлять ему уведомления.
    | Friends -- Доступ к друзьям.
    | Photos  -- Доступ к фотографиям.
    | Audio   -- Доступ к аудиозаписям.
    | Video   -- Доступ к видеозаписям.
    | Docs    -- Доступ к документам.
    | Notes   -- Доступ заметкам пользователя.
    | Pages   -- Доступ к wiki-страницам.
    | Status  -- Доступ к статусу пользователя.
    | Offers  -- Доступ к предложениям (устаревшие методы).
    | Questions   -- Доступ к вопросам (устаревшие методы).
    | Wall    -- Доступ к обычным и расширенным методам работы со стеной.
              -- Внимание, данное право доступа недоступно для сайтов (игнорируется при попытке авторизации).
    | Groups  -- Доступ к группам пользователя.
    | Messages    -- (для Standalone-приложений) Доступ к расширенным методам работы с сообщениями.
    | Notifications   -- Доступ к оповещениям об ответах пользователю.
    | Stats   -- Доступ к статистике групп и приложений пользователя, администратором которых он является.
    | Ads     -- Доступ к расширенным методам работы с рекламным API.
    | Offline -- Доступ к API в любое время со стороннего сервера. 
    deriving(Show)

-- | Verbosity level. Setting Debug results in dumping *html files
data Verbosity = Normal | Trace | Debug
    deriving(Enum,Eq,Ord,Show)

type ClientId = String

-- | VKHS environment
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
    -- ^ Delay in milliseconds. Set after each request to prevent application
    -- from being blocked for flooding.
    , ac_rights :: [AccessRight]
    -- ^ Access rights to request
    }
    deriving (Show)

