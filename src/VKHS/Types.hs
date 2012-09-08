module VKHS.Types where

-- | AccessToken is a authentication data, required by all VK API
-- functions. It is a tuple of following fields: access_token,user_id,expires_in
--
-- See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
-- (in Russian) for details
type AccessToken = (String,String,String)

