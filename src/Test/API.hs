module Test.API where

import Web.VKHS.Login
import Web.VKHS.API
import Text.Printf

-- | Almost working example. Just set correct values for client_id/login/password
print_user_info = do
    let e = env "3213232" "user@example.com" "password" [Photos,Audio,Groups]
    (Right at) <- login e
    (Right ans) <- api e{verbose = Debug} at "users.get" [
          ("uids","911727")
        , ("fields","first_name,last_name,nickname,screen_name")
        , ("name_case","nom")
        ]
    putStrLn ans
