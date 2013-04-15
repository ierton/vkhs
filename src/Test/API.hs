module Test.API where

import Web.VKHS

-- | Almost working example. Just set correct values for client_id/login/password
print_user_info :: IO ()
print_user_info = do
    let e = env "3213232" "user@example.com" "password" [Photos,Audio,Groups]
    r <- login e{verbose = Debug}
    print r
    let (Right (at,_,_)) = r
    ans <- api (callEnv e{verbose = Debug} at) "users.get" [
          ("uids","911727")
        , ("fields","first_name,last_name,nickname,screen_name")
        , ("name_case","nom")
        ]
    print ans
