{- |
Module      :  Web.VKHS
Copyright   :  (c) Sergey Mironov <ierton@gmail.com> 2012
License     :  BSD-style (see the file LICENSE)

Maintainer  :  ierton@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

[@VKHS@]

VKHS is written in Haskell and provides access to Vkontakte <http://vk.com>
social network, popular mainly in Russia. Library can be used to login into the
network as a standalone application (OAuth implicit flow as they call it).
Interaction with user is not required. For now, vkhs offers limited error
detection and no captcha support.

Following example illustrates basic usage (please fill client_id, email and
password with correct values):

>   import Web.VKHS.Login
>   import Web.VKHS.API
>
>   main = do
>       let client_id = "111111"
>       let e = env client_id "user@example.com" "password" [Photos,Audio,Groups]
>       (Right at) <- login e
>
>       let user_of_interest = "222222"
>       (Right ans) <- api e at "users.get" [
>             ("uids",user_of_interest)
>           , ("fields","first_name,last_name,nickname,screen_name")
>           , ("name_case","nom")
>           ]
>       putStrLn ans

client_id is an application identifier, provided by vk.com. Users receive it
after registering their applications after SMS confirmation. Registration form is 
located here <http://vk.com/editapp?act=create>.

Internally, library uses small curl-based HTTP automata and tagsoup for jumping
over relocations and submitting various \'Yes I agree\' forms. Curl .so library is
required for vkhs to work. I am using curl-7.26.0 on my system.

[@Debugging@]

To authenticate the user, vkhs acts like a browser: it analyzes html but fills
all forms by itself instead of displaying pages. Of cause, would vk.com change
html design, things stop working.

To deal with that potential problem, I\'ve included some debugging facilities:
changing:

writing

>       (Right at) <- login e { verbose = Debug }

will trigger curl output plus html dumping to the current directory. Please,
mail those .html to me if problem appears.

[@Limitations@]

    * Ignores \'Invalid password\' answers

    * Captchas are treated as errors

    * Implicit-flow authentication, see documentation in
      Russian <http://vk.com/developers.php>
      for details

    * Probably, low speed due to restarting curl session on every request. But
      anyway, vk.com limits request rate to 3 per second.

-}

module Web.VKHS
    ( module Web.VKHS.Login
    , module Web.VKHS.Types
    , module Web.VKHS.API
    ) where

import Web.VKHS.Types
import Web.VKHS.Login
import Web.VKHS.API
