VKHS
====

Library written in Haskell provides access to Vkontakte (popular
Russian social network) API methods.

Internally, library uses small curl-based HTTP automata and tagsoup for jumping
over relocations and submitting various 'Yes I agree' forms.

Following exmaple illustrates basic usage:

    import Web.VKHS.Login
    import Web.VKHS.Api

    main = do
        let client_id = "111111"
        let user_of_interest = "222222"
        let e = env client_id "user@example.com" "password" [Photos,Audio,Groups]
        (Right at) <- login e
        (Right ans) <- api e at "users.get" [
              ("uids",user_of_interest)
            , ("fields","first_name,last_name,nickname,screen_name")
            , ("name_case","nom")
            ]
        putStrLn ans

client\_id here is an application identifier, provided by vk.com. Users recieve it
after registring their applications. Registration form is located [here](http://vk.com/apps.php?act=add).

Limitations
===========

* 'Invalid password' answers are ignored (on TODO list)
* Captchas are treated as errors
* Implicit-flow authentication, see [documentation in
  Russian](http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений)
  for details

License
=======

BSD3 license

Copyright (c) 2012, Sergey Mironov <ierton@gmail.com>

