VKHS
====

VKHS provides access to [Vkontakte][1] social network, popular mainly in Russia.
Library can be used to login into the network as a standalone application (OAuth
implicit flow as they call it). Interaction with user is not required. For now,
vkhs offers limited error detection and no captcha support.

VKQ
===

VKQ is a demo tool able of performing basic actions like login, music query and
download plus some others. Login session looks like following:

  $ vkq login user@mail.org pass123
  d785932b871f096bd73aac6a35d7a7c469dd788d796463a871e5beb5c61bc6c96788ec2 11229 32833

VKQ returns three values. First is a access token which is required to execute
future API requests. VKQ reads it from VKQ\_ACCESS\_TOKE environment variable so
we have to set it up

  $ export VKQ\_ACCESS\_TOKEN=d785932b871f096bd73aac6a35d7a7c469dd788d796463a871e5beb5c61bc6c96788ec2

Now, to list first 10 music files owned by current user, one have to execute

  $ vkq music -l | head -n 10

Lets download first record to the current directory:

  $ vkq music -o . 12331\_12312

VKQ prints file ID, its title and author told by VK and final filename. More
options are available, please consult vkq --help. Also, vkq supports generic API
calls. To use this option, just do

  $ vkq call METHOD\_NAME ARG1\=VAL1 ARG2\=VAL2 ...

Response will be returned asis, in JSON or XML format depending on a method.

VKHS library
============

Following example illustrates basic usage (please fill client\_id, email and
password with correct values):

    import Web.VKHS.Login
    import Web.VKHS.API

    main = do
        let client_id = "111111"
        let e = env client_id "user@example.com" "password" [Photos,Audio,Groups]
        (Right at) <- login e

        let user_of_interest = "222222"
        (Right ans) <- api e at "users.get" [
              ("uids",user_of_interest)
            , ("fields","first_name,last_name,nickname,screen_name")
            , ("name_case","nom")
            ]
        putStrLn ans

client\_id is an application identifier, provided by vk.com. Users receive it
after registering their applications after SMS confirmation. Registration form is 
located [here](http://vk.com/editapp?act=create).

Internally, library uses small curl-based HTTP automata and tagsoup for jumping
over relocations and submitting various 'Yes I agree' forms. Curl .so library is
required for vkhs to work. I am using curl-7.26.0 on my system.

Debugging
=========

To authenticate the user, vkhs acts like a browser: it analyzes html but fills
all forms by itself instead of displaying pages. Of cause, would vk.com change
html design, things stop working.

To deal with that potential problem, I've included some debugging facilities:
changing:

writing

        (Right at) <- login e { verbose = Debug }

will trigger curl output plus html dumping to the current directory. Please,
mail those .html to me if problem appears.

Limitations
===========

* 'Invalid password' answers are ignored (on TODO list)
* Captchas are treated as errors
* Implicit-flow authentication, see [documentation in
  Russian](http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений)
  for details
* Probably, low speed due to restarting curl session on every request. But
  anyway, vk.com limits request rate to 3 per second.

License
=======

BSD3 license

Copyright (c) 2012, Sergey Mironov <ierton@gmail.com>

[1]: http://vk.com

