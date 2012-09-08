VKHS
====

Library written in Haskell Provides access to Vkontakte API methods (russian
social network). Currently, library supports authentication using scheme for
standalone applications.

See http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений
for details

Internally, the library uses small HTTP fsm, curl and tagsoup for
jumping over relocations and submitting various 'Yes I agree' forms.

BSD3 license

Copyright (c) 2012, Sergey Mironov <ierton@gmail.com>
