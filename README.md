VKHS
====

Library written in Haskell provides access to Vkontakte API methods (popular
russian social network).

See [documentation in Russian](http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений)
for details

Internally, library uses small curl-based HTTP automata and tagsoup for jumping
over relocations and submitting various 'Yes I agree' forms.

Limitations
===========

* 'Invalid password' answers are ignored (on TODO list)
* Captchas are treated as errors
* Standalone authentication only

License
=======

BSD3 license

Copyright (c) 2012, Sergey Mironov <ierton@gmail.com>

