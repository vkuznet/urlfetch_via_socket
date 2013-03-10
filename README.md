urlfetch
========

urlfetch is a fork of original urlfetch project [1].

Asynchronous URL Fetch Service
==============================

This project aims at providing a URL Fetch service for web application
developers to make concurrent asynchronous HTTP requests. The service is
entirely written in Erlang and so, it leverages the robust and scalable
Erlang/OTP platform.

Copyright and License
---------------------

Copyright 2010 Tobias Rodaebel

This software is released under the Apache License, Version 2.0. You may obtain
a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Requirements
------------

In order to run the URL Fetch service, Erlang/OTP R14A or higher is required.
The pyurlfetch library [1] is compatible with Python 2.5 up to 2.7

Credits
-------

Original idea, design and implementation belongs to Tobias Rodaebel, see
Copyright and License notice above.

The code has been extended by Valentin Kuznetsov to cover the following topics:
- replace urlfetch_cache implementation with gen_server behavior
- replace original packaging with rebar (ability to create release distribution)

Installation
------------

1. run make
2. tune-up rel/mynode/etc/vm.args (if required)
3. start-up server as (using bash shell):

- # export PATH=$PWD/rel/urlfetchd/bin:$PATH
- # urlfetchd console|start|stop

The console mode will start the application and gives you access to
standard Erlang console, while start|stop action will either
start or stop the application (aka daemon mode).

Server configuration:
---------------------

The bapp is a standard OTP application [1]. You can adjust server
parameters in rel/etc/vm.args. The release parameters can be tune-up
in rel/reltool.config.

References
----------

1. https://code.google.com/p/urlfetch/
2. https://github.com/basho/rebar
3. http://www.erlang.org/doc/design_principles/users_guide.html
