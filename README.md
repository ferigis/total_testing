Total Testing
=====

__Authors:__ Felipe Ripoll. ([`ferigis@gmail.com`](mailto:ferigis@gmail.com)).

With this web application you can create your own tests (Driving school tests, Academic tests, Flight license tests...) and practice with them.
This application has been developed using Erlang/OTP (using Mnesia as a store) and Angular JS.

![tf2cal icon](https://raw.githubusercontent.com/ferigis/total_testing/master/screenshot.png)

Build
-----

Total testing uses [Rebar3](https://www.rebar3.org/).

    $ git clone https://github.com/ferigis/total_testing.git
    $ cd total_testing
    $ make

After compile we should compile the [jiffy](https://github.com/davisp/jiffy) dependency, it seems is not well integrated with rebar3.

    $ cd _build/default/lib/jiffy
    $ make
    $ cd ../../../..

Use
-----

After compiling the code, open a erlang console

    $ erl -pa _build/default/lib/*/ebin

Then start total testing :

```erlang
total_testing:start().
```

Open your browser and go to [http://localhost:8082](http://localhost:8082) and enjoy!


Tests
-----

In order to run the Erlang tests:

    $ make test