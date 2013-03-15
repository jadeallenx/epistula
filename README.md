epistula
========
An Erlang library for constructing and sending MIME emails

Overview
--------

Epistula is a [latin noun][1] that means "letter."

The goal of this application is to permit easy construction and sending of
MIME emails.  This application cribs several functions and ideas from
other Erlang applications, including its dependency [esmtp][2] and the
[zotonic][3] CMS.

The emphasis here is on constructing and sending email, not receiving 
or parsing it.

Build
-----
    rebar get-deps

    rebar compile

Example
-------
    erl -c erl.config -pa ebin -pa deps/*/ebin
    1> epistula:start().
    2> Msg = epistula:new("to@example.com", "from@example.com", "Example subject", "Example body text").
    3> esmtp:send(epistula:to(Msg), epistula:from(Msg), epistula:encode(Msg)).

Email transport
---------------
This application assumes that you will be using an SMTP "smarthost" to do your email routing.
Depending on your network, it may be a local smtp server listening on port 25.

The following esmtp application configuration variables are available:
* `smarthost` {Host::string(), Port::pos\_integer()}
* `login` {Username::string, Password::string()}
* `default_from` From :: string()

If the smarthost port is set to 465 or 587, the esmtp application will attempt to
use the `STARTTLS` directive to begin an SSL session with the smarthost.

Example esmtp config
--------------------
```erlang
[
 {esmtp, [
    {smarthost, {"localhost", 25}},
    {default_from, "youremail@example.com"}
 ]}
].
```

See also the `erl.config` file in the project root.

[1]: http://en.wiktionary.org/wiki/epistula
[2]: https://github.com/archaelus/esmtp
[3]: https://github.com/zotonic/zotonic
