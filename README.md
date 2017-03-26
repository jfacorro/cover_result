cover_result
=====

Display cover result in the command-line

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { cover_result, ".*", {git, "git@host:user/cover_result.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 cover_result
    ===> Fetching cover_result
    ===> Compiling cover_result
    <Plugin Output>
