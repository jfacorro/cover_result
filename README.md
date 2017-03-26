cover_result
=====

Display cover result in the command-line

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [{cover_result, ".*", {git, "https://github.com/jfacorro/cover_result", {tag, "0.1.0"}}}]}.

Then just call your plugin directly in an existing application:


    $ rebar3 do ct, cover, cover_result
    ===> Fetching cover_result
    ===> Compiling cover_result
    ==== Code Coverage ====

    [% 100] module_name1
    [%  99] module_name2

    =======================
