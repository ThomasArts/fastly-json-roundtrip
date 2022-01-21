Json parser / encoder tester
=====

Build
-----
MNake sure you have Erlang installed, QuickCheck installed and a licence activated

    $ rebar3 compile

Test
----

In top dir of this repo, `fastly compute serve`
Then in different shell and this dirsctory
`rebar3 eqc -n 1000` to run 1000 tests

