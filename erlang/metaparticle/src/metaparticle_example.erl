-module(metaparticle_example).
-export([start/0]).

start() ->
    metaparticle:containerize(
        #{ executor => docker },
        #{ builder => docker,
           otp_version => <<"20">>,
           env => <<"dev">>,
           name => <<"example">>,
           repository => <<"example">>
         },
        fun do_start/0).

do_start() ->
    io:format("Hello ~s", ["World"]).
