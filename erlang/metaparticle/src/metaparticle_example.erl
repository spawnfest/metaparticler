-module(metaparticle_example).
-behaviour(application).
-export([start/0, start/2, stop/1]).

%% required OTP application callbacks.
start(_, _) ->
    start(),
    case metaparticle:in_docker_container() of
        true ->
            {ok, self()};
        false ->
            {error, shutdown}
    end.

stop(_) -> ok.

%% metaparticle API
start() ->
    metaparticle:containerize(
        #{ executor => docker },
        #{ builder => docker,
           otp_version => 20,
           env => "dev",
           name => "example",
           repository => "example"
         },
        fun do_start/0).

do_start() ->
    io:format("Hello ~s", ["World"]).
