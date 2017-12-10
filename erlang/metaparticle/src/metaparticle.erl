-module(metaparticle).

%% API exports
-export([
    containerize/3
]).

% runtime
% replicas >= 1 (how many replicas to deploy)
% shards >= 0
% shard pattern (a regexp to use as the shard key)
% executor (the execution environment; one of 'docker', etc)
%
% package
% repository binary 
% builder 'docker' atom
% verbose bool
% quiet bool
% publish bool

-define(CGROUP_FILE, "/proc/1/cgroup").
-define(DOCKER_TYPES, [<<"docker">>, <<"kubepods">>]).

%%====================================================================
%% API functions
%%====================================================================
containerize(Runtime, Package, Fun) ->
     case in_docker_container() of
         true -> Fun();
	 false -> 
            NewP = build_container(Package),
	    _ = maybe_publish(NewP),
	    _ = run_container(Runtime, NewP)
     end.

%%====================================================================
%% Internal functions
%%====================================================================

in_docker_container() ->
    case os:getenv("METAPARTICLE_IN_CONTAINER") of
    	false -> check_filesystem();
	_ -> true
    end.

check_filesystem() ->
    case filelib:is_regular_file(?CGROUP_FILE) of
       true -> read_cgroup_data();
       false -> false
    end.

read_cgroup_data() ->
    {ok, Data} = file:read_file(?CGROUP_FILE),
    Parts = binary:split(Data, <<"\n">>, [global, trim]),
    lists:any(fun(E) -> lists:member(E, ?DOCKER_TYPES) end, 
              Parts).

build_container(#{ builder := docker } = P) -> 
    metaparticle_docker:build(P);
build_container(#{ builder := Builder }) ->
    erlang:error({error, invalid_builder}, [Builder]);
build_container(Package) ->
    %% if there's no builder key, the default is docker.
    metaparticle_docker:build(Package).

maybe_publish(#{ publish := true } = P) ->
    metaparticle_docker:publish(P);
maybe_publish(_P) -> ok.

run_container(#{ executor := docker } = R, Package) ->
    metaparticle_docker:run(R, Package);
%% TODO: These executors are supports in other
%% languages, but I don't think I'll have time to
%% get to them.
%run_container(#{ executor := aci } = R, Package) ->
%    metaparticle_aci:run(Image, Name, R, Package);
%run_container(#{ executor := metaparticle } = R, Package) ->
%    metaparticle_mp:run(Image, Name, R, Package);
run_container(#{ executor := Other }, _Package) ->
    erlang:error({error, invalid_executor}, [Other]);
run_container(Runtime, _Package) ->
    erlang:error({error, invalid_runtime}, [Runtime]).
