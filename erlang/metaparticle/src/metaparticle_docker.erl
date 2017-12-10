-module(metaparticle_docker).

%% TODO: consider erlexec to attach to standard out/standard error better

%% builder
-export([
    write_dockerfile/1,
    build/1,
    publish/1
]).

%% runner
-export([
    run/2,
    cancel/2
]).

build(P) ->
    NewP = case maps:is_key(name, P) of
               true -> P;
	       false -> 
	           {ok, D} = file:get_cwd(),
                   N = filename:basename(D),
                   P#{ name => N }
           end,
    {ok, NewP1} = write_dockerfile(NewP),
    NewP2 = make_image_name(NewP1),
    ok = do_build(NewP2),
    NewP2.

make_image_name(#{ repository := Repo, name := Name } = P) ->
    Image = Repo ++ "/" ++ Name ++ ":latest",
    maps:put(image, Image, P).

write_dockerfile(P) ->
    NewP = validate_package_map(P),
    Raw = <<"FROM erlang:{{otp_version}}

COPY ./ /{{name}}/
WORKDIR /{{name}}
RUN rebar3 release
CMD [\"/{{name}}/_build/default/rel/{{name}}/bin/{{name}}\", \"console\"]">>,
    
    Template = bbmustache:parse_binary(Raw),
    Out = bbmustache:compile(Template, binmap(NewP)),
    ok = file:write_file("Dockerfile", Out),
    {ok, NewP}.

%% This function makes a temporary map just for templating
binmap(M) ->
    M0 = lists:foldl(fun(K, Acc) -> 
		    V = maps:get(K, M),
		    maps:put(atom_to_list(K), V, Acc)
		end,
                #{},
		[name, env, otp_version]),
    maps:map(fun(_K,V) -> to_binary(V) end, M0).

to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_binary(X) -> X.

validate_package_map(P) ->
    lists:foldl(fun present_or_default/2, P, 
		[otp_version, env]).

present_or_default(K, M) ->
    case maps:is_key(K, M) of
        true -> M;
	false -> maps:put(K, default(K), M)
    end.

default(otp_version) -> 20;
default(env) -> "dev".

do_build(#{ image := Image }) ->
    %% The period in the docker command below represents the current working
    %% directory.
    os:cmd("docker build -t " ++ Image ++ " .").

publish(#{ image := Image }) ->
    os:cmd("docker push " ++ Image).

cancel(_Runtime, #{ name := Name }) ->
    os:cmd("docker kill " ++ Name),
    os:cmd("docker rm " ++ Name).

run(Runtime, #{ name := N, image := I }) ->
    Ports = maybe_make_ports(Runtime),
    os:cmd("docker run -rm -e METAPARTICLE_IN_CONTAINER=true --name " ++ N ++ " " 
            ++ Ports 
	    ++ " -d " ++ I),
    os:cmd("docker logs -f " ++ I).

maybe_make_ports(R) ->
    case maps:is_key(ports, R) of
        false -> " ";
	true -> make_ports(maps:get(ports, R))
    end.

make_ports(Ports) ->
    P = lists:foldl(fun(Num, Acc) ->
		      NStr = integer_to_list(Num),
		      [ " -p " ++ NStr ++ ":" ++ NStr | Acc ]
		    end,
		    [], Ports),
    lists:flatten(lists:reverse(P)).
    
    
    
