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
    map:put(image, Image, P).

write_dockerfile(P) ->
    NewP = validate_package_map(P),
    Template = <<"FROM erlang:{{otp_version}}

COPY ./ /{{name}}/
WORKDIR /{{name}}
RUN rebar3 as {{env}} release
CMD [\"/{{name}}/_build/{{env}}/rel/{{name}}/bin/{{name}}\"]">>,
    
    Out = bbmustache:render(Template, NewP),
    ok = file:write_file("Dockerfile", Out),
    {ok, NewP}.

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
    os:cmd("docker run -rm --name " ++ N ++ " " 
            ++ Ports 
	    ++ " -d " ++ I).

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
    
    
    
