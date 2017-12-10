metaparticle
============

[Metaparticle][1] is an abstraction layer for creating container runtimes such as
Docker and deploying those containers into different runtimes such as
Kubernetes, Azure, Amazon Web Services and more.

Metaparticle presents itself as extensions to a programming language so that
the full power of a language can be brought to bear on the needs of a developer
creating a runtime.

This is the Erlang implementation of metaparticle.

Example
-------
Consider the simple Erlang application of

```erlang
start() ->
    io:format("Hello ~s", ["World"]).
```

To containerize and deploy this application requires a developer learn and
configure several different tools. But using metaparticle, we can present an
abstraction that allows a developer to use these tools from the context of
Erlang.

```erlang
start() ->
    Runtime = #{ executor => docker },

    Package = #{ builder => docker,
                 name => example,
                 repository => <<YOURNAME>>,
                 publish => false,
                 otp_version => 20,
                 env => "dev"
               },

    metaparticle:containerize(Runtime, Package, fun do_start/0).

do_start() ->
    io:format("Hello ~s", ["World"]).

```

Using the `ports` key in the runtime map, we can tell the runtime environment
what ports on a container we should expose so that the they are available to
interact with the code running in the container itself.

Use
---
Add metaparticle as a dependency in your rebar.config.

The main entry point is the `containerize/3` function, and most of the behavior
of the library is handled by setting values in the two configuration maps: one
for the runtime environment, and one for the container builder environment.

You must also have [Docker][3] installed and running on your system.

Try typing `docker` at your command line to see if it's readily available.

TODO
----

* Consider using [erlexec][2] for better control with full Erlang idioms when
  dealing with external commands.

* Extending the runtime environments to support things other than docker, including
  kubernetes, azure, and AWS.


[1]: https://metaparticle.io/
[2]: https://saleyn.github.io/erlexec/
[3]: https://get.docker.io/
