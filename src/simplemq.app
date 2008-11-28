{application, simplemq,
    [{description, "simplemq"},
        {vsn, "0.0.1"},
        {modules, [
                simplemq,
                simplemq_app,
                simplemq_server,
                simplemq_sup
            ]},
        {registered, []},
        {mod, {simplemq_app, []}},
        {env, []},
        {applications, [kernel, stdlib, crypto]}]}.

%% vim:ft=erlang:sw=4:sts=4:ts=8:et
