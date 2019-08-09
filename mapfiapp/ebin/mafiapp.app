%%%-------------------------------------------------------------------
%%% @author cdw
%%% Created 2019/03/21
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
{application, mafiapp, [
    {description, "Help the boss keep track of his friends"},
    {vsn, "1.0.0"},
    {registered, []},
    {modules, [mafiapp, mafiapp_sup]},
    {applications, [
        kernel,
        stdlib,
        mnesia
    ]},
    {mod, {mafiapp, []}},
    {env, []}
]}.