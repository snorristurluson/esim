%%%-------------------------------------------------------------------
%% @doc esim public API
%% @end
%%%-------------------------------------------------------------------

-module(esim_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    world:start_link(),
    {ok, Ball1} = gen_statem:start(ball, [{-13, 42}, {1, 0}], []),
    {ok, Ball2} = gen_statem:start(ball, [{17, -34}, {0.5, 2}], []),
    gen_server:call(world, {add_ball, Ball1}),
    gen_server:call(world, {add_ball, Ball2}),
    esim_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
