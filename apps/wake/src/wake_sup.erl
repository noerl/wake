%%%-------------------------------------------------------------------
%% @doc wake top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wake_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	WakeJJZSup = {wake_jjz_sup,{wake_jjz_sup, start_link, []}, permanent,5000,supervisor, [wake_jjz_sup]},
	WakeLoad = {wake_load,{wake_load, start_link, []}, permanent, 5000, worker, [wake_load]},
    {ok, { {one_for_all, 0, 1}, [WakeJJZSup, WakeLoad]} }.

%%====================================================================
%% Internal functions
%%====================================================================
