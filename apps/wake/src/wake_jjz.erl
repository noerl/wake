-module(wake_jjz).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, req/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    calc_time(),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(calc, State) ->
    calc_time(),
    {noreply, State};
handle_info(notice, State) ->
    req(),
    calc_time(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{8,0,0}}) = 62167248000 
calc_time() ->
    CurDay = calendar:day_of_the_week(date()),
    NeedDay = calendar:day_of_the_week({2018,8,26}),
    LastSec = (NeedDay - CurDay) * 86400 - calendar:time_to_seconds(time()) + calendar:time_to_seconds({14, 12, 30}),
    io:format("lasttime:~p~n", [LastSec]),
    if  LastSec > 30 ->
            erlang:send_after(30000, self(), calc);
        LastSec > 0 ->
            erlang:send_after(LastSec * 1000, self(), notice);
        true ->
            NewLastSec = LastSec + 7 * 86400,
            erlang:send_after(NewLastSec * 1000, self(), notice)
    end.


req() ->
    req(18500519190).



req(Mobile) ->
    Url = "http://sdk2.entinfo.cn/webservice.asmx/SendSMS",
	Body = io_lib:format(<<"sn=SDK-HBY-010-00003&pwd=310348&mobile=~p&content=【京华旺】提醒您,请您务必今天办理进京证!进京证!!"/utf8>>, [Mobile]),
	case httpc:request(post, {Url, [], "application/x-www-form-urlencoded; charset=utf-8", Body}, [], []) of
        {ok,{{"HTTP/1.1",200,"OK"}, _, RespBody}} ->
            io:format("Resp:~ts~n", [list_to_binary(RespBody)]);
        Error ->
            io:format("Error:~p~n", [Error])
    end.