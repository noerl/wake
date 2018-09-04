-module(wake_jjz).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, req/3]).

-record(state, {
    car,
    day,
    phone,
    second,
    ref
}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Car, Time) ->
    gen_server:start_link(?MODULE, [Car, Time], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{CarNo, Day, PhoneList}, SecondList]) ->
    Ref = calc_time(Day, SecondList),
    {ok, #state{car = CarNo, day = Day, phone = PhoneList, second = SecondList, ref = Ref}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(calc, State) ->
    calc_time(State),
    {noreply, State};
handle_info(notice, State) ->
    req(State),
    calc_time(State),
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

calc_time(#state{day = Day, second = SecondList}) ->
    calc_time(Day, SecondList).


%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{8,0,0}}) = 62167248000
calc_time(_NeedDay, []) -> undefined;
calc_time(NeedDay, SecondList) ->
    CurDay = calendar:day_of_the_week(date()),
    LastSec = last_time(NeedDay - CurDay, SecondList),
    io:format("Pid:~p LastSec:~p~n", [self(), LastSec]),
    if  LastSec > 30 ->
            erlang:send_after(30000, self(), calc);
        LastSec > 0 ->
            erlang:send_after(LastSec * 1000, self(), notice);
        true ->
            erlang:send_after(30000, self(), calc)
    end.


last_time(LastDay, [Second|_List]) when LastDay > 0 ->
    CurTime = calendar:time_to_seconds(time()),
    LastDay * 86400 + Second - CurTime;
last_time(LastDay, SecondList) ->
    CurTime = calendar:time_to_seconds(time()),
    NextTime = 
        case next_time(CurTime, SecondList) of
            [] -> lists:nth(1, SecondList);
            NextSecond -> NextSecond
        end,
   LastDay * 86400 + NextTime - CurTime.



next_time(CurTime, [Second|_List]) when CurTime =< Second -> Second;
next_time(CurTime, [_Second|List]) -> next_time(CurTime, List);
next_time(_CurTime, []) -> [].


req(#state{car = Car, day = Day, phone = PhoneList}) ->
    req(Car, Day, PhoneList).



req(Car, Day, PhoneList) ->
    PhoneString = phone_append(Day, PhoneList),
    Url = "http://sdk2.entinfo.cn/webservice.asmx/SendSMS",
	Body = list_to_binary(io_lib:format(<<"sn=SDK-HBY-010-00003&pwd=310348&mobile=~s&content=【京华旺】尊敬的车牌号为:**~s 你好,请您务必今天办理进京证!进京证!!"/utf8>>, [PhoneString, Car])),
	io:format("Req:~p,~ts~n", [Url, Body]),
	case httpc:request(post, {Url, [], "application/x-www-form-urlencoded; charset=utf-8", Body}, [], []) of
        {ok,{{"HTTP/1.1",200,"OK"}, _, RespBody}} ->
            io:format("Pid:~p, Resp:~ts~n", [self(), list_to_binary(RespBody)]);
        Error ->
            io:format("Pid:~p, Error:~p~n", [self(), Error])
    end.


phone_append(Day, PhoneList) ->
    phone_append(Day, PhoneList, []).


phone_append(Day, [Phone|List], []) ->
    RealPhone = 
        case Phone rem 10 >= Day of
            true -> Phone - Day;
            false -> Phone + 10 - Day
        end,
    phone_append(Day, List, [RealPhone]);
phone_append(Day, [Phone|List], RealList) ->
    RealPhone = 
        case Phone rem 10 >= Day of
            true -> Phone - Day;
            false -> Phone + 10 - Day
        end,
    phone_append(Day, List, [RealPhone, ","|RealList]);
phone_append(_Day, [], RealList) -> lists:concat(RealList).


