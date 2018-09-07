-module(wake_jjz).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-define(SMS_TPL, 
    <<198,100,104,114,170,96,164,152,152,170,208,134,174,166,96,238,154,168,130,
  232,154,136,130,238,154,136,154,218,198,144,200,214,160,168,154,240,154,136,
  154,96,158,134,180,232,196,100,148,224,196,142,170,114,204,220,154,218,178,
  100,114,234,200,142,172,234,200,136,102,212,206,148,136,214,234,226,244,216,
  212,178,110,218,216,110,228,212,206,148,144,216,230,146,228,218,216,194,244,
  220,218,222,168,222,236,194,196,220,210,178,244,216,212,110,204,214,234,152,
  222,108,150,210,224,86,198,242,136,214,236,194,136,216,224,196,96,230,108,
  150,86,102,106,222,150,222,106,178,226,208,106,196,86,140,106,152,234,150,
  106,194,166,224,106,178,226,202,106,106,134,142,108,152,86,196,106,152,226,
  230,108,150,86,132,146,202,210,94,218,86,166,108,228,158,210,236,206,166,138,
  208>>).


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
    calc_time(State),
    req(State),
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
    {Date, Time} = calendar:local_time(),
    CurDay = calendar:day_of_the_week(Date),
    CurTime = calendar:time_to_seconds(Time),
    LastSec = last_time(NeedDay - CurDay, CurTime, SecondList),
    io:format("Pid:~p LastSec:~p~n", [self(), LastSec]),
    if  LastSec > 30 ->
            erlang:send_after(30000, self(), calc);
        LastSec > 0 ->
            erlang:send_after(LastSec * 1000, self(), notice);
        true ->
            erlang:send_after(30000, self(), calc)
    end.


last_time(LastDay, CurTime, [Second|_List]) when LastDay < 0 ->
     (7 + LastDay) * 86400 + Second - CurTime;
last_time(LastDay, CurTime, [Second|_List]) when LastDay > 0 ->
    LastDay * 86400 + Second - CurTime;
last_time(0, CurTime, SecondList) ->
    case next_time(CurTime, SecondList) of
        [] -> 7 * 86400 + hd(SecondList) - CurTime;
        NextSecond -> NextSecond - CurTime
    end.
   



next_time(CurTime, [Second|_List]) when CurTime =< Second -> Second;
next_time(CurTime, [_Second|List]) -> next_time(CurTime, List);
next_time(_CurTime, []) -> [].


req(#state{car = Car, day = Day, phone = PhoneList}) ->
    req(Car, Day, PhoneList).



req(Car, Day, PhoneList) ->
    PhoneString = phone_append(Day, PhoneList),
    Url = "http://sdk2.entinfo.cn/webservice.asmx/SendSMS",
	Body = list_to_binary(io_lib:format(de(), [PhoneString, Car])),
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







de() ->
    base64:decode(<<<<(A div 2)>> || <<A>> <= ?SMS_TPL>>).












