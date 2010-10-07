-module(u).
-compile(nowarn_unused_function).
-compile(export_all).

say(X) -> spawn(fun() -> io:format("~s~n",[X]) end).
trace(X) -> spawn(fun() -> io:format("~p~n",[X]) end).
trace(X,Y) -> spawn(fun() -> io:format("~s: ~p~n",[X,Y]) end).
traceBinary(X) -> spawn(fun() -> io:format("~p~n",[b2h(X)]) end).
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].
b2h(Bin) -> lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
h2b(String) -> << << (erlang:list_to_integer([Char], 16)):4/integer >> || Char <- String >>.
txt(Bin) -> [X || <<X>> <= Bin,X > 32, X < 127, X =/= 45].
b2s(Bin) ->
    b2s1(binary_to_list(Bin),[]).
b2s1([],Str) ->
    lists:reverse(Str);
b2s1([H|T],Str) ->
    case H > 32 andalso H < 127 andalso H =/= 45 of
	true -> b2s1(T,[H,$.|Str]);
	false -> b2s1(T,[46,46|Str])
end.

pmap(F, L,Parent) -> [receive {Pid, Res} -> Res end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

timer(Time,Fun) -> spawn(fun() -> receive after Time -> ?MODULE:Fun() end end).

signSubtract(A,B) ->
    case A<0 of
        true -> (erlang:abs(A)-erlang:abs(B))*-1;
        false -> (erlang:abs(A)-erlang:abs(B))
    end.

signSubtract1(A,B) ->
    case A<0 of
        true -> (erlang:abs(A)-B)*-1;
        _ -> (erlang:abs(A)-B)
    end.

floor(X) when X < 0 ->
    T = trunc(X),
    case (X - T) =:= 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).

addLen(Bin) ->
    Len=erlang:size(Bin)+2,
    <<Len:16,Bin/binary>>.

datetime_to_unixtime({{_Year, _Month, _Day},{_Hour, _Min, _Sec}}=Datetime) ->
    UnixZero = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime),
    Seconds - UnixZero.

unixtime() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    MegaSecs * 1000000 + Secs.
