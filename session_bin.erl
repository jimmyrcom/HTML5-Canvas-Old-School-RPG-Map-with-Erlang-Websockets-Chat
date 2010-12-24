-module(session_bin).
-export([session/1]).

%% Copyright (C) 2010 Jimmy Ruska (www.JimmyR.com,Youtube:JimmyRcom,Gmail:JimmyRuska), under GPL 2.0
%This code can parse the PHP Session. For now, I'm just using it to see if the session exists
%in order to authenticate a particular name. Authenticated names will show up in a different
%color, while guests can still set nicknames.

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

%binary input
session(Session1) ->
    Session = re:replace(Session1,<<"[^a-z0-9]+">>,<<"">>,[global,{return,binary}]),

    case byte_size(Session) of
        26 -> void;
        _ -> throw("invalid session"),u:trace("Invalid Session")
    end,

    case file:read_file(["/var/lib/php5/sess_",binary_to_list(Session1)]) of
        {ok,Bin} -> parse(Bin);
        {error,_} -> throw("Could Not Load File")
    end.
parse(<<>>) -> fail;
parse(S) -> parseKey(S,<<>>,[]).

parseKey(<<>>,_,List) -> lists:reverse(List);
parseKey(<<$\|,S/binary>>,Key,List) -> 
    parseType(S,Key,List);
parseKey(<<C,S/binary>>,Key,List) ->
    parseKey(S,<<Key/binary,C>>,List).

parseType(<<>>,_,_) -> fail;
parseType(<<C,S/binary>>,Key,List) ->
    case C of
        $i ->
            <<_,S1/binary>> = S,
            parseInt(S1,Key,<<>>,List);
        $s ->
            <<$:,S1/binary>> = S,
            parseStrLen(S1,<<>>,Key,List)
    end.

%parseInt([],Key,Value,List) -> [{Key,list_to_integer(Value)}|List];
parseInt(<<$;,S/binary>>,Key,Value,List) -> parseKey(S,<<>>,[{Key,binary_to_integer(Value,0)}|List]);
parseInt(<<C,S/binary>>,Key,Value,List) -> parseInt(S,Key,<<Value/binary,C>>,List).

parseStrLen(<<$:,$",T/binary>>,Len,Key,List) -> parseString(binary_to_integer(Len,0),T,Key,<<>>,List);
parseStrLen(<<C,T/binary>>,Len,Key,List) -> parseStrLen(T,<<Len/binary,C>>,Key,List).

parseString(0,<<$",$;,S/binary>>,Key,Value,List) -> parseKey(S,<<>>,[{Key,Value}|List]);
parseString(Amount,<<C,S/binary>>,Key,Value,List) -> parseString(Amount-1,S,Key,<<Value/binary,C>>,List).

binary_to_integer(<<>>,Acc) -> Acc;
binary_to_integer(<<Num:8,Rest/binary>>,Acc) when Num >= 48 andalso Num < 58 ->
   binary_to_integer(Rest, Acc*10 + (Num-48));
binary_to_integer(_,Acc) -> exit({badarg,Acc}).
