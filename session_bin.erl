-module(session_bin).
-export([session/1]).

%% Copyright (C) 2010 Jimmy Ruska (www.JimmyR.com,Youtube:JimmyRcom,Gmail:JimmyRuska), under GPL 2.0
%This code can parse the PHP Session. For now, I'm just using it to see if the session exists
%in order to authenticate a particular name. Authenticated names will show up in a different
%color, while guests can still set nicknames.

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


session(Session1) ->
    Session = re:replace(Session1,"[^a-z0-9]+","",[global,{return,list}]),

    case byte_size(Session) of
        26 -> void;
        _ -> throw("invalid session"),u:trace("Invalid Session")
    end,

    case file:read_file(["/var/lib/php5/sess_",Session]) of
        {ok,Bin} -> parse(Bin);
        {error,_} -> fail
    end.

parse(<<>>) -> fail;
parse(S) -> parseKey(S,[],[]).

parseKey(<<>>,_,List) -> lists:reverse(List);
parseKey(<<$\|,S/binary>>,Key,List) -> 
    parseType(S,lists:reverse(Key),List);
parseKey(<<C,S/binary>>,Key,List) ->
    parseKey(S,[C|Key],List).

parseType(<<>>,_,_) -> fail;
parseType(<<C,S/binary>>,Key,List) ->
    case C of
        $i ->
            <<_,S1/binary>> = S,
            parseInt(S1,Key,[],List);
        $s ->
            <<$:,S1/binary>> = S,
            {Amount,S2}=parseStrLen(S1,[]),
            parseString(Amount,S2,Key,[],List)
    end.

%parseInt([],Key,Value,List) -> [{Key,list_to_integer(Value)}|List];
parseInt(<<$;,S/binary>>,Key,Value,List) -> parseKey(S,[],[{Key,list_to_integer(Value)}|List]);
parseInt(<<C,S/binary>>,Key,Value,List) -> parseInt(S,Key,[C|Value],List).

parseStrLen(<<$:,$",T/binary>>,List) -> {list_to_integer(lists:reverse(List)),T};
parseStrLen(<<C,T/binary>>,List) -> parseStrLen(T,[C|List]).

parseString(0,<<$",$;,S/binary>>,Key,Value,List) -> parseKey(S,[],[{Key,lists:reverse(Value)}|List]);
parseString(Amount,<<C,S/binary>>,Key,Value,List) -> parseString(Amount-1,S,Key,[C|Value],List).
