-module(say).
-export([say/3]).
-define(FLOOD, 2000).
-include("user.hrl").
%%Created by Jimmy Ruska under GPL 2.0
%% Copyright (C) 2010 Jimmy Ruska (@JimmyRcom,Youtube:JimmyRcom,Gmail:JimmyRuska)

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


say(#simple{id=ID,map=Map},Message,State = #state{maps=Maps}) ->
    MapDict=array:get(Map,Maps),
    say1({ID,Map,MapDict,Message,Maps},dict:find(ID,MapDict),State).

say1({ID,Map,MapDict,Message,Maps},{ok, Record},State) ->
    #user{lastMessage=LastMessage,floodTest=[_|FloodTest],user=User,sock=Sock} = Record,
    Unix=u:munixtime(),
    Waited=Unix-LastMessage,
    Difference=Waited-?FLOOD,
    case Waited > ?FLOOD of 
        true ->
            es_websock:sendToAll(MapDict,ID,["say @@@ ",User,"||",Message]);
        false ->
            websockets:alert(Sock,["Error: Flooding, message not sent, wait ",?FLOOD - Difference," more seconds."])
    end,
    FloodTest1=FloodTest ++ [Difference],
    {Len,Sum} = lists:foldl(fun(X,{Len,Sum}) -> {Len+1,Sum+X} end,{0,0},FloodTest1),
    Test = Sum div Len,
%    u:trace("Flood Test",Test),
    case Test<1000 of
        false ->
            NewDict=dict:store(ID,Record#user{floodTest=FloodTest1,lastMessage=Unix,lastAction=Unix},MapDict),
            NewMap=array:set(Map,NewDict,Maps),
            {noreply,State#state{maps=NewMap}};
        true ->
            #state{banned=Banned} = State,
            #user{ip=IP} = Record,
            NewState=State#state{banned=[IP|Banned]},
            kill:kill(NewState,ID,"Excess Flooding, you have been banned")
    end;
say1(_,_,State) -> State.
    
