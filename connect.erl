-module(connect).
-export([accept_connections/1]).
-include("user.hrl").
-define(TIMEOUT,30*1000).
-define(IDLE,60*10*1000).
%% Copyright (C) 2010 Jimmy Ruska (www.JimmyR.com,Youtube:JimmyRcom,Gmail:JimmyRuska), under GPL 2.0

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

accept_connections(S) ->
    {ok, ClientS} = gen_tcp:accept(S),
    spawn(fun() -> accept_connections(S) end),
    receive {tcp,_,Bin} ->
            gen_tcp:send(ClientS, websockets:handshake(Bin)),
            step2(ClientS)
    after ?TIMEOUT ->
            websockets:die(ClientS,"Timeout on Handshake")
    end.

step2(ClientS) ->
    receive {tcp,_,Bin1} ->
            ["register",User,Sprite,X,Y] = string:tokens(binary_to_list(binary:part(Bin1,1,byte_size(Bin1)-2)),"||"),
            if (length(User)>25) -> websockets:die("Name too long"); true -> void end,
            Test = lists:any(fun(E) when E=:=$ ;E=:=$<;E=:=$> -> true;(_)->false end,User),
            case Test of true -> websockets:die("Bad characters in username.'"); false -> void end,        
            {ok,{IP,_}} = inet:peername(ClientS),
            State = #user{user=User,sprite=Sprite,sock=ClientS,x=X,y=Y,ip=IP,pid=self()},
            case es_websock:checkUser(State) of
                fail -> websockets:die(ClientS,"Already Connected");
                ID -> client(#simple{id=ID,sock=ClientS})
            end
    after ?TIMEOUT ->
            websockets:die(ClientS,"Timeout on Handshake")
    end.
   
client(State) ->
    receive
        {tcp,_,Bin} -> 
            Bin1 = binary_to_list(binary:part(Bin,1,byte_size(Bin)-2)),
            actions(State,string:tokens(Bin1,"||")),
            client(State);
        {tcp_closed,_} ->
            logoutAndDie(State,"Disconnected");
        {die,Reason} ->
            logoutAndDie(State,Reason);
        What ->
            u:trace(What),
            logoutAndDie(State,"Crash")
    after ?IDLE ->
            logoutAndDie(State,"Idle")
    end.

logoutAndDie(State,MSG) ->
    es_websock:logout(State),
    websockets:die(State#simple.sock,MSG).
    
actions(State,Data) ->
    case Data of
        ["move",X,Y]  -> es_websock:move(State,X,Y);
        ["say",Message] -> es_websock:say(State,Message);
        ["nick",Name] -> es_websock:nick(State,Name);
        ["sprite",Sprite] when Sprite=:="0";Sprite=:="1" -> es_websock:sprite(State,Sprite);
        ["challenge",User]  -> es_websock:challenge(State,User);
        _ -> u:trace("Unidentified Message",Data)
    end.



