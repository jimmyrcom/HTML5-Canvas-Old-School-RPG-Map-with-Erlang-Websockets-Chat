-module(connect).
-export([accept_connections/1]).
-include("user.hrl").
-define(TIMEOUT,30*1000).
-define(IDLE,60*10*1000).
%%Created by Jimmy Ruska under GPL 2.0
%% Copyright (C) 2010 Jimmy Ruska (@JimmyRcom,Youtube:JimmyRcom,Gmail:JimmyRuska)

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
            {ok,{IP,_}} = inet:peername(ClientS),
            ["move",User,X,Y] = string:tokens(binary_to_list(binary:part(Bin1,1,byte_size(Bin1)-2)),"||"),
            State=#simple{user=User,sock=ClientS,x=X,y=Y},
            case es_websock:checkUser(State,IP,self()) of
                fail ->
                    websockets:die(ClientS,"Already Connected");
                go ->
                    gen_tcp:send(ClientS,[0,"all @@@ ",es_websock:allUsers(User),255]),
                    client(State)
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
        die ->
            websockets:die(State#user.sock,"Server murdered websocket.");
        _ ->
            es_websock:logout(State#simple.user)
    after ?IDLE ->
            es_websock:logout(State#simple.user),
            websockets:die(State#user.sock,"Disconnected from server: IDLE Timeout")
    end.

actions(State,Data) ->
    User=State#simple.user,
    case Data of
        ["move",User1,X,Y] ->
            case User=/=User1 of
                false -> test(es_websock:move(User,X,Y),State);
                true -> websockets:die("Nice try brohan")
            end;
        ["say",User1,Message] ->
            case User=/=User1 of
                false ->
                    u:trace(User,Message),
                    test(es_websock:say(User,Message),State);
                true ->
                    websockets:die("Nice try brohan")
            end;
        _ ->
            u:trace("Unidentified Message",Data)
    end.

test(Response,_) when Response=:=fail -> self() ! die;
test(_,_) -> ok.

    

