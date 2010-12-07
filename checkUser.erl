-module(checkUser).
-export([checkUser/2]).
-include("user.hrl").
%%Created by Jimmy Ruska under GPL 2.0
%% Copyright (C) 2010 Jimmy Ruska (@JimmyRcom,Youtube:JimmyRcom,Gmail:JimmyRuska)

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


checkUser(Record = #user{ip=IP}, State = #state{banned=Banned}) ->
    case lists:any(fun(IP1) when IP1=:=IP -> true;(_)->false end,Banned) of
        false -> checkUser1(IP,Record,State);
        true -> u:trace("fail at IP ban"), {noreply,State}
    end.

checkUser1(IP,Record=#user{ip=IP},State=#state{lookupByIP=LBIP}) ->
    case gb_trees:lookup(IP,LBIP) of
        none -> checkUser2(IP,Record,State);
        {value,_} ->  u:trace("fail at already logged in"), {reply,fail,State}
    end.

checkUser2(IP,Record,State) ->
    #user{user=User,x=X,y=Y,sprite=Sprite,sock=Sock,auth=Auth} = Record,
    #state{maps=Maps,increment=ID,lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP} = State,
    Map0Dict=array:get(0,Maps),

    %%send all user locations for current map
    Gather =
        fun(_,#user{x=X1,y=Y1,sprite=Sprite1,auth=Auth1,user=User1},Acc)->
                 [[",[\"",User1,"\",\"",Sprite1,"\",\"",Auth1,"\",\"",X1,"\",\"",Y1,"\"]"]|Acc]
        end,
    case lists:flatten(dict:fold(Gather,[],Map0Dict)) of
        [_|Out] -> void;
        [] -> Out="[]"
    end,
    websockets:msg(Sock,"all",["[",Out,"]"]),

    NewDict=dict:store(ID,Record#user{id=ID},Map0Dict),
    Maps1=array:set(0,NewDict,Maps),
    LBID1=dict:store(ID,0,LBID),
    LBIP1=gb_trees:enter(IP,{0,ID},LBIP),
    LBName1=gb_trees:enter(User,{0,ID},LBName),
    array:foldl(fun(_,Dict,_) -> es_websock:sendToAll(Dict,ID,["login @@@ ",User,"||",Sprite,"||",Auth,"||",X,"||",Y]) end,0,Maps),
    {reply,ID,State#state{maps=Maps1,increment=ID+1,lookupByID=LBID1,lookupByIP=LBIP1,lookupByName=LBName1}}.
