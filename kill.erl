-module(kill).
-export([kill/3]).
-include("user.hrl").
%%Created by Jimmy Ruska under GPL 2.0
%% Copyright (C) 2010 Jimmy Ruska (@JimmyRcom,Youtube:JimmyRcom,Gmail:JimmyRuska)

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

kill(State,ID,Message) ->
    #state{maps=Maps,banned=IPBlock,lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP} = State,
    {ok,Map}=dict:find(ID,LBID),
    MapDict=array:get(Map,Maps),
    {ok, #user{ip=IP,user=Username,pid=Pid}} = dict:find(ID,MapDict),
    Pid ! {kill,Message},
    array:foldl(fun(_,Dict) -> es_websock:sendToAll(Dict,ID,["logout @@@ ",Username]) end,0,Maps),
    Maps1=array:set(Map,dict:erase(ID,MapDict)),
    LBID1=dict:erase(ID,LBID),
    LBName1=removeID(ID,LBName),
    LBIP1=removeID(ID,LBIP),
    IPBlock1=lists:delete(IP,IPBlock),
    State#state{maps=Maps1,banned=IPBlock1,lookupByID=LBID1,lookupByName=LBName1,lookupByIP=LBIP1}.
    
removeID(ID,GB) ->
    Filter=fun({_,ID1}) when ID1=:=ID -> false;(_) -> true end,
    gb_trees:from_orddict(lists:filter(Filter,gb_trees:to_list(GB))).
    
