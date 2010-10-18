-module(move).
-compile(export_all).
-include("user.hrl").
%% Copyright (C) 2010 Jimmy Ruska (www.JimmyR.com,Youtube:JimmyRcom,Gmail:JimmyRuska), under GPL 2.0

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


move(#simple{id=ID,map=Map},X,Y,State = #state{maps=Maps}) ->
    MapDict=array:get(Map,Maps),
    Now=u:munixtime(),
    case dict:find(ID,MapDict) of
        {ok, Record=#user{lastAction=LastAction}} when (Now-LastAction)>349 ->
            NewMaps=array:set(Map,dict:store(MapDict,Record#user{lastAction=Now,x=X,y=Y}),Maps),
            es_websock:sendToAll(MapDict,ID,["move @@@ ",X,"||",Y]),
            {reply,ok,State#state{maps=NewMaps}};
        _ -> {reply,State}
    end.

    
