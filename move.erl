-module(move).
-compile(export_all).
-include("user.hrl").

move(#simple{id=ID,map=Map},X,Y,State = #state{maps=Maps}) ->
    MapDict=array:get(Map,Maps),
    Now=u:munixtime(),
    case dict:find(ID,MapDict) of
        {ok, Record#user{lastAction=LastAction}} when (Now-LastAction)>349 ->
            NewMaps=array:set(Map,dict:store(MapDict,Record#user{lastAction=Now,x=X,y=Y}),Maps),
            {reply,ok,State#state{maps=NewMaps}}.
        _ -> {reply,State}
    end;

    
