-module(es_websock).
-behaviour(gen_server).
-compile(export_all).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-include("user.hrl").
-define(SERVER, ?MODULE).

%%Created by Jimmy Ruska under GPL 2.0
%% Copyright (C) 2010 Jimmy Ruska (@JimmyRcom,Youtube:JimmyRcom,Gmail:JimmyRuska)

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
    process_flag(trap_exit, true),
    %443
    case gen_tcp:listen(844, [binary, {packet, 0}, {active, true}, {reuseaddr, true}, {packet_size,1024*2}]) of
        {ok, S} -> 
            spawn(fun() -> connect:accept_connections(S) end),
            {ok,#state{sock=S}};
        Err -> 
            u:trace("Accept connections failed"),
            throw(Err)
    end.

debug() -> gen_server:call(?MODULE,debug).
stop() -> gen_server:call(?MODULE,die).
gs() -> gen_server:call(?MODULE,getState).
rs() -> gen_server:call(?MODULE,resetState).

sendToAll(Dict,You,Message) ->
    dict:map(fun(ID,_) when ID=:=You -> void;
                (_,Record) -> gen_tcp:send(Record#user.sock,[0,Message,255])
             end,Dict).

say(ID,Message) -> gen_server:cast(?MODULE,{say,ID,Message}).
move(ID) -> gen_server:cast(?MODULE,{move,ID}).
logout(ID) -> gen_server:cast(?MODULE,{logout,ID}).

checkUser(State) -> gen_server:call(?MODULE,{checkUser,State}).

%performance penalty from using modules, but putting all the code here gets ridiculous pretty fast
handle_call({checkUser,UserState}, _, State) -> checkUser:checkUser(UserState,State);
handle_call(getState, _From, State) -> {reply,State,State};
handle_call(debug, _From, State) ->
    #state{lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP,maps=Maps} = State,
    u:trace(dict:to_list(array:get(0,Maps))),
    u:trace(gb_trees:to_list(LBName)),
    u:trace(gb_trees:to_list(LBIP)),
    u:trace(dict:to_list(LBID)),
    {reply,ok,State};
handle_call(resetState, _From, _State) -> {reply,ok,#state{}};
handle_call(die, _From, State) -> {stop, normal, State};
handle_call(_Request, _From, State) ->
    u:trace("unknown gen_server:handle_call()",_Request),
    {reply, ok, State}.

%% handle_cast({say,Simple,Message}, State) when Message=/="" ->
%%    {noreply, say:say(Simple,Message,State)}
%% handle_cast({move,ID,User,X,Y},  State = #state{users=Users}) ->
%%     case dict:find(ID,Users) of
%%         {ok, Record} ->
%%             Last=Record#user.lastMessage,
%%             sendToAll(Users,User,["move @@@ ",User,"||",X,"||",Y]),
%%             {reply,ok,State#state{users=dict:store(User,Record#user{lastMessage=Last,lastAction=Last,x=X,y=Y},Users)}};
%%         _ -> {reply,State}
%%     end;
%% handle_cast({logout,User}, State=#state{users=Users}) ->
%%     sendToAll(Users,User,["logout @@@ ",User]),
%%     {reply,ok,State#state{users=dict:erase(User,Users)}};
%% handle_cast({allUsers,User},  State=#state{users=Users}) ->
%%     Gather =
%%         fun(Key,#user{x=X,y=Y},Acc)->
%%                 if User=/=Key -> [[",[\"",Key,"\",\"",X,"\",\"",Y,"\"]"]|Acc];
%%                    true -> Acc end                               
%%         end,
%%     case lists:flatten(dict:fold(Gather,[],Users)) of
%%         [_|Out] -> void;
%%         [] -> Out="[]"
%%     end,
%%     {reply,["[",Out,"]"],State};
handle_cast(_Msg, State) ->
    u:trace("gen_server:cast()",_Msg),
    {noreply, State}.
handle_info(_Info, State) ->
    u:trace("gen_server:handle_info()",_Info),
    {noreply, State}.
terminate(_Reason, #state{sock=Sock} = State) ->
    gen_tcp:close(Sock),
    u:trace("gen_server:terminate()",{_Reason,State}),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
