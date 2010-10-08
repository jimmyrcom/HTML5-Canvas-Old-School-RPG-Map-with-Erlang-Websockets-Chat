-module(es_websock).
-behaviour(gen_server).
-compile(export_all).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-record(state, 
        { users = dict:new()
          , sock
          , increment = 0
        }).
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
    case gen_tcp:listen(844, [binary, {packet, 0}, {active, true}, {reuseaddr, true}, {packet_size,1024}]) of
        {ok, S} -> 
            spawn(fun() -> connect:accept_connections(S) end),
            {ok,#state{sock=S}};
        Err -> 
            u:trace("Accept connections failed"),
            throw(Err)
    end.

stop() -> gen_server:call(?MODULE,die).
gs() -> gen_server:call(?MODULE,getState).
rs() -> gen_server:call(?MODULE,resetState).
say(User,Message) -> gen_server:call(?MODULE,{say,User,Message}).
move(User,X,Y) -> gen_server:call(?MODULE,{move,User,X,Y}).
checkUser(Simple,IP,Pid) -> gen_server:call(?MODULE,{checkUser,Simple,IP,Pid}).
allUsers(User) -> gen_server:call(?MODULE,{allUsers,User}).
logout(User) -> gen_server:call(?MODULE,{logout,User}).

sendToAll(Dict,You,Message) ->
    dict:map(fun(User,_) when User=:=You -> void;
                (_,Record) -> gen_tcp:send(Record#user.sock,[0,Message,255])
             end,Dict).

handle_call(getState, _From, State) -> {reply,State,State};
handle_call({say,User,Message}, _From, State = #state{users=Users}) when Message=/=[] ->
    case dict:find(User,Users) of
        {ok, Record} ->
            case (u:unixtime()-Record#user.lastMessage)>1 of 
                true -> sendToAll(Users,User,["say @@@ ",User,"||",Message]);
                false -> websockets:alert(Record#user.sock,["Error: Flooding, message not sent, wait ",integer_to_list(2-(u:unixtime()-Record#user.lastMessage))," more seconds to post again"])
            end,
            Unix=u:unixtime(),
            {reply,ok,State#state{users=dict:store(User,Record#user{lastMessage=Unix,lastAction=Unix},Users)}};
        _ ->
            {reply,fail,State}
        end;
handle_call({move,User,X,Y}, _From, State = #state{users=Users}) ->
    case dict:find(User,Users) of
        {ok, Record} ->
            Last=Record#user.lastMessage,
            sendToAll(Users,User,["move @@@ ",User,"||",X,"||",Y]),
            {reply,ok,State#state{users=dict:store(User,Record#user{lastMessage=Last,lastAction=Last,x=X,y=Y},Users)}};
        _ -> {reply,fail,State}
    end;
handle_call({checkUser,#simple{sock=Sock,user=User,x=X,y=Y},IP,Pid}, _From, State = #state{users=Users,increment=ID}) ->
    case dict:find(User,Users) of
        {ok,_} -> {reply,fail,State};
        error -> {reply,go,State#state{increment=ID+1,users=dict:store(User,#user{sock=Sock,id=ID,ip=IP,pid=Pid,x=X,y=Y},Users)}}
    end;
handle_call({logout,User}, _From, State=#state{users=Users}) ->
    sendToAll(Users,User,["logout @@@ ",User]),
    {reply,ok,State#state{users=dict:erase(User,Users)}};
handle_call({allUsers,User}, _From, State=#state{users=Users}) ->
    Gather =
        fun(Key,#user{x=X,y=Y},Acc)->
                if User=/=Key -> [[",[\"",Key,"\",\"",X,"\",\"",Y,"\"]"]|Acc];
                   true -> Acc end                               
        end,
    case lists:flatten(dict:fold(Gather,[],Users)) of
        [_|Out] -> void;
        [] -> Out="[]"
    end,
    {reply,["[",Out,"]"],State};
handle_call(resetState, _From, _State) -> {reply,ok,#state{}};
handle_call(die, _From, State) -> {stop, normal, State};
handle_call(_Request, _From, State) ->
    u:trace("unknown gen_server:handle_call()",_Request),
    {reply, ok, State}.
handle_cast(_Msg, State) ->
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
